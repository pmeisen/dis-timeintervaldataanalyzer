package net.meisen.dissertation.server;

import java.io.EOFException;
import java.io.IOException;
import java.net.Socket;
import java.net.SocketException;

import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.jdbc.protocol.DataType;
import net.meisen.dissertation.jdbc.protocol.Protocol;
import net.meisen.dissertation.jdbc.protocol.QueryStatus;
import net.meisen.dissertation.jdbc.protocol.WrappedException;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.parser.query.IQueryResultSet;
import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;
import net.meisen.general.genmisc.types.Streams;
import net.meisen.general.server.listener.utility.WorkerThread;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A thread used to handle requests to the server.
 * 
 * @author pmeisen
 * 
 */
public class RequestHandlerThread extends WorkerThread {
	private final static Logger LOG = LoggerFactory
			.getLogger(RequestHandlerThread.class);

	private QueryFactory queryFactory;
	private Exception lastException = null;

	/**
	 * A thread to handle requests on server side.
	 * 
	 * @param input
	 *            the {@code Socket} to retrieve the requests on
	 * @param queryFactory
	 *            the factory used to interprete a request
	 */
	public RequestHandlerThread(final Socket input,
			final QueryFactory queryFactory) {
		super(input);

		this.queryFactory = queryFactory;
	}

	@Override
	public void run() {
		try {
			handleRequests();
		} catch (final SocketException e) {
			// ignore any kind of socket exception, it is closed
			if (LOG.isTraceEnabled()) {
				LOG.trace("Exception thrown while handling a request.", e);
			}
		} catch (final Exception e) {
			lastException = e;
		} finally {
			close();
		}
	}

	/**
	 * Handles a request by reading a string using the current {@code Protocol}.
	 * 
	 * @throws IOException
	 *             if the requests handling fails
	 */
	public void handleRequests() throws IOException {
		final Protocol p = new Protocol(getSocket());

		try {
			while (true) {
				final String msg;
				try {
					// get the real message that was send
					msg = p.readMessage();
					if (LOG.isDebugEnabled()) {
						LOG.debug("Retrieved query '" + msg + "'.");
					}
				} catch (final WrappedException e) {

					/*
					 * The client send an exception, we can just log it here.
					 * All other exception (not send just occuring) are serious
					 * enough to close the connection completely.
					 */
					if (LOG.isErrorEnabled()) {
						LOG.error("Client send exception '" + e.getMessage()
								+ "'", e);
					}

					/*
					 * Read the next one, the client will close the connection
					 * if the error is serious.
					 */
					continue;
				}

				try {

					// parse the query and send the type
					final IQuery query = queryFactory.parseQuery(msg);
					if (LOG.isTraceEnabled()) {
						LOG.trace("Writing queryType of '" + msg + "' as '"
								+ query.getQueryType() + "'.");
					}
					p.writeQueryType(query.getQueryType());

					// check the status
					final QueryStatus status = p.readQueryStatus();
					if (LOG.isTraceEnabled()) {
						LOG.trace("Got queryStatus '" + status
								+ "' from client for query '" + msg + "'.");
					}
					if (!QueryStatus.CANCEL.equals(status)) {

						// enable identifier collection if needed
						final boolean enableIdCollection = QueryStatus.PROCESSANDGETIDS
								.equals(status);
						query.enableIdCollection(enableIdCollection);

						// retrieve the result
						final IQueryResult result = queryFactory.evaluateQuery(
								query, new ClientResourceResolver(p));

						// write the collected identifiers if it was activated
						if (enableIdCollection) {
							if (LOG.isTraceEnabled()) {
								LOG.trace("Writing the collected identifiers.");
							}

							final int[] collectedIds = result.getCollectedIds();
							if (collectedIds == null) {
								p.writeInts(collectedIds);
							} else {
								p.writeInts(new int[0]);
							}
						}

						// write the result
						if (result instanceof IQueryResultSingleInteger) {
							final IQueryResultSingleInteger res = (IQueryResultSingleInteger) result;

							if (LOG.isTraceEnabled()) {
								LOG.trace("Replying to query '" + msg
										+ "' with single integer "
										+ res.getResult() + ".");
							}

							p.writeInt(res.getResult());
						} else if (result instanceof IQueryResultSet) {
							final IQueryResultSet res = (IQueryResultSet) result;

							// write the header
							if (LOG.isTraceEnabled()) {
								LOG.trace("Writing headers of reply of query '"
										+ msg + "'.");
							}
							final DataType[] header = p.writeHeader(res
									.getTypes());
							p.writeHeaderNames(res.getNames());

							// write the records
							if (LOG.isTraceEnabled()) {
								LOG.trace("Writing records of reply of query '"
										+ msg + "'.");
							}
							for (final Object[] values : res) {
								p.writeResult(header, values);
							}
						}
					}

					if (LOG.isTraceEnabled()) {
						LOG.trace("Answer of '" + msg + "' sent, sending eor.");
					}

					p.writeEndOfResult();
				} catch (final Exception e) {
					if (LOG.isTraceEnabled()) {
						LOG.trace("Exception while handling '" + msg
								+ "' sending '" + e.getMessage() + "'.");
					}

					p.writeException(e);
				}
			}
		} catch (final EOFException e) {
			// indicates that the stream was just closed
		} finally {

			// make sure the DataInputStream is closed now
			Streams.closeIO(p);
		}
	}

	/**
	 * Get the last exception thrown.
	 * 
	 * @return the last exception thrown, {@code null} if none was thrown
	 */
	public Exception getLastException() {
		return lastException;
	}

	@Override
	public void close() {
		if (getSocket().isClosed()) {
			return;
		}

		// log the closing and do it
		if (LOG.isDebugEnabled()) {
			LOG.debug("Closing socket used for request handling...");
		}
		super.close();
	}
}
