package net.meisen.dissertation.server;

import java.io.EOFException;
import java.io.IOException;
import java.net.Socket;
import java.net.SocketException;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.jdbc.protocol.DataType;
import net.meisen.dissertation.jdbc.protocol.Protocol;
import net.meisen.dissertation.jdbc.protocol.QueryStatus;
import net.meisen.dissertation.jdbc.protocol.WrappedException;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.parser.query.IQueryResultSet;
import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
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

	private final IExceptionRegistry exceptionRegistry;
	private final QueryFactory queryFactory;

	private Exception lastException = null;

	/**
	 * A thread to handle requests on server side.
	 * 
	 * @param input
	 *            the {@code Socket} to retrieve the requests on
	 * @param queryFactory
	 *            the factory used to interprete a request
	 * @param exceptionRegistry
	 */
	public RequestHandlerThread(final Socket input,
			final QueryFactory queryFactory,
			final IExceptionRegistry exceptionRegistry) {
		super(input);

		this.queryFactory = queryFactory;
		this.exceptionRegistry = exceptionRegistry;
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

			if (LOG.isErrorEnabled()) {
				LOG.error("Exception thrown while handling a request.", e);
			}
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
			while (!Thread.interrupted()) {

				final String msg;
				try {

					// get the real message that was send
					msg = p.waitForMessage();
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

					// cancel if wished for
					if (QueryStatus.CANCEL.equals(status)) {
						p.writeEndOfResponse();
						continue;
					}

					// enable identifier collection if needed
					final boolean enableIdCollection = QueryStatus.PROCESSANDGETIDS
							.equals(status);
					query.enableIdCollection(enableIdCollection);

					// retrieve the result
					try {
						final IQueryResult res = queryFactory.evaluateQuery(
								query, new ClientResourceResolver(p));

						// write the identifiers of not canceled
						if (checkCancellation(p)) {
							continue;
						} else if (enableIdCollection
								&& writeIdentifiers(p, res.getCollectedIds())) {
							continue;
						}

						/*
						 * All the methods return true if the process was
						 * cancelled, therefore check the pre-requirements and
						 * the result of execution.
						 */
						if (checkCancellation(p)) {
							continue;
						} else if (res instanceof IQueryResultSingleInteger
								&& writeSingleInteger(p, msg,
										(IQueryResultSingleInteger) res)) {
							continue;
						} else if (res instanceof IQueryResultSet
								&& writeSet(p, msg, (IQueryResultSet) res)) {
							continue;
						}
					} catch (final CancellationException e) {
						if (LOG.isTraceEnabled()) {
							LOG.trace("Handling of '" + msg
									+ "' was canceled during evaluation.");
						}
						p.writeEndOfResponse();
						continue;
					}

					if (LOG.isTraceEnabled()) {
						LOG.trace("Answer of '" + msg + "' sent, sending eor.");
					}
					p.writeEndOfResponse();
				} catch (final SocketException e) {
					if (LOG.isTraceEnabled()) {
						LOG.trace("Exception while handling '" + msg
								+ "' sending '" + e.getMessage() + "'.");
					}
				} catch (final Exception e) {
					if (LOG.isErrorEnabled()) {
						LOG.error("Exception while handling '" + msg
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
	 * Writes a set of results according to the specified {@code Protocol}.
	 * 
	 * @param p
	 *            the {@code Protocol} used to write the results.
	 * @param msg
	 *            the message the integer was written for
	 * @param result
	 *            the result to be written
	 * 
	 * @return {@code true} if the writing was canceled, otherwise {@code false}
	 * 
	 * @throws IOException
	 *             if an exception is thrown during the writing
	 */
	protected boolean writeSet(final Protocol p, final String msg,
			final IQueryResultSet result) throws IOException {
		boolean canceled = false;

		final IQueryResultSet res = (IQueryResultSet) result;

		// write the header
		if (LOG.isTraceEnabled()) {
			LOG.trace("Writing headers of reply of query '" + msg + "'.");
		}
		final DataType[] header = p.writeHeader(res.getTypes());
		p.writeHeaderNames(res.getNames());

		// send an end of meta, so that the client knows that data will follow
		p.writeEndOfMeta();

		// write the records
		if (LOG.isTraceEnabled()) {
			LOG.trace("Writing records of reply of query '" + msg + "'.");
		}
		for (final Object[] values : res) {
			if (!(canceled = canceled || checkCancellation(p))) {
				p.writeResult(header, values);
			} else {
				break;
			}
		}

		return canceled;
	}

	/**
	 * Writes a single integer according to the passed {@code Protocol}.
	 * 
	 * @param p
	 *            the {@code Protocol} used to write the single integer.
	 * @param msg
	 *            the message the integer was written for
	 * @param result
	 *            the result to be written
	 * 
	 * @return {@code true} if the writing was canceled, otherwise {@code false}
	 * 
	 * @throws IOException
	 *             if an exception is thrown during the writing
	 */
	protected boolean writeSingleInteger(final Protocol p, final String msg,
			final IQueryResultSingleInteger result) throws IOException {

		if (LOG.isTraceEnabled()) {
			LOG.trace("Replying to query '" + msg + "' with single integer "
					+ result.getResult() + ".");
		}

		p.writeInt(result.getResult());

		return false;
	}

	/**
	 * Writes the specified identifiers according to the passed {@code Protocol}
	 * .
	 * 
	 * @param p
	 *            the {@code Protocol} used to write the single integer.
	 * @param collectedIds
	 *            the identifiers to be written
	 * 
	 * 
	 * @return {@code true} if the writing was canceled, otherwise {@code false}
	 * 
	 * @throws IOException
	 *             if an exception is thrown during the writing
	 */
	protected boolean writeIdentifiers(final Protocol p,
			final int[] collectedIds) throws IOException {
		if (LOG.isTraceEnabled()) {
			LOG.trace("Writing the collected identifiers.");
		}

		if (collectedIds == null) {
			p.writeInts(collectedIds);
		} else {
			p.writeInts(new int[0]);
		}

		return false;
	}

	protected boolean checkCancellation(final Protocol p) throws IOException {
		final String[] msg = new String[] { null };
		final Boolean peek = p.peekForCancel(msg);
		if (peek == null) {
			return false;
		} else if (peek) {
			if (LOG.isTraceEnabled()) {
				LOG.trace("Handling of '" + msg + "' canceled.");
			}
			p.writeEndOfResponse();
			return true;
		} else {
			exceptionRegistry.throwRuntimeException(
					QueryEvaluationException.class, 1017, msg[0]);
			return false;
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
