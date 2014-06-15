package net.meisen.dissertation.server;

import java.io.EOFException;
import java.io.IOException;
import java.net.Socket;
import java.net.SocketException;

import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.jdbc.protocol.Protocol;
import net.meisen.dissertation.jdbc.protocol.Protocol.WrappedException;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.parser.query.IQueryResultArrayOfIntegers;
import net.meisen.dissertation.model.parser.query.IQueryResultSingleInteger;
import net.meisen.dissertation.model.parser.query.IQueryResultSet;
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
					final IQuery query = queryFactory.parseQuery(msg);
					final IQueryResult result = queryFactory.evaluateQuery(
							query, new ClientResourceResolver(p));

					if (result instanceof IQueryResultSingleInteger) {
						final IQueryResultSingleInteger resultInt = (IQueryResultSingleInteger) result;
						p.writeInt(resultInt.getResult());
					} else if (result instanceof IQueryResultArrayOfIntegers) {

						// TODO write all the integers
					} else if (result instanceof IQueryResultSet) {
						final IQueryResultSet resultSet = (IQueryResultSet) result;
						p.writeHeader(resultSet.getTypes());
						p.writeHeaderNames(resultSet.getNames());

						// TODO iterate over the data and write it
					} else {

					}

					// write the result
					p.writeResult(result.toString().getBytes());
					p.writeEndOfResult();
				} catch (final Exception e) {
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
