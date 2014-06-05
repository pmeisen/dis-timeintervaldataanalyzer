package net.meisen.dissertation.server;

import java.io.ByteArrayInputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;

import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.parser.query.IResourceResolver;
import net.meisen.dissertation.server.protocol.Communication;
import net.meisen.dissertation.server.protocol.Communication.WrappedException;
import net.meisen.general.genmisc.types.Streams;
import net.meisen.general.server.listener.utility.WorkerThread;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RequestHandlerThread extends WorkerThread {
	private final static Logger LOG = LoggerFactory
			.getLogger(RequestHandlerThread.class);

	private QueryFactory queryFactory;

	public RequestHandlerThread(final Socket input,
			final QueryFactory queryFactory) {
		super(input);

		this.queryFactory = queryFactory;
	}

	@Override
	public void run() {
		try {
			handleRequests();
		} catch (final Exception e) {
			// TODO log
			System.out.println("TODO ERROR: " + e.getMessage());
			e.printStackTrace();
		} finally {
			close();
		}
	}

	public void handleRequests() throws IOException {
		final Communication c = new Communication(getSocket());

		try {
			while (true) {
				final String msg;
				try {
					// get the real message that was send
					msg = c.readString();
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
							query, new ClientResourceResolver(c));

					// write the result
					c.writeResult(result.toString().getBytes());
					c.writeEndOfResult();
				} catch (final Exception e) {
					c.writeException(e);
				}
			}
		} catch (final EOFException e) {
			// indicates that the stream was just closed
		} finally {

			// make sure the DataInputStream is closed now
			Streams.closeIO(c);
		}
	}

	@Override
	public void close() {
		if (LOG.isDebugEnabled()) {
			LOG.debug("Closing socket used for request handling...");
		}

		super.close();
	}
}
