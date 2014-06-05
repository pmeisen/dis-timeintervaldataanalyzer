package net.meisen.dissertation.server;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.SocketException;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.general.genmisc.types.Streams;
import net.meisen.general.server.api.impl.BaseListener;
import net.meisen.general.server.listener.utility.WorkerThread;
import net.meisen.general.server.settings.pojos.Connector;
import net.meisen.general.server.settings.pojos.Extension;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class TidaServerListener extends BaseListener {
	private final int DEFAULT_TIMEOUTINMS = 30 * 60 * 1000;

	/**
	 * The name under which the listener is registered
	 */
	public static final String NAME = "TSQL";

	private final static Logger LOG = LoggerFactory
			.getLogger(TidaServerListener.class);

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private QueryFactory queryFactory;

	private int timeoutInMs = -1;

	@Override
	public void initialize(final Connector c) {
		super.initialize(c);

		final Extension client = c.getExtension("client");
		if (client != null) {
			final int timeoutInS = client.getProperty("timeout");
			timeoutInMs = Math.max(0, timeoutInS * 1000);
		} else {
			timeoutInMs = DEFAULT_TIMEOUTINMS;
		}
	}

	@Override
	protected Thread createWorkerThread(final Socket socket) {
		try {
			socket.setSoTimeout(timeoutInMs);
		} catch (final SocketException e) {
			// getExceptionRegistry()
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return new WorkerThread(socket) {

			@Override
			public void run() {
				try {
					handleRequests();
				} catch (final Exception e) {
					// TODO log
					System.out.println("TODO ERROR: " + e.getMessage());
				} finally {
					close();
				}
			}

			public void handleRequests() throws IOException {

				// create a input- and outputStream
				final DataInputStream is = new DataInputStream(
						new BufferedInputStream(getSocket().getInputStream()));
				final DataOutputStream os = new DataOutputStream(
						new BufferedOutputStream(getSocket().getOutputStream()));

				try {
					while (true) {

						// get the real message that was send
						final String msg = readMsg(is);
						if (LOG.isDebugEnabled()) {
							LOG.debug("Retrieved query '" + msg + "'.");
						}

						// let's use the query
						try {
							final IQuery query = queryFactory.parseQuery(msg);
							final IQueryResult result = queryFactory
									.evaluateQuery(query, null);

							// write the result
							writeResult(os, result);
						} catch (final Exception e) {
							writeException(os, e);
						}
					}
				} catch (final EOFException e) {
					// indicates that the stream was just closed
				} finally {

					// make sure the DataInputStream is closed now
					Streams.closeIO(is);
					Streams.closeIO(os);
				}
			}

			protected void writeResult(final DataOutputStream os,
					final IQueryResult result) throws IOException {
				writeMsg(os, result.toString());
			}

			protected void writeException(final DataOutputStream os,
					final Exception exception) {
				if (exception == null) {
					return;
				}

				final String msg = exception.getMessage();
				if (LOG.isErrorEnabled()) {
					LOG.error("Request handling failed: " + msg, exception);
				}

				// send the
				try {
					writeMsg(os, msg);
				} catch (final IOException e) {
					if (LOG.isErrorEnabled()) {
						LOG.error(
								"Unable to send the exception '"
										+ exception.getMessage() + "'.", e);
					}
				}
			}

			protected String readMsg(final DataInputStream is)
					throws IOException {
				final int size = is.readInt();
				final byte[] msgAsBytes = new byte[size];
				is.read(msgAsBytes);

				return new String(msgAsBytes, "UTF8");
			}

			protected void writeMsg(final DataOutputStream os, final String msg)
					throws IOException {

				final byte[] msgAsBytes = msg.getBytes("UTF8");
				os.writeInt(msgAsBytes.length);
				os.write(msgAsBytes);
				os.flush();
			}

			@Override
			public void close() {
				if (LOG.isDebugEnabled()) {
					LOG.debug("Closing socket on '"
							+ TidaServerListener.this.toString() + "'...");
				}

				super.close();
			}
		};
	}

	@Override
	public String toString() {
		return NAME + (getPort() == -1 ? "" : " (" + getPort() + ")");
	}
}
