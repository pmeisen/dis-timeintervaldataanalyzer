package net.meisen.dissertation.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketException;
import java.util.Properties;

import net.meisen.dissertation.jdbc.QueryResponseHandler;
import net.meisen.dissertation.jdbc.TidaResultSetType;
import net.meisen.dissertation.jdbc.protocol.IResponseHandler;
import net.meisen.dissertation.jdbc.protocol.Protocol;
import net.meisen.dissertation.jdbc.protocol.ResponseType;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests the implementation of the {@code TidaServer} and it's listeners and
 * handlers.
 * 
 * @author pmeisen
 * 
 */
public class TestTidaServer {
	private final int serverPort = 6666;

	private TidaServer server;
	private Socket socket;

	/**
	 * Start a server for testing purposes
	 * 
	 * @throws Exception
	 *             if the server could not be started
	 */
	@Before
	public void startServer() throws Exception {

		final Properties properties = new Properties();
		properties.setProperty("tida.server.tsql.port", "" + serverPort);

		server = TidaServer.create(properties);
		server.startAsync();

		// directly create a socket
		socket = new Socket();
		socket.connect(new InetSocketAddress("localhost", serverPort), 1000);
	}

	/**
	 * Tests the implemented {@code Protocol}.
	 * 
	 * @throws IOException
	 *             if the socket cannot be handled
	 */
	@Test
	public void testProtocol() throws IOException {
		final Protocol p = new Protocol(socket);
		p.writeCredential("", "");

		// create a handler for testing
		final QueryResponseHandler handler = new QueryResponseHandler();
		handler.setExpectedResultSetType(TidaResultSetType.MODIFY);

		// load a model
		p.writeAndHandle(
				"LOAD FROM 'classpath:/net/meisen/dissertation/impl/parser/query/testPersonModel.xml'",
				handler);

		handler.setExpectedResultSetType(TidaResultSetType.QUERY);
		for (int i = 0; i < 10; i++) {
			p.initializeCommunication(
					"select timeseries of count(PERSON) AS PERSON from testPersonModel",
					handler);

			// handle data up to the meta-data
			p.handleResponse(handler);

			int counter = 0;
			while (!p.handleResponse(handler)) {
				counter++;
			}

			// we expect one answer per query
			assertEquals(1, counter);
		}

		handler.setExpectedResultSetType(TidaResultSetType.MODIFY);
		p.writeAndHandle("unload testPersonModel", handler);
		assertTrue(handler.reachedEOR());

		// close the socket
		p.close();
	}

	/**
	 * Tests that an open {@code Socket} is closed, after the server is shut
	 * down.
	 * 
	 * @throws IOException
	 *             if an unexpected exception while writing or reading is thrown
	 */
	@Test
	public void testCloseSocketAfterShutdown() throws IOException {
		final IResponseHandler handler = new QueryResponseHandler();
		final Protocol p = new Protocol(socket);
		p.writeCredential("", "");

		// send a message everything should just work fine
		p.writeAndHandle("ALIVE", handler);

		// shut the server down
		server.shutdown();

		// the socket should be closed
		Exception exception = null;
		try {
			p.writeAndHandle("ALIVE", handler);
		} catch (final SocketException e) {
			exception = e;
		}
		assertNotNull(exception);
		assertTrue(
				exception.getMessage(),
				exception.getMessage().contains("Connection reset by peer")
						|| exception.getMessage().contains(
								"caused connection abort"));

		// close the socket
		p.close();
	}

	/**
	 * Tests the cancellation of a statement on client side.
	 * 
	 * @throws IOException
	 *             if some error occurs
	 */
	@Test
	public void testCancellation() throws IOException {
		final boolean[] control = new boolean[] { false };
		final QueryResponseHandler handler = new QueryResponseHandler() {

			@Override
			public boolean handleResult(final ResponseType type,
					final Object[] value) {
				super.handleResult(type, value);

				return control[0];
			}
		};
		final Protocol p = new Protocol(socket);
		p.writeCredential("", "");

		// send a message which might take a while
		p.initializeCommunication(
				"LOAD FROM 'classpath:/net/meisen/dissertation/impl/parser/query/testPersonModel.xml'",
				handler);

		// interrupt the current thread
		Thread.currentThread().interrupt();

		// start the handling
		while (!handler.isEOR()) {
			p.handleResponse(handler);
		}

		// check that the flag was removed
		assertFalse(Thread.currentThread().isInterrupted());
		control[0] = true;

		// use the protocol again, it should work just fine
		p.writeAndHandle(
				"LOAD FROM 'classpath:/net/meisen/dissertation/impl/parser/query/testPersonModel.xml'",
				handler);

		// close the socket
		p.close();
	}

	/**
	 * Shutdowns the server.
	 * 
	 * @throws IOException
	 *             if the shutdown cannot be achieved
	 */
	@After
	public void shutdownServer() throws IOException {
		socket.close();
		server.shutdown(true);
	}
}
