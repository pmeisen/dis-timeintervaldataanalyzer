package net.meisen.dissertation.server;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketException;

import net.meisen.dissertation.jdbc.protocol.Protocol;
import net.meisen.dissertation.jdbc.protocol.Protocol.IResponseHandler;

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
		server = TidaServer.create();
		server.startAsync();

		// wait for the server to start
		while (!server.isRunning()) {
			Thread.sleep(50);
		}

		// directly create a socket
		socket = new Socket();
		socket.connect(new InetSocketAddress("localhost", 7001), 1000);
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

		// create a handler for testing
		final IResponseHandler handler = new IResponseHandler() {

			@Override
			public InputStream getResourceStream(final String resource) {
				return getClass().getResourceAsStream(resource);
			}

			@Override
			public void handleResult(final byte[] result) {
				try {
					System.out.println(new String(result, "UTF8"));
				} catch (final UnsupportedEncodingException e) {
					// ignore
				}
			}
		};

		// load a model
		p.write("LOAD FROM '/net/meisen/dissertation/impl/parser/query/testPersonModel.xml'");
		p.handleResponse(handler);

		for (int i = 0; i < 10; i++) {
			p.write("select timeseries of count(PERSON) AS PERSON from testPersonModel");
			p.handleResponse(handler);
		}

		p.write("unload testPersonModel");
		p.handleResponse(handler);

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
		final Protocol p = new Protocol(socket);

		// send a message everything should just work fine
		p.write("ALIVE");
		p.read();

		// shut the server down
		server.shutdown();

		// the socket should be closed
		Exception exception = null;
		try {
			p.write("ALIVE");
			p.read();
		} catch (final SocketException e) {
			exception = e;
		}
		assertNotNull(exception);
		assertTrue(exception.getMessage().contains("caused connection abort"));
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
