package net.meisen.dissertation.server;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.Date;

import net.meisen.dissertation.help.Performance;
import net.meisen.dissertation.server.protocol.Communication;
import net.meisen.dissertation.server.protocol.Communication.IResponseHandler;
import net.meisen.dissertation.server.protocol.Communication.ResponseType;
import net.meisen.dissertation.server.protocol.Communication.RetrievedValue;
import net.meisen.general.genmisc.types.Files;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TestTidaServer {
	private TidaServer server;
	private Socket socket;

	@Before
	public void startServer() throws InterruptedException, IOException {
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

	@Test
	public void testCommunication() throws IOException {
		final Communication c = new Communication(socket);

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
		c.write("LOAD FROM '/net/meisen/dissertation/impl/parser/query/testPersonModel.xml'");
		c.handleResponse(handler);

		Performance p = new Performance();

		for (int i = 0; i < 10; i++) {
			p.start();
			c.write("select timeseries of count(PERSON) AS PERSON from testPersonModel");
			c.handleResponse(handler);
			System.out.println(p.printSecs(p.stop()));
		}

		c.write("unload testPersonModel");
		c.handleResponse(handler);

		// close the socket
		c.close();
	}

	@After
	public void shutdownServer() throws IOException {
		socket.close();
		server.shutdown();

		// make sure the used model is cleaned
		assertTrue(Files.deleteDir(new File(".", "testPersonModel")));
	}
}
