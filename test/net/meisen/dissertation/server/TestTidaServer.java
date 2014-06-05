package net.meisen.dissertation.server;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;

import net.meisen.general.genmisc.types.Streams;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class TestTidaServer {
	private TidaServer server;

	@Before
	public void startServer() throws InterruptedException {
		server = TidaServer.create();
		server.startAsync();

		// wait for the server to start
		while (!server.isRunning()) {
			Thread.sleep(50);
		}
	}

	@Test
	public void testConnection() throws IOException, InterruptedException {
		Socket socket = new Socket();
		socket.connect(new InetSocketAddress("localhost", 7001), 1000);
//		socket.setSoTimeout(1000);

		byte[] msgAsBytes;
		
		// create a input- and outputStream
		final DataInputStream is = new DataInputStream(
				new BufferedInputStream(socket.getInputStream()));
		final DataOutputStream os = new DataOutputStream(
				new BufferedOutputStream(socket.getOutputStream()));

		msgAsBytes = "SELECT".getBytes("UTF8");
		os.writeInt(msgAsBytes.length);
		os.write(msgAsBytes);
		os.flush();
		
		msgAsBytes = new byte[is.readInt()];
		is.read(msgAsBytes);
		System.out.println("echo: " + new String(msgAsBytes, "UTF8"));
				
		msgAsBytes = "SELECT".getBytes("UTF8");
		os.writeInt(msgAsBytes.length);
		os.write(msgAsBytes);
		os.flush();
		
		msgAsBytes = new byte[is.readInt()];
		is.read(msgAsBytes);
		System.out.println("echo: " + new String(msgAsBytes, "UTF8"));
		
		Streams.closeIO(os);
		Streams.closeIO(is);
		
		// close the socket
		socket.close();
		
		System.out.println("DONE");
	}

	@After
	public void shutdownServer() {
		server.shutdown();
	}
}
