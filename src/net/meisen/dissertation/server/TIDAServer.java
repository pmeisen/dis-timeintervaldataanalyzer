package net.meisen.dissertation.server;

import net.meisen.dissertation.config.TIDAConfig;
import net.meisen.general.server.Server;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TIDAServer {
	private final static Logger LOG = LoggerFactory.getLogger(TIDAServer.class);
	
	private Server server;

	public TIDAServer() {
		server = Server.createServer("sbconfigurator-core.xml",
				TIDAConfig.class);
	}
	
	public static void main(final String[] args) {
		final Server server = Server.createServer("sbconfigurator-core.xml",
				TIDAConfig.class);

		server.start();
	}
}
