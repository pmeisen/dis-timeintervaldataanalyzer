package net.meisen.dissertation.server;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.general.sbconfigurator.ConfigurationCoreSettings;
import net.meisen.general.server.Server;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class TidaServer {
	private final static Logger LOG = LoggerFactory.getLogger(TidaServer.class);

	@Autowired
	@Qualifier("server")
	private Server server;

	public void startAsync() {
		server.startAsync();
	}

	public void start() {
		server.start();
	}

	public boolean isRunning() {
		return server.isRunning();
	}

	public void shutdown() {
		server.shutdown();
	}

	public static TidaServer create() {
		final ConfigurationCoreSettings settings = ConfigurationCoreSettings
				.loadCoreSettings("sbconfigurator-core.xml", TidaConfig.class);
		return settings.getConfiguration().getModule("tidaServer");
	}

	public static void main(final String[] args) {
		final TidaServer server = create();

		server.start();
	}
}
