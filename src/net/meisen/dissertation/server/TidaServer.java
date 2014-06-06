package net.meisen.dissertation.server;

import java.io.File;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.ConfigurationCoreSettings;
import net.meisen.general.server.Server;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * The server implementation.
 * 
 * @author pmeisen
 * 
 */
public class TidaServer {
	private final static Logger LOG = LoggerFactory.getLogger(TidaServer.class);

	@Autowired
	@Qualifier("server")
	private Server server;

	@Autowired
	@Qualifier(DefaultValues.HANDLER_ID)
	private TidaModelHandler handler;

	/**
	 * Starts the server asynchronously.
	 * 
	 * @see Server#startAsync()
	 */
	public void startAsync() {
		server.startAsync();
	}

	/**
	 * Starts the server within the current thread.
	 * 
	 * @see Server#start()
	 */
	public void start() {
		server.start();
	}

	/**
	 * Checks if the server is running.
	 * 
	 * @return {@code true} if the server is running, otherwise {@code false}
	 */
	public boolean isRunning() {
		return server.isRunning();
	}

	/**
	 * Shuts the server down and keeps all the needed files to reboot the server
	 * with the stated defined on shutdown.
	 */
	public void shutdown() {
		shutdown(false);
	}

	/**
	 * Shuts the server down and depending on the {@code cleanUp} defined, the
	 * server will delete all created files, i.e. {@code true}, or not, i.e.
	 * {@code false}.
	 * 
	 * @param cleanUp
	 *            {@code true} to delete all the files, otherwise {@code false}
	 */
	public void shutdown(final boolean cleanUp) {
		server.shutdown();

		if (cleanUp) {
			if (!Files.deleteDir(new File(handler.getDefaultLocation()))) {
				if (LOG.isWarnEnabled()) {
					LOG.warn("CleanUp of server failed.");
				}
			}
		}
	}

	/**
	 * Creates an instance of a {@code TidaServer}, which is completely
	 * auto-wired according to the configuration.
	 * 
	 * @return the created {@code TidaServer} instance
	 */
	public static TidaServer create() {
		final ConfigurationCoreSettings settings = ConfigurationCoreSettings
				.loadCoreSettings("sbconfigurator-core.xml", TidaConfig.class);
		return settings.getConfiguration().getModule("tidaServer");
	}

	/**
	 * Main method which starts the server in the main-thread.
	 * 
	 * @param args
	 *            additional arguments for the start
	 */
	public static void main(final String[] args) {
		final TidaServer server = create();
		server.start();
	}
}
