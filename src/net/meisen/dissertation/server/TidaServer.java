package net.meisen.dissertation.server;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Properties;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.general.genmisc.collections.Collections;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.ConfigurationCoreSettings;
import net.meisen.general.server.Server;
import net.meisen.general.server.settings.pojos.Connector;

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
		if (LOG.isInfoEnabled()) {
			final List<String> s = new ArrayList<String>();
			for (final Connector connector : server.getServerSettings()
					.getConnectorSettings()) {
				if (connector.isEnable()) {
					s.add(connector.getListener() + " (" + connector.getPort()
							+ ")");
				}
			}

			LOG.info("Server started with: " + Collections.concate(", ", s));
		}

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

	public static Properties getDefaultProperties() {
		final Properties properties = new Properties();

		properties.setProperty("tida.config.selector", "tidaConfig.xml");

		properties.setProperty("tida.server.http.port", "7000");
		properties.setProperty("tida.server.http.enabled", "true");

		properties.setProperty("tida.server.tsql.port", "7001");
		properties.setProperty("tida.server.tsql.enabled", "true");
		properties.setProperty("tida.server.tsql.timeout", "1800000");

		return properties;
	}

	/**
	 * Creates an instance of a {@code TidaServer}, which is completely
	 * auto-wired according to the configuration.
	 * 
	 * @return the created {@code TidaServer} instance
	 */
	public static TidaServer create() {
		return create(null);
	}

	/**
	 * Creates an instance of a {@code TidaServer}, which is completely
	 * auto-wired according to the configuration.
	 * 
	 * @param props
	 *            the properties to be used for the server, can be {@code null}
	 *            if the default properties should be used
	 * 
	 * @return the created {@code TidaServer} instance
	 * 
	 * @see #getDefaultProperties()
	 */
	public static TidaServer create(final Properties props) {

		// start with the default properties
		final Properties properties = getDefaultProperties();

		// override the properties
		if (props != null) {
			for (final Entry<Object, Object> property : props.entrySet()) {
				properties.put(property.getKey(), property.getValue());
			}
		}

		// set the values as system properties
		for (final Entry<Object, Object> property : properties.entrySet()) {
			final String key = (String) property.getKey();
			final String val = (String) property.getValue();

			System.setProperty(key, val);
		}

		// create the instance
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
