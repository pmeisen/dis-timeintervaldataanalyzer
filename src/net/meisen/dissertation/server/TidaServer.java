package net.meisen.dissertation.server;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.server.messages.ShutdownMessage;
import net.meisen.general.genmisc.collections.Collections;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.ConfigurationCoreSettings;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.config.placeholder.SpringPropertyHolder;
import net.meisen.general.server.Server;
import net.meisen.general.server.api.IControlMessagesManager;
import net.meisen.general.server.control.DefaultControlMessagesManager;
import net.meisen.general.server.settings.pojos.Connector;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.PropertyPlaceholderConfigurer;
import org.springframework.core.io.support.PropertiesLoaderSupport;

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
	@Qualifier("controlMessagesManager")
	private IControlMessagesManager serverControlMessagesManager;

	@Autowired
	@Qualifier(DefaultValues.HANDLER_ID)
	private TidaModelHandler handler;

	@Autowired
	@Qualifier(IConfiguration.coreConfigurationId)
	private IConfiguration configuration;

	/**
	 * Registers some control instances, i.e. a shutdown-hook and a control
	 * message to shutdown the server.
	 */
	protected void registerControls() {

		// override the shutdown message of the server with one of the
		// tidaServer
		serverControlMessagesManager.addControlMessage(ShutdownMessage.class,
				true);

		Runtime.getRuntime().addShutdownHook(new Thread() {

			@Override
			public void run() {

				// check if the server is running and shut it down if so
				if (TidaServer.this.isRunning()) {
					if (LOG.isInfoEnabled()) {
						LOG.info("The server will be shut down because of a ShutdownHook.");
					}

					TidaServer.this.shutdown();
				}
			}
		});
	}

	/**
	 * Checks if the server is currently running.
	 * 
	 * @return {@code true} if it's running, otherwise {@code false}
	 */
	public boolean isRunning() {
		return server.isRunning();
	}

	/**
	 * Starts the server asynchronously. The methods holds until the server is
	 * started.
	 * 
	 * @see Server#startAsync()
	 */
	public void startAsync() {
		registerControls();

		server.startAsync(false);
		server.waitForStart();

		if (LOG.isInfoEnabled()) {
			final List<String> s = new ArrayList<String>();
			for (final Connector connector : server.getServerSettings()
					.getConnectorSettings()) {
				if (connector.isEnable()) {
					s.add(connector.getListener() + " (" + connector.getPort()
							+ ")");
				}
			}

			LOG.info("Server started asynchroniously with: "
					+ Collections.concate(", ", s));
		}
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

			LOG.info("Server will be started with: "
					+ Collections.concate(", ", s));
		}

		registerControls();
		server.start(false);
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
		final String location = handler.getDefaultLocation();

		// shutdown the server
		server.shutdown();

		// release the configuration
		configuration.release();

		if (LOG.isInfoEnabled()) {
			LOG.info("The server is shut down.");
		}

		if (cleanUp) {
			if (!Files.deleteDir(new File(location))) {
				if (LOG.isWarnEnabled()) {
					LOG.warn("CleanUp of server failed.");
				}
			}
		}
	}

	/**
	 * Creates an instance using the default properties.
	 * 
	 * @return the created {@code TidaServer}
	 */
	public static TidaServer create() {
		return create(null);
	}

	/**
	 * Creates an instance of a {@code TidaServer}, which is completely
	 * auto-wired according to the configuration.
	 * 
	 * @param properties
	 *            properties to be set for the configuration
	 * 
	 * @return the created {@code TidaServer} instance
	 */
	public static TidaServer create(final Properties properties) {

		final List<PropertiesLoaderSupport> holders;
		if (properties == null) {
			holders = null;
		} else {
			holders = new ArrayList<PropertiesLoaderSupport>();

			// create a propertyHolder for the properties
			final SpringPropertyHolder holder = new SpringPropertyHolder();
			holder.setProperties(properties);
			holder.setLocalOverride(true);
			holder.setSystemPropertiesMode(PropertyPlaceholderConfigurer.SYSTEM_PROPERTIES_MODE_NEVER);
			holder.setOtherHolderOverride(false);

			// add the propertyHolder
			holders.add(holder);
		}

		// create the instance
		final ConfigurationCoreSettings settings = ConfigurationCoreSettings
				.loadCoreSettings("sbconfigurator-core.xml", TidaConfig.class,
						holders, null);
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
