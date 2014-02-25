package net.meisen.dissertation.model.handler;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.impl.persistence.ZipPersistor;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.persistence.ILocation;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.dissertation.model.persistence.MetaData;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Streams;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.api.IModuleHolder;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * A {@code TidaModelHandler} is an instance used to load {@code TidaModel}
 * instances.
 * 
 * @author pmeisen
 * 
 */
public class TidaModelHandler {
	private final static Logger LOG = LoggerFactory
			.getLogger(TidaModelHandler.class);

	/**
	 * The {@code ExceptionRegistry} used to handle exceptions.
	 */
	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	protected IExceptionRegistry exceptionRegistry;

	/**
	 * The loaded configuration used to load the {@code TidaModel} instances
	 * using {@link IConfiguration#loadDelayed(String, InputStream)}.
	 */
	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	protected IConfiguration configuration;

	private Map<String, IModuleHolder> moduleHolders = new ConcurrentHashMap<String, IModuleHolder>();
	private Map<String, byte[]> configurations = new ConcurrentHashMap<String, byte[]>();

	/**
	 * Helper method to load the modules defined by the specified {@code is}.
	 * The loaded {@code ModuleHolder} is kept internally within a {@code Map}.
	 * 
	 * @param id
	 *            the identifier used to refer to the {@code ModuleHolder}
	 *            instance
	 * @param is
	 *            the {@code InputStream} to load the modules from
	 * 
	 * @return the loaded {@code ModuleHolder}
	 * 
	 * @throws RuntimeException
	 *             if the specified resource is invalid or falsely defined
	 */
	protected IModuleHolder getModuleHolder(final String id,
			final InputStream is) {
		if (is == null) {
			// TODO throw exception
		}

		IModuleHolder moduleHolder = moduleHolders.get(id);
		if (moduleHolder == null) {

			// keep the complete configuration in memory for saving purposes
			final byte[] config;
			try {
				config = Streams.copyStreamToByteArray(is);
			} catch (final IOException e) {
				// TODO add exception
				throw new RuntimeException(e);
			}

			moduleHolder = configuration.loadDelayed("tidaXsltModelLoader",
					new ByteArrayInputStream(config));
			moduleHolders.put(id, moduleHolder);
			configurations.put(id, config);

			if (LOG.isInfoEnabled()) {
				LOG.info("Loaded ModuleHolder '" + id + "'.");
			}
		} else {
			// TODO throw exception
		}

		return moduleHolder;
	}

	/**
	 * Unloads all the loaded {@code TidaModel} instances.
	 */
	public synchronized void unloadAll() {
		for (final IModuleHolder moduleHolder : moduleHolders.values()) {
			moduleHolder.release();
		}

		if (LOG.isInfoEnabled()) {
			LOG.info("Unloaded all ModuleHolder " + moduleHolders.keySet()
					+ ".");
		}

		moduleHolders.clear();
	}

	/**
	 * Unload the specified {@code TidaModel} instance and all the related
	 * stuff.
	 * 
	 * @param id
	 *            the identifier of the {@code TidaModel} to be unloaded, the
	 *            identifier is specified when loading the {@code TidaModel}
	 */
	public synchronized void unload(final String id) {
		final IModuleHolder moduleHolder = moduleHolders.get(id);
		if (moduleHolder != null) {
			moduleHolders.remove(id);
			moduleHolder.release();

			if (LOG.isInfoEnabled()) {
				LOG.info("Unloaded ModuleHolder '" + id + "'.");
			}
		}
	}

	/**
	 * Loads a {@code TidaModel} from the specified {@code classPathResource}.
	 * 
	 * @param id
	 *            the identifier of the loaded instance used to refer to it when
	 *            releasing the {@code TidaModel}
	 * @param file
	 *            the {@code File} to load the {@code TidaModel} from
	 * 
	 * @return the loaded instance of the {@code TidaModel}
	 */
	public TidaModel loadViaXslt(final String id, final File file) {
		try {
			return loadViaXslt(id, new FileInputStream(file));
		} catch (final FileNotFoundException e) {
			// TODO Auto-generated catch block
			return null;
		}
	}

	/**
	 * Loads a {@code TidaModel} from the specified {@code classPathResource}.
	 * 
	 * @param id
	 *            the identifier of the loaded instance used to refer to it when
	 *            releasing the {@code TidaModel}
	 * @param classPathResource
	 *            the classpath location to load the {@code TidaModel} from
	 * 
	 * @return the loaded instance of the {@code TidaModel}
	 */
	public TidaModel loadViaXslt(final String id, final String classPathResource) {
		return loadViaXslt(id, getClass()
				.getResourceAsStream(classPathResource));
	}

	/**
	 * Loads a {@code TidaModel} from the specified {@code is}.
	 * 
	 * @param id
	 *            the identifier of the loaded instance used to refer to it when
	 *            releasing the {@code TidaModel}
	 * @param is
	 *            the {@code InputStream} to load the {@code TidaModel} from
	 * 
	 * @return the loaded instance of the {@code TidaModel}
	 */
	public synchronized TidaModel loadViaXslt(final String id,
			final InputStream is) {

		final TidaModel model = getModuleHolder(id, is).getModule(
				DefaultValues.TIDAMODEL_ID);

		if (LOG.isInfoEnabled()) {
			LOG.info("Loaded TidaModel '" + model.getId()
					+ "' from ModuleHolder '" + id + "'.");
		}

		// initialize the model
		model.initialize();

		return model;
	}

	public TidaModel load(final String id, final ILocation location) {
		final Identifier configId = new Identifier("config.xml");
		final ZipPersistor persistor = new ZipPersistor();

		// check if a module has the id already
		if (moduleHolders.get(id) != null) {
			// TODO throw exception
		}

		// just load the MetaData
		final MetaData config = new MetaData(configId);
		persistor.load(location, config);

		// now load the model
		final TidaModel model = loadViaXslt(id, config.getStream());
				
		// register the model and load again
		persistor.register(configId.getGroup().append("model"), model);
		persistor.load(location);
		
		return model;
	}

	public void save(final String id, final ILocation location) {
		final Identifier configId = new Identifier("config.xml");
		final ZipPersistor persistor = new ZipPersistor();

		// get the configuration and the holder
		final IModuleHolder moduleHolder = moduleHolders.get(id);
		final byte[] config = configurations.get(id);

		// get the module from the holder
		final TidaModel model = moduleHolder
				.getModule(DefaultValues.TIDAMODEL_ID);
		persistor.register(configId.getGroup().append("model"), model);

		// write the file
		final InputStream is = new ByteArrayInputStream(config);

		// save the data with the additional MetaData
		persistor.save(location, new MetaData(configId, is));
	}
}