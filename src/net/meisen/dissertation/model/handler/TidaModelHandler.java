package net.meisen.dissertation.model.handler;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.TidaModelException;
import net.meisen.dissertation.exceptions.TidaModelHandlerException;
import net.meisen.dissertation.impl.persistence.ZipPersistor;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.persistence.ILocation;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.dissertation.model.persistence.MetaData;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.resources.IByteBufferReader;
import net.meisen.general.genmisc.resources.Xml;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.genmisc.types.Streams;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.api.IModuleHolder;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;

/**
 * A {@code TidaModelHandler} is an instance used to load {@code TidaModel}
 * instances.
 * 
 * @author pmeisen
 * 
 */
public class TidaModelHandler {
	private final static String AUTOLOAD_FILENAME = "handler.data";
	private final static String MODEL_FILENAME = "model.xml";
	private final static Logger LOG = LoggerFactory
			.getLogger(TidaModelHandler.class);

	private String defaultLocation;

	private ReentrantReadWriteLock autoloadLock = new ReentrantReadWriteLock();

	/**
	 * Class to keep track of changes on an xml document.
	 * 
	 * @author pmeisen
	 * 
	 */
	protected class ManipulatedXml {
		private byte[] manipulatedXml;
		private Map<String, String> oldValues = new HashMap<String, String>();

		/**
		 * Adds the old value of a changed value.
		 * 
		 * @param name
		 *            the name of the changed value
		 * @param oldValue
		 *            the old value
		 */
		public void addValue(final String name, final String oldValue) {
			oldValues.put(name, oldValue);
		}

		/**
		 * Gets the old value of the specified {@code name}.
		 * 
		 * @param name
		 *            the name to get the old value for
		 * 
		 * @return the old value or {@code null} if no old value was set or
		 *         known
		 */
		public String getOldValue(final String name) {
			return oldValues.get(name);
		}

		/**
		 * Gets the manipulated xml.
		 * 
		 * @return the manipulated xml
		 */
		public byte[] getXml() {
			return manipulatedXml;
		}

		/**
		 * Sets the manipulated xml.
		 * 
		 * @param manipulatedXml
		 *            the manipulated xml
		 */
		public void setManipulatedXml(final byte[] manipulatedXml) {
			this.manipulatedXml = manipulatedXml;
		}
	}

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
	 * Initializes the handler.
	 */
	public void init() {
		this.autoloadModels();
	}

	/**
	 * The {@code TidaModel} instances held by {@code this}.
	 * 
	 * @return a set of loaded {@code TidaModel} instances
	 */
	public Set<String> getTidaModels() {
		return Collections.unmodifiableSet(moduleHolders.keySet());
	}

	/**
	 * Gets all the identifiers loaded automatically.
	 * 
	 * @return the identifiers loaded automatically
	 */
	public Set<String> getAutoloadedTidaModels() {
		autoloadLock.readLock().lock();
		try {
			final Set<String> modelIds = _readAutoloads();
			return modelIds;
		} finally {
			autoloadLock.readLock().unlock();
		}
	}

	/**
	 * Gets the models available by the handler.
	 * 
	 * @return the available (i.e. also not loaded models)
	 */
	public Set<String> getAvailableTidaModels() {
		final Set<String> set = new HashSet<String>();

		final File defLoc = new File(getDefaultLocation());
		final List<File> modelDirs = Files.getCurrentSubDirectories(defLoc);

		for (final File modelDir : modelDirs) {
			final File modelFile = new File(modelDir, MODEL_FILENAME);
			if (Files.checkFile(modelFile, false) != null) {
				set.add(modelDir.getName());
			}
		}

		return set;
	}

	/**
	 * Gets the {@code TidaModel} loaded by the {@code ModuleHolder} with the
	 * specified id. If no {@code ModuleHolder} with the specified id is loaded,
	 * {@code null} is returned.
	 * 
	 * @param id
	 *            the if to moduleHolder to get the {@code TidaModel} for
	 * 
	 * @return the {@code TidaModel} or {@code null} if the id is unknown
	 */
	public TidaModel getTidaModel(final String id) {
		final IModuleHolder holder = moduleHolders.get(id);

		if (holder == null) {
			return null;
		} else {
			return holder.getModule(DefaultValues.TIDAMODEL_ID);
		}
	}

	/**
	 * Helper method to load the modules defined by the specified {@code is}.
	 * The loaded {@code ModuleHolder} is kept internally within a {@code Map}.
	 * 
	 * @param is
	 *            the {@code InputStream} to load the modules from
	 * @param force
	 *            {@code true} if the model should be loaded even if this might
	 *            lead to an exception, because another or the same model with
	 *            the same identifier exists, otherwise {@code false}
	 * 
	 * @return the loaded {@code ModuleHolder}
	 * 
	 * @throws RuntimeException
	 *             if the specified resource is invalid or falsely defined
	 */
	protected IModuleHolder getModuleHolder(final InputStream is,
			final boolean force) {
		if (is == null) {
			exceptionRegistry.throwRuntimeException(
					TidaModelHandlerException.class, 1000);
		}

		// keep the complete configuration in memory for saving purposes
		final byte[] config;
		try {
			config = Streams.copyStreamToByteArray(is);
		} catch (final IOException e) {
			exceptionRegistry.throwRuntimeException(
					TidaModelHandlerException.class, 1002);
			return null;
		}

		// load the configuration
		final InputStream bais = new ByteArrayInputStream(config);
		IModuleHolder moduleHolder = null;
		try {
			moduleHolder = configuration.loadDelayed("tidaXsltModelLoader",
					bais);
		} catch (final BeanCreationException e) {
			final Throwable cause = e.getMostSpecificCause();
			if (cause != null && cause instanceof ForwardedRuntimeException) {
				exceptionRegistry
						.throwRuntimeException((ForwardedRuntimeException) cause);
			} else {
				throw e;
			}
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}

		// close the streams silently
		Streams.closeIO(bais);
		Streams.closeIO(is);

		final TidaModel model = moduleHolder
				.getModule(DefaultValues.TIDAMODEL_ID);
		final String id = model.getId();

		if (moduleHolders.get(id) == null) {
			moduleHolders.put(id, moduleHolder);
			configurations.put(id, config);

			if (LOG.isInfoEnabled()) {
				LOG.info("Loaded ModuleHolder '" + id + "'.");
			}
		} else if (!force) {
			return moduleHolders.get(id);
		} else {
			exceptionRegistry.throwRuntimeException(
					TidaModelHandlerException.class, 1001, id);
		}

		return moduleHolder;
	}

	/**
	 * Unloads all the loaded {@code TidaModel} instances.
	 */
	public synchronized void unloadAll() {
		for (final IModuleHolder moduleHolder : moduleHolders.values()) {
			final TidaModel model = moduleHolder
					.getModule(DefaultValues.TIDAMODEL_ID);
			model.release();

			moduleHolder.release();
		}

		if (LOG.isInfoEnabled()) {
			LOG.info("Unloaded all ModuleHolder " + moduleHolders.keySet()
					+ ".");
		}

		configurations.clear();
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
			configurations.remove(id);
			moduleHolder.release();

			// release the model
			final TidaModel model = moduleHolder
					.getModule(DefaultValues.TIDAMODEL_ID);
			model.release();

			if (LOG.isInfoEnabled()) {
				LOG.info("Unloaded ModuleHolder '" + id + "'.");
			}
		}
	}

	/**
	 * Loads a {@code TidaModel} from the specified {@code classPathResource}.
	 * 
	 * @param file
	 *            the {@code File} to load the {@code TidaModel} from
	 * 
	 * @return the loaded instance of the {@code TidaModel}
	 */
	public TidaModel loadViaXslt(final File file) {
		if (file == null) {
			return loadViaXslt((InputStream) null);
		} else {
			try {
				return loadViaXslt(new FileInputStream(file));
			} catch (final FileNotFoundException e) {
				exceptionRegistry.throwRuntimeException(
						TidaModelHandlerException.class, 1003, e, file);
				return null;
			}
		}
	}

	/**
	 * Loads a {@code TidaModel} from the specified {@code classPathResource}.
	 * 
	 * @param classPathResource
	 *            the classpath location to load the {@code TidaModel} from
	 * 
	 * @return the loaded instance of the {@code TidaModel}
	 */
	public TidaModel loadViaXslt(final String classPathResource) {

		if (classPathResource == null) {
			return loadViaXslt((InputStream) null);
		} else {
			return loadViaXslt(getClass()
					.getResourceAsStream(classPathResource));
		}
	}

	/**
	 * Loads a {@code TidaModel} from the specified {@code is}.
	 * 
	 * @param is
	 *            the {@code InputStream} to load the {@code TidaModel} from
	 * 
	 * @return the loaded instance of the {@code TidaModel}
	 */
	public synchronized TidaModel loadViaXslt(final InputStream is) {
		return loadViaXslt(is, true);
	}

	/**
	 * Loads a {@code TidaModel} from the specified {@code is}.
	 * 
	 * @param is
	 *            the {@code InputStream} to load the {@code TidaModel} from
	 * @param force
	 *            {@code true} if the model should be loaded even if this might
	 *            lead to an exception, because another or the same model with
	 *            the same identifier exists, otherwise {@code false}
	 * 
	 * @return the loaded instance of the {@code TidaModel}
	 */
	public synchronized TidaModel loadViaXslt(final InputStream is,
			final boolean force) {
		final TidaModel model = getModuleHolder(is, force).getModule(
				DefaultValues.TIDAMODEL_ID);

		// check if the model is already loaded
		if (!force && model.isInitialized()) {
			return model;
		} else if (LOG.isInfoEnabled()) {
			LOG.info("Loaded TidaModel '" + model.getId()
					+ "' from ModuleHolder '" + model.getId() + "'.");
		}

		// initialize the model
		try {
			model.initialize();

			// define the file to store the model at
			final String modelId = model.getId();
			final File modelDir = getModelDir(model.getId());
			final File modelFile = new File(modelDir, MODEL_FILENAME);
			if (modelFile.exists()) {
				if (modelFile.isDirectory()) {
					exceptionRegistry.throwRuntimeException(
							TidaModelHandlerException.class, 1006, modelFile);
				} else if (!modelFile.delete()) {
					exceptionRegistry.throwRuntimeException(
							TidaModelHandlerException.class, 1007, modelFile);
				}
			} else {
				modelDir.mkdirs();
			}

			// copy the file to the specified location
			final byte[] config = configurations.get(modelId);
			try {
				Streams.copyStreamToFile(new ByteArrayInputStream(config),
						modelFile);
			} catch (final IOException e) {
				exceptionRegistry.throwRuntimeException(
						TidaModelHandlerException.class, 1008, e, modelFile);
			}
		} catch (final Throwable t) {
			if (model != null) {

				/*
				 * if an exception is thrown we have to remove the model, at
				 * least try it
				 */
				try {
					this.deleteModel(model.getId());
				} catch (final RuntimeException ignore) {
					if (LOG.isWarnEnabled()) {
						LOG.warn("Deleting the failed model '" + model.getId()
								+ "' failed.", ignore);
					}
				}
			}

			if (t instanceof RuntimeException) {
				throw (RuntimeException) t;
			} else {
				exceptionRegistry.throwRuntimeException(
						TidaModelHandlerException.class, 1016, t,
						model.getId(), t.getLocalizedMessage());
			}
		}

		return model;
	}

	/**
	 * Loads a previously loaded model from the default location, i.e. the
	 * location where all the models are stored.
	 * 
	 * @param modelId
	 *            the identifier of the model to be loaded
	 * 
	 * @return the loaded {@code TidaModel}
	 */
	public synchronized TidaModel loadFromDefaultLocation(final String modelId) {
		final File modelDir = new File(getDefaultLocation(), modelId);
		final File modelFile = new File(modelDir, MODEL_FILENAME);

		// check if the file does not exist or if it's a directory
		if (!modelFile.exists() || modelFile.isDirectory()) {
			exceptionRegistry.throwRuntimeException(
					TidaModelHandlerException.class, 1009, modelId, modelFile);
		}

		return loadViaXslt(modelFile);
	}

	/**
	 * Loads a {@code TidaModel} using a persisted {@code location}.
	 * 
	 * @param location
	 *            the {@code Location} to load the {@code TidaModel} from
	 * 
	 * @return the loaded {@code TidaModel}
	 * 
	 * @see ILocation
	 */
	public TidaModel load(final ILocation location) {

		final Identifier configId = new Identifier("config.xml");
		final ZipPersistor persistor = new ZipPersistor(exceptionRegistry);

		// set the identifiers to be handled
		persistor.setIncludedIdentifier(configId);

		// just load the MetaData
		final MetaData config = new MetaData(configId);
		persistor.load(location, config);

		// manipulate the configuration so that it can be used for loading
		final ManipulatedXml xml = manipulateXmlForLoading(config.getStream(),
				"offlinemode", "auto");

		// load the xml
		final ByteArrayInputStream configIs = new ByteArrayInputStream(
				xml.getXml());
		final TidaModel model = loadViaXslt(configIs);
		if (!"auto".equalsIgnoreCase(xml.getOldValue("offlinemode"))) {
			if (LOG.isWarnEnabled()) {
				LOG.warn("The 'offlinemode' of the model was changed during the loading from the location '"
						+ location
						+ "' (old: '"
						+ xml.getOldValue("offlinemode") + "', new: 'auto')");
			}
		}

		// close the created and used stream
		Streams.closeIO(configIs);

		// set the identifiers to be handled
		persistor.clearAllIdentifiers();
		persistor.addExcludedIdentifier(configId);

		// register the model and load again
		persistor.register(configId.getGroup().append("model"), model);
		persistor.load(location);

		return model;
	}

	/**
	 * Saves the {@code TidaModel} specified by the {@code id} under the
	 * specified {@code location}.
	 * 
	 * @param id
	 *            the id of the {@code TidaModel} to be saved
	 * @param location
	 *            the {@code Location} to store the data
	 * 
	 * @see ILocation
	 */
	public void save(final String id, final ILocation location) {
		final Identifier configId = new Identifier("config.xml");
		final ZipPersistor persistor = new ZipPersistor(exceptionRegistry);

		// get the configuration and the holder
		final IModuleHolder moduleHolder = moduleHolders.get(id);
		if (moduleHolder == null) {
			exceptionRegistry.throwRuntimeException(
					TidaModelHandlerException.class, 1004, id);
		}
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

	/**
	 * Manipulates the specified {@code xml} to be used for loading.
	 * 
	 * @param xml
	 *            the xml to be manipulated
	 * @param key
	 *            the attribute to be manipulated
	 * @param value
	 *            the new value of the attribute
	 * 
	 * @return the manipulated xml
	 */
	protected ManipulatedXml manipulateXmlForLoading(final byte[] xml,
			final String key, final String value) {
		final Properties p = new Properties();
		p.setProperty("offlinemode", "auto");

		return manipulateXmlForLoading(xml, p);
	}

	/**
	 * Manipulates the specified {@code xml} to be used for loading.
	 * 
	 * @param xml
	 *            the xml to be manipulated
	 * @param properties
	 *            the attributes to be manipulated
	 * 
	 * @return the manipulated xml
	 */
	protected ManipulatedXml manipulateXmlForLoading(final byte[] xml,
			final Properties properties) {
		final InputStream bais = new ByteArrayInputStream(xml);

		final ManipulatedXml manipulatedXml = manipulateXmlForLoading(bais,
				properties);
		Streams.closeIO(bais);

		return manipulatedXml;
	}

	/**
	 * Manipulates the specified {@code xml} to be used for loading.
	 * 
	 * @param xml
	 *            the xml to be manipulated
	 * @param key
	 *            the attribute to be manipulated
	 * @param value
	 *            the new value of the attribute
	 * 
	 * @return the manipulated xml
	 */
	protected ManipulatedXml manipulateXmlForLoading(final InputStream xml,
			final String key, final String value) {
		final Properties p = new Properties();
		p.setProperty("offlinemode", "auto");

		return manipulateXmlForLoading(xml, p);
	}

	/**
	 * Manipulates the specified {@code xml} to be used for loading.
	 * 
	 * @param xml
	 *            the xml to be manipulated
	 * @param properties
	 *            the attributes to be manipulated
	 * 
	 * @return the manipulated xml
	 */
	protected ManipulatedXml manipulateXmlForLoading(final InputStream xml,
			final Properties properties) {
		final Document doc = Xml.createDocument(xml, true);

		// check if the document could be read
		if (doc == null) {
			exceptionRegistry.throwRuntimeException(
					TidaModelHandlerException.class, 1005);
		}

		// prepare the result
		final ManipulatedXml manipulatedXml = new ManipulatedXml();

		// get the root element and it's attributes
		final Node root = doc.getDocumentElement();
		final NamedNodeMap nodeAttributes = root.getAttributes();

		// manipulate the offlinemode attribute
		for (final Entry<Object, Object> e : properties.entrySet()) {
			final String key = (String) e.getKey();

			Node attribute = nodeAttributes.getNamedItem(key);
			if (attribute == null) {
				attribute = doc.createAttribute(key);
			} else {
				manipulatedXml.addValue(key, attribute.getTextContent());
			}
			attribute.setTextContent((String) e.getValue());

			// make sure it is added
			nodeAttributes.setNamedItem(attribute);
		}

		manipulatedXml.setManipulatedXml(Xml.createByteArray(doc));

		return manipulatedXml;
	}

	/**
	 * Gets the default location under which the models are stored.
	 * 
	 * @return the default location under which the models are stored
	 */
	public String getDefaultLocation() {
		return getDefaultLocation(false);
	}

	/**
	 * Gets the default location under which the models are stored. If needed
	 * (i.e. {@code createIfNotAvailable} is set to {@code true}) the location
	 * is created.
	 * 
	 * @param createIfNotAvailable
	 *            {@code true} if the location should be created if not
	 *            available, otherwise {@code false}
	 * 
	 * @return the default location under which the models are stored
	 */
	public String getDefaultLocation(final boolean createIfNotAvailable) {
		if (createIfNotAvailable) {
			final File locFile = new File(defaultLocation);
			if (!locFile.exists() && !locFile.mkdirs()) {
				exceptionRegistry.throwRuntimeException(
						TidaModelHandlerException.class, 1015, defaultLocation);
			}
		}

		return defaultLocation;
	}

	/**
	 * Specifies the default location under which the models are stored.
	 * 
	 * @param defaultLocation
	 *            the default location under which the models are stored
	 */
	public void setDefaultLocation(final String defaultLocation) {
		this.defaultLocation = defaultLocation;
	}

	/**
	 * Enables the automatically loading for the specified {@code modelId}.
	 * 
	 * @param modelId
	 *            the modelId to be automatically loaded
	 * @throws TidaModelHandlerException
	 *             if the {@code TidaModel} with the specified {@code modelId}
	 *             is not known, i.e. was never loaded before
	 */
	public void enableAutoload(final String modelId)
			throws TidaModelHandlerException {

		// check if the directory exists
		final File modelDir = getModelDir(modelId);
		if (!modelDir.exists()) {
			exceptionRegistry.throwRuntimeException(
					TidaModelHandlerException.class, 1011, modelId, modelDir);
		}

		// check if the file exists
		final File modelFile = new File(modelDir, MODEL_FILENAME);
		if (!modelFile.exists()) {
			exceptionRegistry.throwRuntimeException(
					TidaModelHandlerException.class, 1011, modelId, modelFile);
		}

		// the model is qualified to be autoloaded
		setAutoload(modelId, true);
	}

	/**
	 * Disables the automatically loading for the specified {@code modelId}.
	 * 
	 * @param modelId
	 *            the modelId to be automatically loaded
	 */
	public void disableAutoload(final String modelId) {
		setAutoload(modelId, false);
	}

	/**
	 * Loads all the models defined for automatically loading.
	 */
	public void autoloadModels() {
		autoloadLock.readLock().lock();
		final Collection<String> modelIds;
		try {
			modelIds = _readAutoloads();
		} finally {
			autoloadLock.readLock().unlock();
		}

		if (LOG.isInfoEnabled()) {
			LOG.info("Autoloading the modules: " + modelIds);
		}

		// load each model
		for (final String modelId : modelIds) {
			this.loadFromDefaultLocation(modelId);
		}
	}

	/**
	 * Sets the automatically loading, i.e. {@code true} to enable and
	 * {@code false} to disable, for the specified {@code modelId}.
	 * 
	 * @param modelId
	 *            the identifier of the model to be set
	 * @param autoload
	 *            {@code true} to enable auto-loading, otherwise {@code false}
	 */
	protected void setAutoload(final String modelId, final boolean autoload) {
		autoloadLock.writeLock().lock();

		try {
			final Set<String> modelIds = _readAutoloads();
			if (modelIds.contains(modelId) == autoload) {
				// autoload == true == contains => nothing to do
				// autoload == false == contains => nothing to do
				return;
			}

			// modify the set according to the autoload argument
			if (autoload) {
				modelIds.add(modelId);
			} else {
				modelIds.remove(modelId);
			}

			// remove the file if it exists
			final File autoloadFile = getAutoloadFile();
			if (autoloadFile.exists() && !autoloadFile.delete()) {
				exceptionRegistry.throwRuntimeException(
						TidaModelHandlerException.class, 1012, autoloadFile);
				return;
			}

			// create the file and get access
			final FileOutputStream out;
			try {
				autoloadFile.createNewFile();
				out = new FileOutputStream(autoloadFile);
			} catch (final IOException e) {
				exceptionRegistry.throwRuntimeException(
						TidaModelHandlerException.class, 1013, e, autoloadFile);
				return;
			}

			// write everything to the file
			try {
				out.write(Streams.writeAllObjects(modelIds.toArray()));
			} catch (final IOException e) {
				exceptionRegistry.throwRuntimeException(
						TidaModelHandlerException.class, 1014, e, autoloadFile);
				return;
			} finally {
				Streams.closeIO(out);
			}
		} finally {
			autoloadLock.writeLock().unlock();
		}
	}

	/**
	 * Reads the file of models to be automatically loaded and returns the
	 * defined identifiers of the models to be automatically loaded.
	 * 
	 * @return set of identifiers of {@code TidaModel} instances to be
	 *         automatically loaded
	 */
	protected Set<String> _readAutoloads() {
		final Set<String> autoloads = new HashSet<String>();
		final File autoloadFile = getAutoloadFile();

		if (!autoloadFile.exists()) {
			// do nothing
		} else if (autoloadFile.isDirectory()) {
			exceptionRegistry.throwRuntimeException(
					TidaModelHandlerException.class, 1010, autoloadFile);
		} else {
			final IByteBufferReader reader = Streams
					.createByteBufferReader(autoloadFile);

			try {

				// read all the objects
				while (reader.hasRemaining()) {
					final String modelId = (String) Streams
							.readNextObject(reader);
					final File modelDir = getModelDir(modelId);

					// validate the directory
					if (!modelDir.exists() || !modelDir.isDirectory()) {
						exceptionRegistry.throwRuntimeException(
								TidaModelHandlerException.class, 1011, modelId,
								modelDir);
					} else {
						autoloads.add(modelId);
					}
				}
			} finally {
				Streams.closeIO(reader);
			}
		}

		return autoloads;
	}

	/**
	 * Gets the file containing all the models' identifier to be automatically
	 * loaded.
	 * 
	 * @return the file containing all the models' identifier to be
	 *         automatically loaded
	 * 
	 * @see #_readAutoloads()
	 */
	protected File getAutoloadFile() {
		return new File(getDefaultLocation(), AUTOLOAD_FILENAME);
	}

	/**
	 * Gets the default directory used to store data about a specific
	 * {@code TidaModel}.
	 * 
	 * @param modelId
	 *            the identifier of the model to get the directory for
	 * 
	 * @return default directory used to store data for the {@code TidaModel}
	 *         with the specified {@code modelId}
	 */
	protected File getModelDir(final String modelId) {
		return new File(getDefaultLocation(), modelId);
	}

	/**
	 * Deletes the model with the specified id and removes it from the handler.
	 * 
	 * @param modelId
	 *            the model to be deleted
	 */
	public void deleteModel(final String modelId) {

		// we have to get the loaded model (if it exists)
		final TidaModel model = getTidaModel(modelId);

		disableAutoload(modelId);
		unload(modelId);

		// finally we have to delete the model's file
		final File dir = getModelDir(modelId);
		if (dir.exists() && !Files.deleteOnExitDir(dir)) {
			if (LOG.isWarnEnabled()) {
				LOG.warn("Unable to remove the model's directory at '"
						+ Files.getCanonicalPath(dir) + "'.");
			}
		}

		// delete the folder of the model if there is one
		if (model != null) {
			try {
				model.release(true);
			} catch (final TidaModelException e) {
				/*
				 * TODO: the directory might be not deletable, because of MapDB
				 */
				if (LOG.isErrorEnabled()) {
					LOG.error("Could not clean-up correctly!", e);
				}
			}
		}
	}
}
