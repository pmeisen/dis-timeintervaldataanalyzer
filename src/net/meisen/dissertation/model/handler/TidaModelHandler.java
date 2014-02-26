package net.meisen.dissertation.model.handler;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.TidaModelHandlerException;
import net.meisen.dissertation.impl.persistence.ZipPersistor;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.persistence.ILocation;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.dissertation.model.persistence.MetaData;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.resources.Xml;
import net.meisen.general.genmisc.types.Streams;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.api.IModuleHolder;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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
	private final static Logger LOG = LoggerFactory
			.getLogger(TidaModelHandler.class);

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
			exceptionRegistry.throwRuntimeException(
					TidaModelHandlerException.class, 1000, id);
		}

		IModuleHolder moduleHolder = moduleHolders.get(id);
		if (moduleHolder == null) {

			// keep the complete configuration in memory for saving purposes
			final byte[] config;
			try {
				config = Streams.copyStreamToByteArray(is);
			} catch (final IOException e) {
				exceptionRegistry.throwRuntimeException(
						TidaModelHandlerException.class, 1002, id);
				return null;
			}

			// load the configuration
			final InputStream bais = new ByteArrayInputStream(config);
			moduleHolder = configuration.loadDelayed("tidaXsltModelLoader",
					bais);

			// close the stream silently
			Streams.closeIO(bais);

			moduleHolders.put(id, moduleHolder);
			configurations.put(id, config);

			if (LOG.isInfoEnabled()) {
				LOG.info("Loaded ModuleHolder '" + id + "'.");
			}
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
			exceptionRegistry.throwRuntimeException(
					TidaModelHandlerException.class, 1003, e, file);
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

	/**
	 * Loads a {@code TidaModel} using a persisted {@code location}.
	 * 
	 * @param id
	 *            the id to keep track of the {@code ModuleHolder}
	 * @param location
	 *            the {@code Location} to load the {@code TidaModel} from
	 * 
	 * @return the loaded {@code TidaModel}
	 * 
	 * @see ILocation
	 */
	public TidaModel load(final String id, final ILocation location) {

		// check if a module has the id already
		if (moduleHolders.get(id) != null) {
			exceptionRegistry.throwRuntimeException(
					TidaModelHandlerException.class, 1001, id);
		}

		final Identifier configId = new Identifier("config.xml");
		final ZipPersistor persistor = new ZipPersistor();

		// set the identifiers to be handled
		persistor.setIncludedIdentifier(configId);

		// just load the MetaData
		final MetaData config = new MetaData(configId);
		persistor.load(location, config);

		// manipulate the configuration so that it can be used for loading
		final ManipulatedXml xml = manipulateXmlForLoading(config.getStream());

		// load the xml
		final ByteArrayInputStream configIs = new ByteArrayInputStream(
				xml.getXml());
		final TidaModel model = loadViaXslt(id, configIs);

		// reset the changes made for loading
		model.setOfflineModeByString(xml.getOldValue("offlinemode"));

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
	 *            the id of the {@code ModuleHolder} to save the
	 *            {@code TidaModel} from
	 * @param location
	 *            the {@code Location} to store the data
	 * 
	 * @see ILocation
	 */
	public void save(final String id, final ILocation location) {
		final Identifier configId = new Identifier("config.xml");
		final ZipPersistor persistor = new ZipPersistor();

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
	 * 
	 * @return the manipulated xml
	 */
	protected ManipulatedXml manipulateXmlForLoading(final byte[] xml) {
		final InputStream bais = new ByteArrayInputStream(xml);

		final ManipulatedXml manipulatedXml = manipulateXmlForLoading(bais);
		Streams.closeIO(bais);

		return manipulatedXml;
	}

	/**
	 * Manipulates the specified {@code xml} to be used for loading.
	 * 
	 * @param xml
	 *            the xml to be manipulated
	 * 
	 * @return the manipulated xml
	 */
	protected ManipulatedXml manipulateXmlForLoading(final InputStream xml) {
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
		Node attribute = nodeAttributes.getNamedItem("offlinemode");
		if (attribute == null) {
			attribute = doc.createAttribute("offlinemode");
		} else {
			manipulatedXml.addValue("offlinemode", attribute.getTextContent());
		}
		attribute.setTextContent("auto");

		// make sure it is added
		nodeAttributes.setNamedItem(attribute);

		manipulatedXml.setManipulatedXml(Xml.createByteArray(doc));

		return manipulatedXml;
	}
}
