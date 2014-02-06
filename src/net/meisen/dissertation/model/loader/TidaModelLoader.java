package net.meisen.dissertation.model.loader;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.api.IModuleHolder;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * A {@code TidaModelLoader} is an instance used to load {@code TidaModel} from
 * a XML-configuration.
 * 
 * @author pmeisen
 * 
 */
public class TidaModelLoader {
	private final static Logger LOG = LoggerFactory
			.getLogger(TidaModelLoader.class);

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
			moduleHolder = configuration.loadDelayed("tidaXsltModelLoader", is);
			moduleHolders.put(id, moduleHolder);

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
	public TidaModel load(final String id, final File file) {
		try {
			return load(id, new FileInputStream(file));
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
	public TidaModel load(final String id, final String classPathResource) {
		return load(id, getClass().getResourceAsStream(classPathResource));
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
	public synchronized TidaModel load(final String id, final InputStream is) {
		final TidaModel model = getModuleHolder(id, is).getModule(
				DefaultValues.TIDAMODEL_ID);

		if (LOG.isInfoEnabled()) {
			LOG.info("Loaded TidaModel '" + model.getId()
					+ "' from ModuleHolder '" + id + "'.");
		}

		return model;
	}
}
