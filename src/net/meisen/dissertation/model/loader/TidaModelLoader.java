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

public class TidaModelLoader {
	private final static Logger LOG = LoggerFactory
			.getLogger(TidaModelLoader.class);

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	protected IExceptionRegistry exceptionRegistry;

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	protected IConfiguration configuration;

	private Map<String, IModuleHolder> moduleHolders = new ConcurrentHashMap<String, IModuleHolder>();

	protected IModuleHolder getModuleHolder(final String id,
			final InputStream is) {

		IModuleHolder moduleHolder = moduleHolders.get(id);
		if (moduleHolder == null) {
			moduleHolder = configuration.loadDelayed("tidaModelLoader", is);
			moduleHolders.put(id, moduleHolder);

			if (LOG.isInfoEnabled()) {
				LOG.info("Loaded ModuleHolder '" + id + "'.");
			}
		} else {
			// TODO throw exception
		}

		return moduleHolder;
	}

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

	public TidaModel load(final String id, final File file) {
		try {
			return load(id, new FileInputStream(file));
		} catch (final FileNotFoundException e) {
			// TODO Auto-generated catch block
			return null;
		}
	}

	public TidaModel load(final String id, final String classPathResource) {
		return load(id, getClass().getResourceAsStream(classPathResource));
	}

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
