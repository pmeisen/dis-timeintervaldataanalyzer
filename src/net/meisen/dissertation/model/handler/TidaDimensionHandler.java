package net.meisen.dissertation.model.handler;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.dimensions.IDimension;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.api.IModuleHolder;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class TidaDimensionHandler {

	/**
	 * The {@code ExceptionRegistry} used to handle exceptions.
	 */
	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	protected IExceptionRegistry exceptionRegistry;

	/**
	 * The loaded configuration used to load the {@code Dimension} instances
	 * using {@link IConfiguration#loadDelayed(String, InputStream)}.
	 */
	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	protected IConfiguration configuration;

	public Map<String, IDimension> loadDimensions(final File file) {
		try {
			return loadDimensions(new FileInputStream(file));
		} catch (final FileNotFoundException e) {
			// TODO add nice exception
			throw new IllegalStateException("INVALID FILE '"
					+ Files.getCanonicalPath(file) + "'.");
		}
	}

	public Map<String, IDimension> loadDimensions(final InputStream resIo) {

		// check if null was passed
		if (resIo == null) {
			// TODO add nice exception
			throw new IllegalStateException("NULL NOT SUPPORTED");
		}

		// get the holder
		final IModuleHolder moduleHolder = configuration.loadDelayed(
				"tidaXsltDimensionLoader", resIo);

		// just get the dimensions
		final Map<String, IDimension> dimensions = new HashMap<String, IDimension>();
		for (final Object module : moduleHolder.getAllModules().values()) {
			if (module instanceof IDimension) {
				final IDimension dimension = (IDimension) module;
				
				if (dimensions.put(dimension.getId(), (IDimension) module) != null) {
					// TODO: make it nice
					throw new IllegalStateException("TWO DIMENSIONS WITH SAME ID");
				}
			}
		}

		/*
		 * Release the moduleHolder, we just wanted the dimensions, they should
		 * not be attached to anything and work after the module is released.
		 */
		moduleHolder.release();

		return dimensions;
	}
}
