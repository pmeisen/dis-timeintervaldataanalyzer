package net.meisen.dissertation.model.handler;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.TidaDimensionHandlerException;
import net.meisen.dissertation.model.dimensions.IDimension;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.api.IModuleHolder;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Handler used to load a dimension definition.
 * 
 * @author pmeisen
 * 
 */
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

	/**
	 * Loads the dimensions defined within the specified {@code file}.
	 * 
	 * @param file
	 *            the file to load the definition from
	 * 
	 * @return the loaded dimensions
	 */
	public Map<String, IDimension> loadDimensions(final File file) {
		try {
			return loadDimensions(new FileInputStream(file));
		} catch (final FileNotFoundException e) {
			exceptionRegistry.throwException(
					TidaDimensionHandlerException.class, 1000,
					Files.getCanonicalPath(file));
			return null;
		}
	}

	/**
	 * Loads the dimensions defined within the specified {@code InputStream}.
	 * 
	 * @param resIo
	 *            the stream to load the data from
	 * 
	 * @return the loaded dimensions
	 */
	public Map<String, IDimension> loadDimensions(final InputStream resIo) {

		// check if null was passed
		if (resIo == null) {
			exceptionRegistry.throwException(
					TidaDimensionHandlerException.class, 1001);
		}

		// get the holder
		final IModuleHolder moduleHolder = configuration.loadDelayed(
				"tidaXsltDimensionLoader", resIo);

		// just get the dimensions
		final Map<String, IDimension> dimensions = new HashMap<String, IDimension>();
		for (final Object dim : moduleHolder.getAllModules().values()) {
			if (dim instanceof IDimension) {
				final IDimension dimension = (IDimension) dim;
				final String id = dimension.getId();
				dimensions.put(id, dimension);
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
