package net.meisen.dissertation.model.handler;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.TidaDimensionHandlerException;
import net.meisen.dissertation.model.dimensions.DescriptorDimension;
import net.meisen.dissertation.model.dimensions.IDimension;
import net.meisen.dissertation.model.dimensions.graph.DescriptorDimensionGraph;
import net.meisen.dissertation.model.dimensions.graph.IDimensionGraph;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
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

	public Map<String, IDimensionGraph> loadDimensions(final File file) {
		try {
			return loadDimensions(new FileInputStream(file));
		} catch (final FileNotFoundException e) {
			exceptionRegistry.throwException(
					TidaDimensionHandlerException.class, 1000,
					Files.getCanonicalPath(file));
			return null;
		}
	}

	public Map<String, IDimensionGraph> loadDimensions(final InputStream resIo) {

		// check if null was passed
		if (resIo == null) {
			exceptionRegistry.throwException(
					TidaDimensionHandlerException.class, 1001);
		}

		// get the holder
		final IModuleHolder moduleHolder = configuration.loadDelayed(
				"tidaXsltDimensionLoader", resIo);

		// just get the dimensions
		final Map<String, IDimensionGraph> dimensions = createMap(moduleHolder
				.getAllModules().values());

		/*
		 * Release the moduleHolder, we just wanted the dimensions, they should
		 * not be attached to anything and work after the module is released.
		 */
		moduleHolder.release();

		return dimensions;
	}

	public Map<String, IDimensionGraph> createMap(final Collection<?> dims) {
		final Map<String, IDimensionGraph> dimensions = new HashMap<String, IDimensionGraph>();

		for (final Object dim : dims) {
			if (dim instanceof IDimension) {
				final IDimension dimension = (IDimension) dim;
				final String id = dimension.getId();
				final IDimensionGraph graph = createGraph(dimension);

				if (dimensions.put(id, graph) != null) {
					exceptionRegistry.throwException(
							TidaDimensionHandlerException.class, 1002, id);
				}
			}
		}

		return dimensions;
	}

	public IDimensionGraph createGraph(final IDimension dimension) {
		final IDimensionGraph graph;

		// factory to pick the correct type for the dimension
		if (dimension instanceof DescriptorDimension) {
			graph = new DescriptorDimensionGraph();
		} else {
			final String type = dimension == null ? null : dimension.getClass()
					.getSimpleName();
			exceptionRegistry.throwException(
					TidaDimensionHandlerException.class, 1003, type);
			return null;
		}

		// create the graph and format exceptions
		try {
			graph.create(dimension);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
		}

		return graph;
	}
}
