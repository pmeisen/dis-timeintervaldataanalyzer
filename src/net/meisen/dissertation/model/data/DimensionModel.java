package net.meisen.dissertation.model.data;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.DimensionModelException;
import net.meisen.dissertation.model.dimensions.IDimension;
import net.meisen.dissertation.model.dimensions.graph.IDimensionGraph;
import net.meisen.dissertation.model.handler.TidaDimensionHandler;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class DimensionModel {

	@Autowired
	@Qualifier(DefaultValues.DIMENSIONHANDLER_ID)
	private TidaDimensionHandler dimensionHandler;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private final Map<String, IDimensionGraph> dimensions;

	private boolean initialized;
	private Collection<IDimension> addedDimensions;

	public DimensionModel() {
		this.dimensions = new HashMap<String, IDimensionGraph>();

		this.addedDimensions = null;
		this.initialized = false;
	}

	/**
	 * Checks if the model is initialized.
	 * 
	 * @return {@code true} if the {@code DimensionModel} is initialized,
	 *         otherwise {@code false}
	 */
	public boolean isInitialized() {
		return initialized;
	}

	/**
	 * Adds the specified dimensions to the {@code DimensionModel}. This should
	 * only be done before or during initialization. The map should be read only
	 * after the initialization is performed.
	 * 
	 * @param dimensions
	 *            the dimensions to be added
	 */
	@Autowired(required = false)
	public void addDimensions(final Collection<IDimension> dimensions) {
		if (isInitialized()) {
			exceptionRegistry.throwException(DimensionModelException.class,
					1002);
		}

		addedDimensions = dimensions;
	}

	/**
	 * Initializes the {@code DimensionModel}.
	 * 
	 * @param tidaModel
	 *            the {@code TidaModel} the {@code DimensionModel} belongs to
	 */
	public void initialize(final TidaModel tidaModel) {
		if (isInitialized()) {
			exceptionRegistry.throwException(DimensionModelException.class,
					1001);
		} else if (addedDimensions == null) {
			return;
		}

		// add the dimensions and make sure there aren't any duplicates
		final Map<String, IDimensionGraph> dims = this.dimensionHandler
				.createMap(addedDimensions);

		for (final Entry<String, IDimensionGraph> e : dims.entrySet()) {
			if (this.dimensions.put(e.getKey(), e.getValue()) != null) {
				exceptionRegistry.throwException(DimensionModelException.class,
						1000, e.getKey());
			}
		}

		this.addedDimensions = null;
		this.initialized = true;
	}

	/**
	 * Gets the {@code DimensionGraph} for the specified {@code dimensionId}.
	 * 
	 * @param dimensionId
	 *            the identifier to get the graph for
	 * 
	 * @return the {@code IDimensionGraph} or {@code null} if not graph is
	 *         defined for the specified id
	 */
	public IDimensionGraph getDimension(final String dimensionId) {
		return this.dimensions.get(dimensionId);
	}

	/**
	 * Get the defined dimensions.
	 * 
	 * @return the defined dimensions
	 */
	public Collection<IDimensionGraph> getDimensions() {
		return Collections.unmodifiableCollection(this.dimensions.values());
	}
}
