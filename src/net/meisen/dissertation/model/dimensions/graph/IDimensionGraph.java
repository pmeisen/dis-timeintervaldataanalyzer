package net.meisen.dissertation.model.dimensions.graph;

import net.meisen.dissertation.model.dimensions.IDimension;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * An internal graph representation of a dimension.
 * 
 * @author pmeisen
 * 
 * @see IDimension
 * 
 */
public interface IDimensionGraph {

	/**
	 * Creates the graph based on the specified {@code IDimension}.
	 * 
	 * @param dimension
	 *            the dimension to create the graph for
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the creation fails, e.g. because of an invalid definition
	 */
	public void create(final IDimension dimension)
			throws ForwardedRuntimeException;

	/**
	 * Gets the dimension the graph belongs to.
	 * 
	 * @return the dimension the graph belongs to
	 */
	public IDimension getDimension();
}
