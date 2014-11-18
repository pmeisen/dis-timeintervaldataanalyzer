package net.meisen.dissertation.model.dimensions.graph;

import java.util.Map;

import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.IDimension;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

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

	public IDimension getDimension();

	public Bitmap createFilter(final String filterExpression,
			final TidaModel model);

	public Map<String, Bitmap> createGroup(final String groupExpression,
			final TidaModel model);

}
