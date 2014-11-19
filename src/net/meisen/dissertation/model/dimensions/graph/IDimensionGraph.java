package net.meisen.dissertation.model.dimensions.graph;

import java.util.Map;

import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.IDimension;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
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

	/**
	 * Resolved the {@code filterExpression} and creates the resulting bitmap.
	 * 
	 * @param filterExpression
	 *            the expression
	 * @param model
	 *            the {@code TidaModel} used
	 * 
	 * @return the bitmap
	 */
	public Bitmap createFilter(final String filterExpression,
			final TidaModel model);

	/**
	 * Resolved the {@code groupExpression} and creates the resulting bitmaps
	 * for each group.
	 * 
	 * @param groupExpression
	 *            the expression
	 * @param model
	 *            the {@code TidaModel} used
	 * 
	 * @return the bitmap
	 */
	public Map<String, Bitmap> createGroup(final String groupExpression,
			final TidaModel model);

}
