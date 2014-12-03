package net.meisen.dissertation.model.measures;

import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;

/**
 * An aggregation function useful to aggregate data on a dimension level.
 * 
 * @author pmeisen
 * 
 */
public interface IDimAggregationFunction extends IAggregationFunction {

	/**
	 * Aggregates the specified values of the records defined by the
	 * {@code bitmap}.
	 * 
	 * @param index
	 *            the index to retrieve additional information
	 * @param bitmap
	 *            the bitmap defining the selected (i.e. by filtering, grouping
	 *            and time slicing) records
	 * @param descriptors
	 *            the descriptors attached to the current time slice
	 * 
	 * @return the aggregated value
	 */
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final FactDescriptorSet descriptors);

	/**
	 * Aggregates the {@code facts}.
	 * 
	 * @param index
	 *            the index to retrieve additional information
	 * @param bitmap
	 *            the bitmap defining the selected (i.e. by filtering, grouping
	 *            and time slicing) records
	 * @param facts
	 *            the facts to be aggregated
	 * 
	 * @return the aggregated value
	 */
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts);
}
