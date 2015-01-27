package net.meisen.dissertation.model.measures;

import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;

/**
 * An aggregation function applied on a bitmap of the lowest granularity. The
 * bitmap has a specific time-point assigned to it.
 * 
 * @author pmeisen
 * 
 */
public interface ILowAggregationFunction extends IAggregationFunction {

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
	 * @param timepoint
	 *            the moment in time the bitmap belongs to
	 * 
	 * @return the aggregated value
	 */
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts, final int timepoint);
}
