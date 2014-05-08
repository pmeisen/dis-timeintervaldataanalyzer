package net.meisen.dissertation.model.measures;

import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;

/**
 * Interface for an aggregation function. A concrete implementation has to be
 * stateless and thread-safe, i.e. the aggregation function has to be executable
 * several times, from several threads, for several parameters from the same
 * instance of the function. Generally the system generates exactly one instance
 * of the concrete {@code AggregationFunction}. Intermediate results are
 * retrieved by the system and passed to the function again if the aggregation
 * is triggered again.
 * 
 * @author pmeisen
 * 
 */
public interface IAggregationFunction {

	/**
	 * Gets the name of the function.
	 * 
	 * @return the name of the function
	 */
	public String getName();

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

	/**
	 * Gets the default value normally used if no facts are available.
	 * 
	 * @return the default value
	 */
	public double getDefaultValue();
}
