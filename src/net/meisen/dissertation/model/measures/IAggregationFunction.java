package net.meisen.dissertation.model.measures;

import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;

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

	public String getName();

	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final Iterable<Descriptor<?, ?, ?>> descriptors);

	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final double[] facts);

}
