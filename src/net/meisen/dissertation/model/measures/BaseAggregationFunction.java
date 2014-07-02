package net.meisen.dissertation.model.measures;

import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.dissertation.model.util.IDoubleIterator;

/**
 * Base implementation of an {@code AggregationFunction}.
 * 
 * @author pmeisen
 * 
 */
public abstract class BaseAggregationFunction implements IAggregationFunction {

	@Override
	public String toString() {
		return getName().toUpperCase();
	}

	/**
	 * Helper method to calculate the sum of the specified {@code facts}.
	 * 
	 * @param facts
	 *            the facts to calculate the sum for
	 * 
	 * @return the sum of the facts
	 */
	public double sum(final IFactsHolder facts) {

		// make sure we have values
		if (facts == null) {
			return 0.0;
		}

		// otherwise get the sum and calculate the average
		double sum = 0.0;
		final IDoubleIterator it = facts.factsIterator();
		while (it.hasNext()) {
			final double fact = it.next();
			sum += fact;
		}

		return sum;
	}

	/**
	 * Helper method to calculate the sum of the specified {@code facts}.
	 * 
	 * @param index
	 *            the {@code TidaIndex} to retrieve the meta-slices from
	 * @param bitmap
	 *            the defined bitmap of the filter and group
	 * @param descriptors
	 *            the descriptors of the fact
	 * 
	 * @return the sum of the facts
	 */
	public double sum(final TidaIndex index, final Bitmap bitmap,
			final FactDescriptorSet descriptors) {
		if (bitmap == null || descriptors == null) {
			return getDefaultValue();
		}

		double sum = 0;
		if (descriptors.containsVariantRecords()) {
			// TODO support it
			throw new UnsupportedOperationException("Currently not supported!");
		} else {
			for (final FactDescriptor<?> desc : descriptors) {

				// get the slice
				final Slice<?> metaSlice = index.getMetaIndexDimensionSlice(
						desc.getModelId(), desc.getId());

				// get the bitmap
				final Bitmap bmp = bitmap.and(metaSlice.getBitmap());
				sum += bmp.determineCardinality() * desc.getFact();
			}

			return sum;
		}
	}

	@Override
	public double getDefaultValue() {
		return Double.NaN;
	}
}
