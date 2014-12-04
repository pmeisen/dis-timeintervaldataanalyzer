package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.dissertation.model.measures.ILowAggregationFunction;

/**
 * Aggregator to determine the count-started.
 * 
 * @author pmeisen
 * 
 */
public class CountStarted extends BaseAggregationFunction implements
		ILowAggregationFunction {
	private final static String name = "countstarted";

	@Override
	public String getName() {
		return name;
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final FactDescriptorSet descriptors, final int timepoint) {
		if (bitmap == null || descriptors == null) {
			return getDefaultValue();
		} else {
			return aggregate(index, bitmap, timepoint);
		}
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts, final int timepoint) {

		if (bitmap == null || facts == null || facts.amountOfFacts() == 0) {
			return getDefaultValue();
		} else {
			return aggregate(index, bitmap, timepoint);
		}
	}

	/**
	 * Aggregates the bitmap and determines the count-started value
	 * 
	 * @param index
	 *            the {@code TidaIndex}
	 * @param bitmap
	 *            the bitmap of the current time-point
	 * @param timepoint
	 *            the normalized time-point
	 * 
	 * @return the aggregated value
	 */
	protected double aggregate(final TidaIndex index, final Bitmap bitmap,
			final int timepoint) {

		if (index.getNormalizedTimeStart() == timepoint) {
			return bitmap.determineCardinality();
		} else {
			final SliceWithDescriptors<?>[] slices = index
					.getIntervalIndexSlices(timepoint - 1, timepoint - 1);
			if (slices == null || slices.length != 1 || slices[0] == null) {
				return getDefaultValue();
			} else {
				final SliceWithDescriptors<?> slice = slices[0];
				final Bitmap prevBitmap = slice.getBitmap();

				return prevBitmap.xor(bitmap).and(bitmap)
						.determineCardinality();
			}
		}
	}

	@Override
	public double getDefaultValue() {
		return 0.0;
	}
}
