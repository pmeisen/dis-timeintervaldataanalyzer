package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;

/**
 * {@code AggregationFunction} to calculate the {@code Median}.
 * 
 * @author pmeisen
 * 
 */
public class Median extends BaseAggregationFunction {
	private final static String name = "median";

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final FactDescriptorSet descriptors) {
		if (bitmap == null || descriptors == null) {
			return getDefaultValue();
		}

		// get the first descriptor to check if invariants are contained
		if (descriptors.containsVariantRecords()) {
			// TODO support it
			throw new UnsupportedOperationException("Currently not supported!");
		} else {
			final int amount = bitmap.determineCardinality();

			// determine if we have an even or odd amount
			final boolean even = (amount & 1) == 0;
			int firstPos = (int) Math.floor(amount * 0.5) + (even ? 0 : 1);

			// get the median
			int processedRecords = 0;
			double median = Double.NaN;
			for (final Descriptor<?, ?, ?> desc : descriptors) {

				// get the slice and the combined bitmap
				final Slice<?> metaSlice = index.getMetaIndexDimensionSlice(
						desc.getModelId(), desc.getId());
				final Bitmap bmp = bitmap.and(metaSlice.getBitmap());

				// add the new amount of records processed
				processedRecords += bmp.determineCardinality();

				// keep processing
				if (processedRecords < firstPos) {
					continue;
				}

				final double curValue = desc.getFactValue(null);
				if (processedRecords > firstPos) {

					// the current value is the median
					median = curValue;
					break;
				} else {

					// the median is the mean of two different values
					if (even) {

						// we didn't get any value yet
						if (Double.isNaN(median)) {
							median = curValue;
							firstPos++;
						}
						// we got the second value
						else {
							median = 0.5 * (median + curValue);
							break;
						}
					}
					// just get the value it's the median
					else {
						median = curValue;
						break;
					}
				}
			}

			return median;
		}
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts) {
		if (facts == null || facts.amountOfFacts() == 0) {
			return getDefaultValue();
		}

		// get the middle position
		final int amount = facts.amountOfFacts();
		final boolean even = (amount & 1) == 0;
		final int firstPos = (int) Math.floor(amount * 0.5) + (even ? -1 : 0);

		// calculate the median
		final double median;
		final double[] sortedFacts = facts.sortedFacts();
		if (even) {
			median = 0.5 * (sortedFacts[firstPos] + sortedFacts[firstPos + 1]);
		} else {
			median = sortedFacts[firstPos];
		}

		return median;
	}

	@Override
	public String getName() {
		return name;
	}
}
