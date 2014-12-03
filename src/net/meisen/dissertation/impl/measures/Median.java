package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;
import net.meisen.dissertation.model.measures.IDimAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.dissertation.model.measures.ILowAggregationFunction;
import net.meisen.dissertation.model.util.IDoubleIterator;

/**
 * {@code AggregationFunction} to calculate the {@code Median}.
 * 
 * @author pmeisen
 * 
 */
public class Median extends BaseAggregationFunction implements
		ILowAggregationFunction, IDimAggregationFunction {
	private final static String name = "median";

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final FactDescriptorSet descriptors) {
		if (bitmap == null || descriptors == null) {
			return getDefaultValue();
		}

		if (descriptors.containsVariantRecords()) {

			// use the implementation of the factHolders to handle this
			return aggregate(index, bitmap, new MapFactsDescriptorBased(
					descriptors, index, bitmap));
		} else {
			final int amount = bitmap.determineCardinality();

			// determine if we have an even or odd amount
			final boolean even = (amount & 1) == 0;
			int firstPos = (int) Math.floor(amount * 0.5) + (even ? 0 : 1);

			// get the median
			int processedRecords = 0;
			double median = Double.NaN;
			for (final FactDescriptor<?> desc : descriptors) {

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

				final double curValue = desc.getFact();
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
		final IDoubleIterator it = facts.sortedFactsIterator();
		int curPos = 0;
		while (it.hasNext()) {
			if (curPos == firstPos) {
				break;
			}
			it.next();

			curPos++;
		}

		if (even) {
			median = 0.5 * (it.next() + it.next());
		} else {
			median = it.next();
		}

		return median;
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final FactDescriptorSet descriptors, final int timepoint) {
		return aggregate(index, bitmap, descriptors);
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts, final int timepoint) {
		return aggregate(index, bitmap, facts);
	}

	@Override
	public String getName() {
		return name;
	}
}
