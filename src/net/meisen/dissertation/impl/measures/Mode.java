package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;

/**
 * Implementation of the {@code Mode} aggregation function.
 * 
 * @author pmeisen
 * 
 */
public class Mode extends BaseAggregationFunction {
	private final static String name = "mode";

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
			int maxAmount = -1;
			double mode = Double.NaN;
			for (final Descriptor<?, ?, ?> desc : descriptors) {

				// get the slice and the combined bitmap
				final Slice<?> metaSlice = index.getMetaIndexDimensionSlice(
						desc.getModelId(), desc.getId());
				final Bitmap bmp = bitmap.and(metaSlice.getBitmap());

				// get the amount of records
				final int amount = bmp.determineCardinality();
				if (amount > maxAmount) {
					maxAmount = amount;
					mode = desc.getFactValue(null);
				} else if (amount == maxAmount) {
					mode = Double.NaN;
				}
			}

			return mode;
		}
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts) {

		if (bitmap == null || facts == null || facts.amountOfFacts() == 0) {
			return getDefaultValue();
		}

		// get some helpers to keep track of the last state
		double lastFact = Double.NaN;
		int maxAmount = 0;

		// iterate over the values
		int counter = 0;
		double mode = Double.NaN;
		for (final double fact : facts.sortedFacts()) {
			if (lastFact == fact) {
				counter++;
			} else if (counter > maxAmount) {
				maxAmount = counter;
				mode = lastFact;
				counter = 1;
			} else if (counter == maxAmount) {
				mode = Double.NaN;
				counter = 1;
			} else {
				counter = 1;
			}

			lastFact = fact;
		}

		// test the last value
		if (counter > maxAmount) {
			mode = lastFact;
		} else if (counter == maxAmount) {
			mode = Double.NaN;
		}

		return mode;
	}

	@Override
	public String getName() {
		return name;
	}
}
