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
import net.meisen.dissertation.model.measures.IMathAggregationFunction;
import net.meisen.dissertation.model.measures.IResultsHolder;
import net.meisen.dissertation.model.util.IDoubleIterator;

/**
 * Implementation of the {@code Mode} aggregation function.
 * 
 * @author pmeisen
 * 
 */
public class Mode extends BaseAggregationFunction implements
		ILowAggregationFunction, IDimAggregationFunction,
		IMathAggregationFunction {
	private final static String name = "mode";

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
			int maxAmount = -1;
			double mode = Double.NaN;
			for (final FactDescriptor<?> desc : descriptors) {

				// get the slice and the combined bitmap
				final Slice<?> metaSlice = index.getMetaIndexDimensionSlice(
						desc.getModelId(), desc.getId());
				final Bitmap bmp = bitmap.and(metaSlice.getBitmap());

				// get the amount of records
				final int amount = bmp.determineCardinality();
				if (amount > maxAmount) {
					maxAmount = amount;
					mode = desc.getFact();
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

		return calc(facts.sortedFactsIterator());
	}

	/**
	 * Calculates the mode for a sorted iterator.
	 * 
	 * @param it
	 *            the iterator running in sorted order
	 * 
	 * @return the mode
	 */
	protected double calc(final IDoubleIterator it) {

		// get some helpers to keep track of the last state
		double lastFact = Double.NaN;
		int maxAmount = 0;

		// iterate over the values
		int counter = 0;
		double mode = Double.NaN;
		while (it.hasNext()) {
			final double fact = it.next();

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

	@Override
	public double aggregate(final IResultsHolder results) {
		if (results == null || results.amountOfResults() == 0) {
			return getDefaultValue();
		}

		return calc(results.sortedResultsIterator());
	}
}
