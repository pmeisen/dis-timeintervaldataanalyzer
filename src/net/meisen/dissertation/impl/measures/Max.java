package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.dissertation.model.util.IDoubleIterator;

/**
 * {@code AggregationFunction} to get the maximum value.
 * 
 * @author pmeisen
 * 
 */
public class Max extends BaseAggregationFunction {
	private final static String name = "max";

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final FactDescriptorSet descriptors) {
		if (descriptors == null || descriptors.size() == 0) {
			return getDefaultValue();
		}

		// get the first descriptor to check if invariants are contained
		if (descriptors.containsVariantRecords()) {
			// TODO support it
			throw new UnsupportedOperationException("Currently not supported!");
		}

		// get the last descriptor and get it's value
		final FactDescriptor<?> last = descriptors.last();
		return last.getFact();
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts) {
		if (facts == null || facts.amountOfFacts() == 0) {
			return getDefaultValue();
		}

		double max = Double.MIN_VALUE;
		final IDoubleIterator it = facts.factsIterator();
		while (it.hasNext()) {
			final double fact = it.next();

			if (fact > max) {
				max = fact;
			}
		}

		return max;
	}

	@Override
	public String getName() {
		return name;
	}
}
