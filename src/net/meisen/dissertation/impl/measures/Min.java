package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.dissertation.model.util.IDoubleIterator;

/**
 * Used to determine the minimum value of the facts and the amount of records.
 * 
 * @author pmeisen
 * 
 */
public class Min extends BaseAggregationFunction {
	private final static String name = "min";

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final FactDescriptorSet descriptors) {
		if (descriptors == null || descriptors.size() == 0) {
			return getDefaultValue();
		}

		if (descriptors.containsVariantRecords()) {

			// use the implementation of the factHolders to handle this
			return aggregate(index, bitmap, new MapFactsDescriptorBased(
					descriptors, index, bitmap));
		} else {

			// get the first descriptor and get it's value
			final FactDescriptor<?> first = descriptors.first();
			return first.getFact();
		}
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts) {
		if (facts == null || facts.amountOfFacts() == 0) {
			return getDefaultValue();
		}

		double min = Double.MAX_VALUE;
		final IDoubleIterator it = facts.sortedFactsIterator();
		if (it.hasNext()) {
			min = it.next();
		}

		return min;
	}

	@Override
	public String getName() {
		return name;
	}
}
