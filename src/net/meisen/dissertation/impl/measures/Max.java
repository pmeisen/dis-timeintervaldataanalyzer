package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;

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
		final Descriptor<?, ?, ?> last = descriptors.last();
		return last.getFactValue(null);
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final double[] facts) {
		if (facts == null || facts.length == 0) {
			return getDefaultValue();
		}

		double max = facts[0];
		for (int i = 1; i < facts.length; i++) {
			if (facts[i] > max) {
				max = facts[i];
			}
		}

		return max;
	}

	@Override
	public String getName() {
		return name;
	}
}
