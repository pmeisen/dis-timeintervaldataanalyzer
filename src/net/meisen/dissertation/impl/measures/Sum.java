package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;

/**
 * {@code AggregationFunction} to calculate the sum.
 * 
 * @author pmeisen
 * 
 */
public class Sum extends BaseAggregationFunction {
	private final static String name = "sum";

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final FactDescriptorSet descriptors) {
		return sum(index, bitmap, descriptors);
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final double[] facts) {

		if (facts == null) {
			return getDefaultValue();
		} else {
			return sum(facts);
		}
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public double getDefaultValue() {
		return 0.0;
	}
}
