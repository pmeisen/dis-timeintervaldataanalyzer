package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;

/**
 * Used to calculate the average value of the facts and the amount of records.
 * 
 * @author pmeisen
 * 
 */
public class Mean extends BaseAggregationFunction {
	private final static String name = "mean";

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final FactDescriptorSet descriptors) {
		if (bitmap == null || descriptors == null) {
			return getDefaultValue();
		}

		int setRecords = bitmap.determineCardinality();

		// if there aren't any values 0.0 is the result
		if (setRecords == 0) {
			return getDefaultValue();
		} else {
			return sum(index, bitmap, descriptors) / setRecords;
		}
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final double[] facts) {
		int setRecords = facts.length;

		// if there aren't any values 0.0 is the result
		if (facts == null || setRecords == 0) {
			return getDefaultValue();
		} else {
			return sum(facts) / setRecords;
		}
	}

	@Override
	public String getName() {
		return name;
	}
}
