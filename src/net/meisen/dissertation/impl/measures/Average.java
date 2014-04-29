package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;

public class Average extends BaseAggregationFunction {
	private final static String name = "average";

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final Iterable<Descriptor<?, ?, ?>> descriptors) {
		int setRecords = bitmap.determineCardinality();

		// if there aren't any values 0.0 is the result
		if (setRecords == 0) {
			return 0.0;
		} else {
			return sum(index, bitmap, descriptors) / setRecords;
		}
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final double[] facts) {
		int setRecords = bitmap.determineCardinality();

		// if there aren't any values 0.0 is the result
		if (facts == null || setRecords == 0) {
			return 0.0;
		} else {
			return sum(facts) / setRecords;
		}
	}

	@Override
	public String getName() {
		return name;
	}
}
