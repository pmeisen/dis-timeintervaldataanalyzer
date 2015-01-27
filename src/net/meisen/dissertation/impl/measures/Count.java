package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;
import net.meisen.dissertation.model.measures.IDimAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.dissertation.model.measures.ILowAggregationFunction;
import net.meisen.dissertation.model.measures.IMathAggregationFunction;
import net.meisen.dissertation.model.measures.IResultsHolder;

/**
 * Aggregations method to count the amount of intervals.
 * 
 * @author pmeisen
 * 
 */
public class Count extends BaseAggregationFunction implements
		ILowAggregationFunction, IDimAggregationFunction,
		IMathAggregationFunction {
	private final static String name = "count";

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts, final int timepoint) {
		return aggregate(index, bitmap, facts);
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts) {
		if (bitmap == null || facts == null) {
			return getDefaultValue();
		}

		return bitmap.determineCardinality();
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public double getDefaultValue() {
		return 0.0;
	}

	@Override
	public double aggregate(final IResultsHolder results) {
		if (results == null || results.amountOfResults() == 0) {
			return getDefaultValue();
		}
		return results.amountOfResults();
	}
}
