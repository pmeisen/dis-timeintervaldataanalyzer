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
		if (bitmap == null) {
			return getDefaultValue();
		}

		/*
		 * This is the result along the time-axis. It might be calculated when
		 * the time-dimension aggregates, or a time-slice is used. In all of
		 * these cases, we are interested in the number of intervals. Thus, we
		 * just count the cardinality (ignoring any fact-value, i.e. also
		 * Double.NaN values set for some intervals).
		 */
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
		if (results == null || results.amount() == 0) {
			return getDefaultValue();
		}

		/*
		 * This is the aggregation of values according to a
		 * descriptor-dimension. NaN values are the results of the
		 * time-combinations. In that case we want to ignore all the NaN values.
		 */
		return results.amountOfNonNaN();
	}
}
