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
 * {@code AggregationFunction} to calculate the sum.
 * 
 * @author pmeisen
 * 
 */
public class Sum extends BaseAggregationFunction implements
		ILowAggregationFunction, IDimAggregationFunction,
		IMathAggregationFunction {
	private final static String name = "sum";

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts) {
		if (facts == null) {
			return getDefaultValue();
		} else {
			return sum(facts);
		}
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
		return sum(results);
	}
}
