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
 * Used to determine the minimum value of the facts and the amount of records.
 * 
 * @author pmeisen
 * 
 */
public class Min extends BaseAggregationFunction implements
		ILowAggregationFunction, IDimAggregationFunction,
		IMathAggregationFunction {
	private final static String name = "min";

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts) {
		if (facts == null || bitmap == null) {
			return getDefaultValue();
		}

		return findFirstNotNaN(facts.sortedIterator());
	}

	@Override
	public double getDefaultValue() {
		return Double.NaN;
	}

	@Override
	public double getNaNValue() {
		return Double.NaN;
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
		if (results == null) {
			return getDefaultValue();
		} else {
			return findFirstNotNaN(results.sortedIterator());
		}
	}
}
