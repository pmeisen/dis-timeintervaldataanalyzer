package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;
import net.meisen.dissertation.model.measures.IDimAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.dissertation.model.measures.ILowAggregationFunction;
import net.meisen.dissertation.model.measures.IMathAggregationFunction;
import net.meisen.dissertation.model.measures.IResultsHolder;
import net.meisen.dissertation.model.util.IDoubleIterator;

/**
 * {@code AggregationFunction} to get the maximum value.
 * 
 * @author pmeisen
 * 
 */
public class Max extends BaseAggregationFunction implements
		ILowAggregationFunction, IDimAggregationFunction,
		IMathAggregationFunction {
	private final static String name = "max";

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts) {
		if (facts == null || bitmap == null || facts.amountOfFacts() == 0) {
			return getDefaultValue();
		}

		double max = Double.MIN_VALUE;
		final IDoubleIterator it = facts.descSortedFactsIterator();
		if (it.hasNext()) {
			max = it.next();
		}

		return max;
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
		if (results == null || results.amountOfResults() == 0) {
			return getDefaultValue();
		} else {
			return results.descSortedResultsIterator().next();
		}
	}
}
