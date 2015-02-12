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
 * Used to calculate the average value of the facts and the amount of records.
 * 
 * @author pmeisen
 * 
 */
public class Mean extends BaseAggregationFunction implements
		ILowAggregationFunction, IDimAggregationFunction,
		IMathAggregationFunction {
	private final static String name = "mean";

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts) {

		// if there aren't any values the mean is not defined
		if (facts == null || facts.amount() == 0) {
			return getDefaultValue();
		}

		if (facts.amountOfNaN() > 0) {
			return getNaNValue();
		} else {
			return sum(facts) / facts.amountOfNonNaN();
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

		// if there aren't any values the mean is not defined
		if (results == null || results.amount() == 0) {
			return getDefaultValue();
		}

		if (results.amountOfNonNaN() == 0) {
			return getNaNValue();
		} else {
			return sum(results) / results.amountOfNonNaN();
		}
	}

	@Override
	public double getDefaultValue() {
		return 0.0;
	}

	@Override
	public double getNaNValue() {
		return Double.NaN;
	}
}
