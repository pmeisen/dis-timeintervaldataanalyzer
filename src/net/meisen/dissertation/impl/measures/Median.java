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
 * {@code AggregationFunction} to calculate the {@code Median}.
 * 
 * @author pmeisen
 * 
 */
public class Median extends BaseAggregationFunction implements
		ILowAggregationFunction, IDimAggregationFunction,
		IMathAggregationFunction {
	private final static String name = "median";

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts) {
		if (facts == null || facts.amount() == 0) {
			return getDefaultValue();
		} else if (facts.amountOfNonNaN() == 0) {
			return getNaNValue();
		}

		return calc(facts.amountOfNonNaN(), facts.sortedIterator());
	}

	/**
	 * Helper method to calculate the median for the specified iterator.
	 * 
	 * @param amount
	 *            the amount of elements to be iterated
	 * @param it
	 *            the sorted iterator
	 * 
	 * @return the median
	 */
	protected double calc(final int amount, final IDoubleIterator it) {

		// get the middle position
		final boolean even = (amount & 1) == 0;
		final int firstPos = (int) Math.floor(amount * 0.5) + (even ? -1 : 0);

		// calculate the median
		final double median;
		int curPos = 0;
		while (it.hasNext()) {
			if (curPos == firstPos) {
				break;
			}
			it.next();

			curPos++;
		}

		if (even) {
			median = 0.5 * (it.next() + it.next());
		} else {
			median = it.next();
		}

		return median;
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
		if (results == null || results.amount() == 0) {
			return getDefaultValue();
		} else if (results.amountOfNonNaN() == 0) {
			return getNaNValue();
		}

		return calc(results.amountOfNonNaN(), results.sortedIterator());
	}
}
