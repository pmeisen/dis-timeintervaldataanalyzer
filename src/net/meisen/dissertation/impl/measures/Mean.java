package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
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
			final FactDescriptorSet descriptors) {
		if (bitmap == null || descriptors == null) {
			return getDefaultValue();
		}

		final int setRecords = bitmap.determineCardinality();

		// if there aren't any values 0.0 is the result
		if (setRecords == 0) {
			return getDefaultValue();
		} else {
			return sum(index, bitmap, descriptors) / setRecords;
		}
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final IFactsHolder facts) {

		// if there aren't any values the mean is not defined
		if (facts == null) {
			return getDefaultValue();
		}

		final int setRecords = facts.amountOfFacts();
		if (setRecords == 0) {
			return getDefaultValue();
		} else {
			return sum(facts) / setRecords;
		}
	}

	@Override
	public double aggregate(final TidaIndex index, final Bitmap bitmap,
			final FactDescriptorSet descriptors, final int timepoint) {
		return aggregate(index, bitmap, descriptors);
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
		if (results == null) {
			return getDefaultValue();
		}

		final int amount = results.amountOfResults();
		if (amount == 0) {
			return getDefaultValue();
		} else {
			return sum(results) / amount;
		}
	}
}
