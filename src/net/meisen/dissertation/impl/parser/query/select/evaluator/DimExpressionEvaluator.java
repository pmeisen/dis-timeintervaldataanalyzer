package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.impl.measures.MapFactsDescriptorBased;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.measures.IDimAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;

public class DimExpressionEvaluator extends ExpressionEvaluator {
	private final Bitmap resultBitmap;
	private final FactDescriptorModelSet facts;

	/**
	 * Default constructor to create a {@code DimExpressionEvaluator} for a
	 * low granularity-aggregation.
	 * 
	 * @param index
	 *            the index to retrieve the data from
	 * @param resultBitmap
	 *            the resulting bitmap defining which records are selected using
	 *            filtering, grouping and time slicing
	 * @param facts
	 *            the facts associated to the time-slice
	 */
	public DimExpressionEvaluator(final TidaIndex index,
			final Bitmap resultBitmap, final FactDescriptorModelSet facts) {
		super(index);

		this.facts = facts;
		this.resultBitmap = resultBitmap;
	}

	protected double applyFunction(final IAggregationFunction func,
			final FactDescriptorSet facts) {

		if (func instanceof IDimAggregationFunction) {
			return ((IDimAggregationFunction) func).aggregate(getIndex(),
					resultBitmap, facts);
		} else {
			// TODO make it nice
			throw new IllegalStateException("FUCK YOU ALL " + func + " "
					+ func.getClass());
		}
	}

	protected double applyFunction(final IAggregationFunction func,
			final IFactsHolder facts) {
		if (func instanceof IDimAggregationFunction) {
			return ((IDimAggregationFunction) func).aggregate(getIndex(),
					resultBitmap, facts);
		} else {
			// TODO make it nice
			throw new IllegalStateException("FUCK YOU ALL " + func + " "
					+ func.getClass());
		}
	}

	@Override
	protected FactDescriptorSet getFactsSet(final String modelId) {
		return facts.getDescriptors(modelId);
	}
	
	@Override
	protected IFactsHolder getFactsHolder(final String modelId) {
		return new MapFactsDescriptorBased(facts.getDescriptors(modelId),
				getIndex(), resultBitmap);
	}
	
	@Override
	protected boolean useDefaultOfFunction() {
		return facts == null || resultBitmap == null;
	}
}
