package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.measures.MapFactsDescriptorBased;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.measures.IDimAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * Evaluator used to evaluate a measure based on {@code IDimAggregationFunction}
 * .
 * 
 * @author pmeisen
 * 
 */
public class DimExpressionEvaluator extends ExpressionEvaluator {
	private final Bitmap resultBitmap;
	private final FactDescriptorModelSet facts;

	/**
	 * Default constructor to create a {@code DimExpressionEvaluator} for a low
	 * granularity-aggregation.
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

	@Override
	protected double applyFunction(final IAggregationFunction func,
			final IFactsHolder facts) {
		if (func instanceof IDimAggregationFunction) {
			return ((IDimAggregationFunction) func).aggregate(getIndex(),
					resultBitmap, facts);
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1022, IDimAggregationFunction.class.getSimpleName(),
					func == null ? null : func.getClass().getSimpleName(), func);
		}
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
