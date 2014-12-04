package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.measures.MapFactsDescriptorBased;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.dissertation.model.measures.ILowAggregationFunction;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * Evaluator used to evaluate low-functions, i.e. instances of
 * {@code ILowAggregationFunction}.
 * 
 * @author pmeisen
 * 
 */
public class LowExpressionEvaluator extends ExpressionEvaluator {
	private final Bitmap resultBitmap;
	private final FactDescriptorModelSet facts;
	private final int timepoint;

	/**
	 * Default constructor to create a {@code LowExpressionEvaluator} for a low
	 * granularity-aggregation.
	 * 
	 * @param index
	 *            the index to retrieve the data from
	 * @param resultBitmap
	 *            the resulting bitmap defining which records are selected using
	 *            filtering, grouping and time slicing
	 * @param facts
	 *            the facts associated to the time-slice
	 * @param timepoint
	 *            the normalized time-point of the time
	 */
	public LowExpressionEvaluator(final TidaIndex index,
			final Bitmap resultBitmap, final FactDescriptorModelSet facts,
			final int timepoint) {
		super(index);

		this.facts = facts;
		this.resultBitmap = resultBitmap;
		this.timepoint = timepoint;
	}

	protected double applyFunction(final IAggregationFunction func,
			final FactDescriptorSet facts) {
		if (func instanceof ILowAggregationFunction) {
			return ((ILowAggregationFunction) func).aggregate(getIndex(),
					resultBitmap, facts, timepoint);
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1022, ILowAggregationFunction.class.getSimpleName(),
					func == null ? null : func.getClass().getSimpleName(), func);
		}
	}

	protected double applyFunction(final IAggregationFunction func,
			final IFactsHolder facts) {
		if (func instanceof ILowAggregationFunction) {
			return ((ILowAggregationFunction) func).aggregate(getIndex(),
					resultBitmap, facts, timepoint);
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1022, ILowAggregationFunction.class.getSimpleName(),
					func == null ? null : func.getClass().getSimpleName(), func);
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
