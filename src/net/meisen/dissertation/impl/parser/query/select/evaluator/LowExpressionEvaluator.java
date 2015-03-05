package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.measures.MapFactsDescriptorBased;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
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
	 * @param groupId
	 *            the group the expression is evaluated for
	 * @param timepoint
	 *            the normalized time-point of the time
	 */
	public LowExpressionEvaluator(final TidaIndex index,
			final Bitmap resultBitmap, final FactDescriptorModelSet facts,
			final String groupId, final int timepoint) {
		super(index, groupId);

		this.facts = facts;
		this.resultBitmap = resultBitmap;
		this.timepoint = timepoint;
	}

	@Override
	protected double applyFunction(final IAggregationFunction func) {
		return applyFunction(func, null);
	}

	@Override
	protected double applyFunction(final IAggregationFunction func,
			final IFactsHolder facts) {

		if (func instanceof ILowAggregationFunction) {

			// check the result of the observers and cancel if asked for
			if (!notifyObservers(timepoint, resultBitmap)) {
				return getCancellationFlag();
			}

			// check the result to be returned
			if (facts == null) {
				return func.getDefaultValue();
			} else {
				return ((ILowAggregationFunction) func).aggregate(getIndex(),
						resultBitmap, facts, timepoint);
			}
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1022, ILowAggregationFunction.class.getSimpleName(),
					func == null ? null : func.getClass().getSimpleName(), func);
		}
	}

	@Override
	protected IFactsHolder getFactsHolder(final String modelId) {
		return new MapFactsDescriptorBased(facts.getDescriptors(modelId),
				getIndex(), resultBitmap);
	}

	@Override
	protected boolean useFunctionDirectly() {
		return facts == null || resultBitmap == null;
	}
}
