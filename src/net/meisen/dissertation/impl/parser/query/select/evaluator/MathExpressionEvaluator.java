package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.List;

import net.meisen.dissertation.impl.measures.ResultsArrayBased;
import net.meisen.dissertation.impl.parser.query.select.measures.IMathTreeElement;
import net.meisen.dissertation.impl.parser.query.select.measures.MathOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.MathOperatorNode;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.dissertation.model.measures.IMathAggregationFunction;
import net.meisen.dissertation.model.measures.IResultsHolder;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Numbers;

/**
 * Evaluator used to evaluate {@code IMathAggregationFunction} instances used
 * within a query. The evaluator only works with such aggregation-functions.
 * 
 * @author pmeisen
 * 
 */
public class MathExpressionEvaluator extends ExpressionEvaluator {
	private final Bitmap bitmap;
	private final List<TimeMemberRange> ranges;
	private final long[] bounds;

	/**
	 * Constructor specifying the ranges and the filter-bitmap (i.e. combination
	 * of valid-records, filtering and group).
	 * 
	 * @param index
	 *            the {@code TidaIndex}
	 * @param groupId
	 *            the group the expression is evaluated for
	 * @param bounds
	 *            the bounds defined generally
	 * @param ranges
	 *            the ranges to evaluate
	 * @param bitmap
	 *            the filter results (i.e. the combination of valid-records,
	 *            filtering and group)
	 */
	public MathExpressionEvaluator(final TidaIndex index, final String groupId,
			final long[] bounds, final List<TimeMemberRange> ranges,
			final Bitmap bitmap) {
		super(index, groupId);

		this.bounds = bounds;
		this.ranges = ranges;
		this.bitmap = bitmap;
	}

	@Override
	protected double applyFunction(final IAggregationFunction func) {
		throw new IllegalStateException("Should never be used.");
	}

	@Override
	protected boolean useFunctionDirectly() {
		return false;
	}

	@Override
	protected IFactsHolder getFactsHolder(final String modelId) {
		throw new IllegalStateException("Should never be used.");
	}

	@Override
	protected double applyFunction(final IAggregationFunction func,
			final IFactsHolder facts) {
		throw new IllegalStateException("Should never be used.");
	}

	@Override
	protected double evaluateFunctionNode(final MathOperatorNode node)
			throws ForwardedRuntimeException {

		// make sure we have a function with one child
		final MathOperator op = node.get();
		if (!op.isFunction()) {
			return super.evaluateFunctionNode(node);
		} else if (node.amountOfChildren() != 1) {
			return super.evaluateFunctionNode(node);
		}

		// validate the type of the function
		final IAggregationFunction func = op.getFunction();
		return evaluateFunction(func, node);
	}

	/**
	 * Method used to define how to evaluate a specific function. The method
	 * evaluates {@code IMathAggregationFunction} instances only, otherwise the
	 * {@link #evaluateFunctionNode(MathOperatorNode)} of the
	 * base-implementation is called.
	 * 
	 * @param func
	 *            the function to be evaluated
	 * @param node
	 *            the node the function belongs to
	 * 
	 * @return the result
	 * 
	 * @throws ForwardedRuntimeException
	 *             if an error occurs
	 */
	protected double evaluateFunction(final IAggregationFunction func,
			final MathOperatorNode node) throws ForwardedRuntimeException {

		if (func == null) {
			return super.evaluateFunctionNode(node);
		} else if (func.getDefinedType().equals(IMathAggregationFunction.class)) {
			return evaluateMathFunction((IMathAggregationFunction) func,
					node.getChild(0));
		} else {
			return super.evaluateFunctionNode(node);
		}
	}

	/**
	 * Evaluates the {@code IMathAggregationFunction} for the specified
	 * {@code node}. The node must represent a {@code ILowAggregationFunction},
	 * otherwise the used {@code LowExpressionEvaluator} will throw an
	 * exception.
	 * 
	 * @param func
	 *            the {@code IMathAggregationFunction} to be evaluated
	 * @param node
	 *            the child of the function, specifying the
	 *            {@code ILowAggregationFunction} to be used
	 * 
	 * @return the result of the application of the function
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the evaluation fails
	 */
	protected double evaluateMathFunction(final IMathAggregationFunction func,
			final IMathTreeElement node) throws ForwardedRuntimeException {

		final IResultsHolder holder = new ResultsArrayBased();
		for (final TimeMemberRange range : ranges) {
			final long start = Math.max(bounds[0], range.getStart());
			final long end = Math.min(bounds[1], range.getEnd());

			// get the slices and make sure there are any
			final SliceWithDescriptors<?>[] slices = getIndex()
					.getIntervalIndexSlices(start, end);
			if (slices.length == 0) {
				continue;
			}

			// ... iterating over the slices
			int i = Numbers.castToInt(start);
			for (final SliceWithDescriptors<?> slice : slices) {
				final Bitmap bitmap;
				final FactDescriptorModelSet facts;
				if (slice == null) {
					facts = null;
					bitmap = null;
				} else {
					facts = slice.getFactsSet();
					bitmap = this.bitmap == null ? slice.getBitmap()
							: this.bitmap.and(slice.getBitmap());
				}

				// get the result for the slice and add it
				final LowExpressionEvaluator evaluator = new LowExpressionEvaluator(
						getIndex(), bitmap, facts, getGroupId(), i);
				evaluator.addObserver(this);

				/*
				 * Get the result and add it if it's a valid value, otherwise
				 * cancel or don't add it.
				 */
				final double result = evaluator.evaluateMeasure(node);
				if (isCancelled(result)) {
					return getCancellationFlag();
				} else if (!Double.isNaN(result)) {
					holder.add(result);
				}

				i++;
			}
		}

		return func.aggregate(holder);
	}
}
