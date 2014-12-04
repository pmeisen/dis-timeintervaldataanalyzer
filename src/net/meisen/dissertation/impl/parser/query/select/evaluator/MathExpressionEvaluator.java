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
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.dissertation.model.measures.IMathAggregationFunction;
import net.meisen.dissertation.model.measures.IResultsHolder;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Numbers;

public class MathExpressionEvaluator extends ExpressionEvaluator {

	private final Bitmap bitmap;
	private final List<TimeMemberRange> ranges;
	private final long[] bounds;

	public MathExpressionEvaluator(final TidaIndex index, final long[] bounds,
			final List<TimeMemberRange> ranges, final Bitmap bitmap) {
		super(index);

		this.bounds = bounds;
		this.ranges = ranges;
		this.bitmap = bitmap;
	}

	@Override
	protected boolean useDefaultOfFunction() {
		return false;
	}

	@Override
	protected FactDescriptorSet getFactsSet(final String modelId) {
		throw new IllegalStateException("Should never be used.");
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
	protected double applyFunction(final IAggregationFunction func,
			final FactDescriptorSet facts) {
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

	protected double evaluateFunction(final IAggregationFunction func,
			final MathOperatorNode node) {

		if (func == null) {
			return super.evaluateFunctionNode(node);
		} else if (func.getDefinedType().equals(IMathAggregationFunction.class)) {
			return evaluateLowFunctionNode((IMathAggregationFunction) func,
					node.getChild(0));
		} else {
			return super.evaluateFunctionNode(node);
		}
	}

	protected double evaluateLowFunctionNode(
			final IMathAggregationFunction func, final IMathTreeElement node)
			throws ForwardedRuntimeException {

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
						getIndex(), bitmap, facts, i);

				// get the result and add it if it's a valid value
				final double result = evaluator.evaluateMeasure(node);
				if (Double.NaN != result) {
					holder.add(evaluator.evaluateMeasure(node));
				}

				i++;
			}
		}

		return func.aggregate(holder);
	}
}
