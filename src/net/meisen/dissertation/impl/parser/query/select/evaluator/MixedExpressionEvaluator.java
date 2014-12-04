package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.List;

import net.meisen.dissertation.impl.parser.query.select.measures.MathOperatorNode;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.measures.IDimAggregationFunction;

public class MixedExpressionEvaluator extends MathExpressionEvaluator {

	private final DimExpressionEvaluator dimEvaluator;

	public MixedExpressionEvaluator(final TidaIndex index, final long[] bounds,
			final List<TimeMemberRange> ranges, final Bitmap groupBitmap,
			final Bitmap memberBitmap, final FactDescriptorModelSet memberFacts) {
		super(index, bounds, ranges, groupBitmap);

		this.dimEvaluator = new DimExpressionEvaluator(getIndex(),
				memberBitmap, memberFacts);
	}

	@Override
	protected double evaluateFunction(final IAggregationFunction func,
			final MathOperatorNode node) {

		if (func.getDefinedType().equals(IDimAggregationFunction.class)) {
			return dimEvaluator.evaluateMeasure(node);
		} else {
			return super.evaluateFunction(func, node);
		}
	}
}
