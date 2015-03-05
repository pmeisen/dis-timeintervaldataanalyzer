package net.meisen.dissertation.impl.parser.query.select.evaluator;

import java.util.List;

import net.meisen.dissertation.impl.parser.query.select.measures.MathOperatorNode;
import net.meisen.dissertation.model.dimensions.TimeMemberRange;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.measures.IDimAggregationFunction;

/**
 * Evaluator used to evaluate a mixed (i.e. several dimensional functions used
 * on top level) measure.
 * 
 * @author pmeisen
 * 
 */
public class MixedExpressionEvaluator extends MathExpressionEvaluator {
	private final DimExpressionEvaluator dimEvaluator;

	/**
	 * Constructor specifying the needed values.
	 * 
	 * 
	 * @param index
	 *            the {@code TidaIndex}
	 * @param groupId
	 *            the group the expression is evaluated for
	 * @param bounds
	 *            the bounds defined generally
	 * @param ranges
	 *            the ranges to evaluate
	 * @param groupBitmap
	 *            the filter results (i.e. the combination of valid-records,
	 *            filtering and group)
	 * @param memberBitmap
	 *            the combined bitmap of the members the evaluator is used for
	 * @param memberFacts
	 *            the combined facts of the members the evaluator is used for
	 */
	public MixedExpressionEvaluator(final TidaIndex index,
			final String groupId, final long[] bounds,
			final List<TimeMemberRange> ranges, final Bitmap groupBitmap,
			final Bitmap memberBitmap, final FactDescriptorModelSet memberFacts) {
		super(index, groupId, bounds, ranges, groupBitmap);

		this.dimEvaluator = new DimExpressionEvaluator(getIndex(), groupId,
				bounds, ranges, groupBitmap, memberBitmap, memberFacts);
		/*
		 * We don't have to add any observer, because the mixed version will
		 * trigger the notification already by the MathExpressionEvaluator.
		 */
		// this.dimEvaluator.addObserver(this);
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
