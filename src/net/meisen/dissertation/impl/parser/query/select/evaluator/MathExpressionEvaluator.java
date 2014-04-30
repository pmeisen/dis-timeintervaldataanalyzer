package net.meisen.dissertation.impl.parser.query.select.evaluator;

import gnu.trove.impl.Constants;
import gnu.trove.map.hash.TIntDoubleHashMap;
import gnu.trove.set.TIntSet;
import gnu.trove.set.hash.TIntHashSet;

import java.util.Arrays;

import com.google.common.primitives.Doubles;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.indexes.TroveIntIndexedCollection;
import net.meisen.dissertation.impl.parser.query.select.measures.ArithmeticOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorLeaf;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.impl.parser.query.select.measures.IMathTreeElement;
import net.meisen.dissertation.impl.parser.query.select.measures.MathOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.MathOperatorNode;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

public class MathExpressionEvaluator {
	private final Bitmap resultBitmap;
	private final TidaIndex index;
	private final FactDescriptorModelSet facts;

	public MathExpressionEvaluator(final TidaIndex index,
			final Bitmap resultBitmap, final FactDescriptorModelSet facts) {
		this.index = index;
		this.facts = facts;
		this.resultBitmap = resultBitmap;
	}

	public double evaluateMeasure(final DescriptorMathTree measure) {
		final MathOperatorNode root = measure.getRoot();

		// check if the root has children, otherwise there is nothing to do
		final int children = root.amountOfChildren();
		if (children != 1) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1009, root);
		} else {
			return evaluateSingleValueForNode(root.getChild(0));
		}
	}

	protected double evaluateSingleValueForNode(final IMathTreeElement node) {
		if (node instanceof MathOperatorNode) {
			return evaluateSingleValueForNode((MathOperatorNode) node);
		} else if (node instanceof DescriptorLeaf) {
			return evaluateSingleValueForNode((DescriptorLeaf) node);
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1009, node);
		}
	}

	protected double evaluateSingleValueForNode(final MathOperatorNode node) {
		final MathOperator op = node.get();

		if (op.isFunction()) {
			return evaluateFunctionNode(node);
		} else if (op.isOperator()) {

			// make sure we have two children
			if (node.amountOfChildren() != 2) {
				throw new ForwardedRuntimeException(
						QueryEvaluationException.class, 1009, node);
			}

			// apply the arithmetic
			final double res1 = evaluateSingleValueForNode(node.getChild(0));
			final double res2 = evaluateSingleValueForNode(node.getChild(1));
			final ArithmeticOperator operator = op.getOperator();

			return calculate(operator, res1, res2);
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1007, op);
		}
	}

	protected double evaluateSingleValueForNode(final DescriptorLeaf node) {
		throw new ForwardedRuntimeException(QueryEvaluationException.class,
				1008, node.get());
	}

	protected double evaluateFunctionNode(final MathOperatorNode node) {
		final MathOperator op = node.get();

		if (op.isFunction()) {
			final IAggregationFunction func = op.getFunction();

			final int children = node.amountOfChildren();
			if (children == 1) {
				final IMathTreeElement childNode = node.getChild(0);

				if (facts == null || resultBitmap == null) {
					return func.getDefaultValue();
				} else if (childNode instanceof MathOperatorNode) {
					final TIntDoubleHashMap facts = evaluateMathForNode((MathOperatorNode) childNode);
					return func.aggregate(index, resultBitmap, facts.values());
				} else if (childNode instanceof DescriptorLeaf) {
					final DescriptorLeaf descLeaf = (DescriptorLeaf) childNode;
					final String descModelId = descLeaf.get();

					return func.aggregate(index, resultBitmap,
							facts.getDescriptors(descModelId));
				} else {
					throw new ForwardedRuntimeException(
							QueryEvaluationException.class, 1009, node);
				}
			} else {
				throw new ForwardedRuntimeException(
						QueryEvaluationException.class, 1011, node);
			}
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1010, op);
		}
	}

	protected TIntDoubleHashMap evaluateMathForNode(final IMathTreeElement node) {
		if (node instanceof MathOperatorNode) {
			return evaluateMathForNode((MathOperatorNode) node);
		} else if (node instanceof DescriptorLeaf) {
			return evaluateMathForNode((DescriptorLeaf) node);
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1009, node);
		}
	}

	protected TIntDoubleHashMap evaluateMathForNode(final MathOperatorNode node) {
		final MathOperator op = node.get();

		// functions are not allowed here, also 2 children are needed
		if (op.isFunction() || node.amountOfChildren() != 2) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1009, node);
		}

		// apply the arithmetic
		final TIntDoubleHashMap res1 = evaluateMathForNode(node.getChild(0));
		final TIntDoubleHashMap res2 = evaluateMathForNode(node.getChild(1));
		final ArithmeticOperator operator = op.getOperator();

		return calculate(operator, res1, res2);
	}

	protected TIntDoubleHashMap evaluateMathForNode(final DescriptorLeaf node) {
		final String descModelId = node.get();

		// create the result
		final TIntDoubleHashMap res = new TIntDoubleHashMap(
				Constants.DEFAULT_CAPACITY, Constants.DEFAULT_LOAD_FACTOR, -1,
				Double.NaN);

		// set the values we know
		for (final Descriptor<?, ?, ?> desc : facts.getDescriptors(descModelId)) {

			if (desc.isRecordInvariant()) {
				final Slice<?> metaSlice = index.getMetaIndexDimensionSlice(
						descModelId, desc.getId());

				// get the bitmap
				final Bitmap bmp = resultBitmap.and(metaSlice.getBitmap());

				// get the values of the facts
				for (final int id : bmp.getIds()) {
					res.put(id, desc.getFactValue(null));
				}
			} else {
				// TODO add support
				throw new UnsupportedOperationException(
						"Currently not supported!");
			}
		}

		return res;
	}

	protected TIntDoubleHashMap calculate(final ArithmeticOperator operator,
			final TIntDoubleHashMap res1, final TIntDoubleHashMap res2) {

		// create a new hashset for the keys
		final TIntHashSet set = new TIntHashSet();
		set.addAll(res1.keys());
		set.addAll(res2.keys());

		// create the result
		final TIntDoubleHashMap res = new TIntDoubleHashMap(set.size(),
				Constants.DEFAULT_LOAD_FACTOR, -1, Double.NaN);
		for (final int key : set.toArray()) {
			final double value = calculate(operator, res1.get(key),
					res2.get(key));
			res.put(key, value);
		}

		return res;
	}

	protected double calculate(final ArithmeticOperator operator, double res1,
			double res2) {

		// handle unknown values
		if (Double.isNaN(res1) || Double.isNaN(res2)) {
			return Double.NaN;
		}

		// handle known values
		switch (operator) {
		case ADD:
			return res1 + res2;
		case MINUS:
			return res1 - res2;
		case MULTIPLY:
			return res1 * res2;
		case DIVIDE:
			if (res2 == 0.0) {
				return 0.0;
			} else {
				return res1 / res2;
			}
		}

		// if we reached that point throw an exception
		throw new IllegalStateException(
				"A ArithmeticOperator is undefined, please specifiy '" + this
						+ "'.");
	}
}
