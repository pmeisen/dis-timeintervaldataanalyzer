package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.parser.query.select.measures.ArithmeticOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorLeaf;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.impl.parser.query.select.measures.IMathTreeElement;
import net.meisen.dissertation.impl.parser.query.select.measures.MathOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.MathOperatorNode;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

public class MathExpressionEvaluator {
	private final SliceWithDescriptors<?> timeSlice;
	private final Bitmap resultBitmap;
	private final TidaIndex index;

	public MathExpressionEvaluator(final TidaIndex index,
			final SliceWithDescriptors<?> timeSlice, final Bitmap resultBitmap) {
		this.index = index;
		this.timeSlice = timeSlice;
		this.resultBitmap = resultBitmap;
	}

	public double evaluateMeasure(final DescriptorMathTree measure) {
		if (timeSlice == null || resultBitmap == null) {
			return 0.0;
		}

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

				if (childNode instanceof MathOperatorNode) {
					final double[] facts = evaluateMathForNode((MathOperatorNode) childNode);

					return func.aggregate(index, resultBitmap, facts);
				} else if (childNode instanceof DescriptorLeaf) {
					final DescriptorLeaf descLeaf = (DescriptorLeaf) childNode;
					final String descModelId = descLeaf.get();

					return func.aggregate(index, resultBitmap,
							timeSlice.facts(descModelId));
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

	protected double[] evaluateMathForNode(final IMathTreeElement node) {
		if (node instanceof MathOperatorNode) {
			return evaluateMathForNode((MathOperatorNode) node);
		} else if (node instanceof DescriptorLeaf) {
			return evaluateMathForNode((DescriptorLeaf) node);
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1009, node);
		}
	}

	protected double[] evaluateMathForNode(final MathOperatorNode node) {
		final MathOperator op = node.get();

		// functions are not allowed here, also 2 children are needed
		if (op.isFunction() || node.amountOfChildren() != 2) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1009, node);
		}

		// apply the arithmetic
		final double[] res1 = evaluateMathForNode(node.getChild(0));
		final double[] res2 = evaluateMathForNode(node.getChild(1));
		final ArithmeticOperator operator = op.getOperator();

		return calculate(operator, res1, res2);
	}

	protected double[] evaluateMathForNode(final DescriptorLeaf node) {
		final String descModelId = node.get();

		final double[] res = new double[index.getAmountOfRecords()];
		for (final Descriptor<?, ?, ?> desc : timeSlice.facts(descModelId)) {

			if (desc.isRecordInvariant()) {

				final Slice<?> metaSlice = index.getMetaIndexDimensionSlice(
						descModelId, desc.getId());

				// get the bitmap
				final Bitmap bmp = resultBitmap.and(metaSlice.getBitmap());
				for (final int id : bmp.getIds()) {
					res[id] = desc.getFactValue(null);
				}
			} else {
				// TODO add support
				throw new UnsupportedOperationException(
						"Currently not supported!");
			}
		}

		return res;
	}

	protected double[] calculate(final ArithmeticOperator operator,
			double[] res1, double[] res2) {
		final int size = Math.min(res1.length, res2.length);
		final double[] res = new double[size];
		for (int i = 0; i < size; i++) {
			res[i] = calculate(operator, res1[i], res2[i]);
		}

		return res;
	}

	protected double calculate(final ArithmeticOperator operator, double res1,
			double res2) {

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
