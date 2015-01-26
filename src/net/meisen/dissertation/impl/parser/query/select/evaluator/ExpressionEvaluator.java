package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.measures.MapFactsArrayBased;
import net.meisen.dissertation.impl.parser.query.select.measures.ArithmeticOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorLeaf;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.impl.parser.query.select.measures.IMathTreeElement;
import net.meisen.dissertation.impl.parser.query.select.measures.MathOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.MathOperatorNode;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.dissertation.model.util.IIntIterator;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * Evaluator to evaluate the values of a mathematical expression.
 * 
 * @author pmeisen
 * 
 */
public abstract class ExpressionEvaluator {
	private final TidaIndex index;

	/**
	 * Default constructor specifying the index.
	 * 
	 * @param index
	 *            the {@code TidaIndex} to be used
	 */
	public ExpressionEvaluator(final TidaIndex index) {
		this.index = index;
	}

	/**
	 * Gets the index.
	 * 
	 * @return the index to be used
	 */
	protected TidaIndex getIndex() {
		return index;
	}

	/**
	 * Evaluates the aggregated result for the specified {@code measure}.
	 * 
	 * @param measure
	 *            the definition of the measure
	 * 
	 * @return the result
	 */
	public double evaluateMeasure(final DescriptorMathTree measure) {
		final MathOperatorNode root = measure.getRoot();

		// check if the root has children, otherwise there is nothing to do
		final int children = root.amountOfChildren();
		if (children != 1) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1009, root);
		} else {
			return evaluateMeasure(root.getChild(0));
		}
	}

	/**
	 * Evaluates the aggregated result for the specified {@code measure}.
	 * 
	 * @param subTree
	 *            the node, i.e. sub-tree, to evaluate the measure for
	 * 
	 * @return the result
	 */
	public double evaluateMeasure(final IMathTreeElement subTree) {
		return evaluateAggregatedValueForNode(subTree);
	}

	/**
	 * Evaluates an aggregated value for the specified {@code node}. The method
	 * uses the implementations
	 * {@link #evaluateAggregatedValueForNode(DescriptorLeaf)} and
	 * {@link #evaluateAggregatedValueForNode(MathOperatorNode)} depending on
	 * the {@code nodes} type.
	 * 
	 * @param node
	 *            the node to evaluate
	 * 
	 * @return the aggregated value
	 */
	protected double evaluateAggregatedValueForNode(final IMathTreeElement node) {
		if (node instanceof MathOperatorNode) {
			return evaluateAggregatedValueForNode((MathOperatorNode) node);
		} else if (node instanceof DescriptorLeaf) {
			return evaluateAggregatedValueForNode((DescriptorLeaf) node);
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1009, node);
		}
	}

	/**
	 * Evaluates an aggregated value for the specified {@code node}.
	 * 
	 * @param node
	 *            the node to evaluate
	 * 
	 * @return the aggregated value
	 */
	protected double evaluateAggregatedValueForNode(final MathOperatorNode node) {
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
			final double res1 = evaluateAggregatedValueForNode(node.getChild(0));
			final double res2 = evaluateAggregatedValueForNode(node.getChild(1));
			final ArithmeticOperator operator = op.getOperator();

			return calculate(operator, res1, res2);
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1007, op);
		}
	}

	/**
	 * Evaluates an aggregated value for the specified {@code node}. The method
	 * is just for safety purposes, i.e. if it occurs to happen that this method
	 * is called an exception is thrown.
	 * 
	 * @param node
	 *            the node to evaluate
	 * 
	 * @return the aggregated value
	 * 
	 * @throws ForwardedRuntimeException
	 *             always
	 */
	protected double evaluateAggregatedValueForNode(final DescriptorLeaf node)
			throws ForwardedRuntimeException {
		throw new ForwardedRuntimeException(QueryEvaluationException.class,
				1008, node.get());
	}

	/**
	 * Evaluates the {@code MathOperatorNode}, which must define a function,
	 * i.e. {@link MathOperator#isFunction()} must return {@code true}.
	 * Otherwise an exception is thrown.
	 * 
	 * @param node
	 *            the node to evaluate
	 * 
	 * @return the aggregated value
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the {@code node} does not specify a function
	 */
	protected double evaluateFunctionNode(final MathOperatorNode node)
			throws ForwardedRuntimeException {
		final MathOperator op = node.get();

		if (op.isFunction()) {
			final IAggregationFunction func = op.getFunction();

			final int children = node.amountOfChildren();
			if (children == 1) {
				final IMathTreeElement childNode = node.getChild(0);

				if (useDefaultOfFunction()) {
					return func.getDefaultValue();
				} else if (childNode instanceof MathOperatorNode) {
					final IFactsHolder facts = evaluateMathForNode((MathOperatorNode) childNode);
					return applyFunction(func, facts);
				} else if (childNode instanceof DescriptorLeaf) {
					final DescriptorLeaf descLeaf = (DescriptorLeaf) childNode;
					return applyFunction(func, getFactsSet(descLeaf.get()));
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

	/**
	 * Method used to check if the default value of the function should be used
	 * instead of a further processing.
	 * 
	 * @return {@code true} if the default value should be used, otherwise
	 *         {@code false}
	 */
	protected abstract boolean useDefaultOfFunction();

	/**
	 * Gets the {@code FactDescriptorSet} for the specified
	 * {@code DescriptorModel} specified by the {@code modelId}.
	 * 
	 * @param modelId
	 *            the {@code DescriptorModel} to get the
	 *            {@code FactDescriptorSet} for
	 * 
	 * @return the {@code FactDescriptorSet}
	 */
	protected abstract FactDescriptorSet getFactsSet(final String modelId);

	/**
	 * Gets the {@code FactsHolder} for the specified {@code DescriptorModel}
	 * specified by the {@code modelId}.
	 * 
	 * @param modelId
	 *            the {@code DescriptorModel} to get the {@code FactsHolder} for
	 * 
	 * @return the {@code FactsHolder}
	 */
	protected abstract IFactsHolder getFactsHolder(final String modelId);

	/**
	 * Applies the function to the specified {@code facts}.
	 * 
	 * @param func
	 *            the function to be applied
	 * @param facts
	 *            the facts
	 * 
	 * @return the result
	 */
	protected abstract double applyFunction(final IAggregationFunction func,
			final IFactsHolder facts);

	/**
	 * Applies the function to the specified {@code facts}.
	 * 
	 * @param func
	 *            the function to be applied
	 * @param facts
	 *            the facts
	 * 
	 * @return the result
	 */
	protected abstract double applyFunction(final IAggregationFunction func,
			final FactDescriptorSet facts);

	/**
	 * Evaluates the mathematical result for the specified
	 * {@code MathOperatorNode}. Depending on the type of the node the
	 * {@link #evaluateMathForNode(DescriptorLeaf)} or
	 * {@link #evaluateMathForNode(MathOperatorNode)} is called.
	 * 
	 * @param node
	 *            the node to be evaluated
	 * 
	 * @return the result of the evaluation, which can be seen as an array
	 *         containing the values for each record
	 */
	protected IFactsHolder evaluateMathForNode(final IMathTreeElement node) {
		if (node instanceof MathOperatorNode) {
			return evaluateMathForNode((MathOperatorNode) node);
		} else if (node instanceof DescriptorLeaf) {
			return evaluateMathForNode((DescriptorLeaf) node);
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1009, node);
		}
	}

	/**
	 * Evaluates the mathematical result for the specified
	 * {@code MathOperatorNode}. The node must define a {@code operator}, i.e.
	 * {@link MathOperator#isOperator()} must return {@code true}. Otherwise an
	 * exception is thrown.
	 * 
	 * @param node
	 *            the node to be evaluated
	 * 
	 * @return the result of the evaluation, which can be seen as an array
	 *         containing the values for each record
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the amount of children is unequal to two or if the
	 *             {@code MathOperatorNode} does not define a {@code operator}
	 */
	protected IFactsHolder evaluateMathForNode(final MathOperatorNode node)
			throws ForwardedRuntimeException {
		final MathOperator op = node.get();

		// functions are not allowed here, also 2 children are needed
		if (op.isFunction() || node.amountOfChildren() != 2) {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1009, node);
		}

		// apply the arithmetic
		final IFactsHolder res1 = evaluateMathForNode(node.getChild(0));
		final IFactsHolder res2 = evaluateMathForNode(node.getChild(1));
		final ArithmeticOperator operator = op.getOperator();

		return calculate(operator, res1, res2);
	}

	/**
	 * Evaluates the mathematical result of a {@code DescriptorLeaf}. Generally
	 * the result is an array (not aggregated yet) with all the values specified
	 * for the descriptor.
	 * 
	 * @param node
	 *            the definition of the descriptor to retrieve the data for
	 * 
	 * @return the unaggregated single values of each record of the descriptor
	 */
	protected IFactsHolder evaluateMathForNode(final DescriptorLeaf node) {
		return getFactsHolder(node.get());
	}

	/**
	 * Calculates the result when applying the specified {@code operator} to the
	 * elements of {@code res1} and {@code res2}. The two result maps
	 * {@code res1} and {@code res2} must be equal considering their
	 * {@code size} and {@code keys}, i.e.
	 * {@code Arrays.equals(res1.keys(), res2.keys)} must be {@code true}.
	 * Otherwise the result will be invalid.
	 * 
	 * @param operator
	 *            the operator to be applied
	 * @param res1
	 *            the left operands
	 * @param res2
	 *            the right operands
	 * 
	 * @return the result
	 */
	protected IFactsHolder calculate(final ArithmeticOperator operator,
			final IFactsHolder res1, final IFactsHolder res2) {
		assert res1.amountOfFacts() == res2.amountOfFacts();

		// create the result, res1 and res2 are both of the same size
		final MapFactsArrayBased res = new MapFactsArrayBased(
				res1.amountOfFacts());

		// set the values within the map
		final IIntIterator it = res1.recordIdsIterator();
		while (it.hasNext()) {
			final int key = it.next();

			final double value = calculate(operator, res1.getFactOfRecord(key),
					res2.getFactOfRecord(key));
			res.set(key, value);
		}

		return res;
	}

	/**
	 * Calculates the result when applying the specified {@code operator} to the
	 * {@code res1} and {@code res2}.
	 * 
	 * @param operator
	 *            the operator to be applied
	 * @param res1
	 *            the left operand
	 * @param res2
	 *            the right operand
	 * 
	 * @return the result
	 */
	protected double calculate(final ArithmeticOperator operator, double res1,
			double res2) {

		// handle unknown values
		if (Double.isNaN(res1) || Double.isNaN(res2)) {
			return Double.NaN;
		}

		// handle known values
		return operator.apply(res1, res2);
	}
}
