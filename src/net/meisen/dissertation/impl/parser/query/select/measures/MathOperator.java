package net.meisen.dissertation.impl.parser.query.select.measures;

import net.meisen.dissertation.model.measures.IAggregationFunction;

/**
 * A {@code MathOperator} represent a mathematical operation, which might be a
 * arithmetic operator (e.g. +) or a aggregation function (e.g. count).
 * 
 * @author pmeisen
 * 
 * @see IAggregationFunction
 * @see ArithmeticOperator
 * 
 */
public class MathOperator {
	private final IAggregationFunction function;
	private final ArithmeticOperator operator;

	/**
	 * Constructor used if the operator represents a {@code ArithmeticOperator}.
	 * 
	 * @param op
	 *            the {@code ArithmeticOperator} {@code this} stands for
	 */
	public MathOperator(final ArithmeticOperator op) {
		this.operator = op;
		this.function = null;
	}

	/**
	 * Constructor used if the operator represents an
	 * {@code AggregationFunction} .
	 * 
	 * @param func
	 *            the {@code AggregationFunction} {@code this} stands for
	 */
	public MathOperator(final IAggregationFunction func) {
		this.operator = null;
		this.function = func;
	}

	@Override
	public String toString() {
		return function != null ? function.toString()
				: (operator != null ? operator.toString() : null);
	}

	/**
	 * Gets the {@code ArithmeticOperator} defined for the operator. The method
	 * will return {@code null} if no operator is defined and instead an
	 * {@code AggregationFunction} is specified.
	 * 
	 * @return the {@code ArithmeticOperator} defined for the operator
	 */
	public ArithmeticOperator getOperator() {
		return operator;
	}

	/**
	 * Gets the {@code AggregationFunction} defined for the operator. The method
	 * will return {@code null} if no function is defined and instead an
	 * {@code ArithmeticOperator} is specified.
	 * 
	 * @return the {@code AggregationFunction} defined for the operator
	 */
	public IAggregationFunction getFunction() {
		return function;
	}

	/**
	 * Checks if the operator represents an {@code AggregationFunction} and
	 * returns {@code true} in that case. Otherwise {@code false} is returned.
	 * 
	 * @return {@code true} if the operator represents a
	 *         {@code AggregationFunction}, otherwise {@code false}
	 */
	public boolean isFunction() {
		return function != null;
	}

	/**
	 * Checks if the operator represents an {@code ArithmeticOperator} and
	 * returns {@code true} in that case. Otherwise {@code false} is returned.
	 * 
	 * @return {@code true} if the operator represents a
	 *         {@code ArithmeticOperator}, otherwise {@code false}
	 */
	public boolean isOperator() {
		return operator != null;
	}
}
