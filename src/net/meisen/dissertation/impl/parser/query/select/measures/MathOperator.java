package net.meisen.dissertation.impl.parser.query.select.measures;

import net.meisen.dissertation.model.measures.IAggregationFunction;

public class MathOperator {
	private final IAggregationFunction function;
	private final ArithmeticOperator operator;

	public MathOperator(final ArithmeticOperator op) {
		this.operator = op;
		this.function = null;
	}

	public MathOperator(final IAggregationFunction func) {
		this.operator = null;
		this.function = func;
	}

	@Override
	public String toString() {
		return function != null ? function.toString()
				: (operator != null ? operator.toString() : null);
	}

	public ArithmeticOperator getOperator() {
		return operator;
	}

	public IAggregationFunction getFunction() {
		return function;
	}

	public boolean isFunction() {
		return function != null;
	}

	public boolean isOperator() {
		return operator != null;
	}
}
