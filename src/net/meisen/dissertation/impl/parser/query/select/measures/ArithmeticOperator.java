package net.meisen.dissertation.impl.parser.query.select.measures;

/**
 * A {@code MathOperator} is used to define mathematical expressions. The
 * specified {@code MathOperator}.
 * 
 * @author pmeisen
 * 
 */
public enum ArithmeticOperator {
	/**
	 * A mathematical {@code +}.
	 */
	ADD,
	/**
	 * A mathematical {@code -}.
	 */
	MINUS,
	/**
	 * A mathematical {@code x}.
	 */
	MULTIPLY,
	/**
	 * A mathematical {@code /}.
	 */
	DIVIDE;
}
