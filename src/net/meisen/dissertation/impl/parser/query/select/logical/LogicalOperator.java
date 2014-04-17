package net.meisen.dissertation.impl.parser.query.select.logical;


/**
 * A {@code LogicalOperator} is used to define logical expressions. The
 * specified {@code LogicalOperators} are complete in the sense of a Boolean
 * algebra.
 * 
 * @author pmeisen
 * 
 */
public enum LogicalOperator {
	/**
	 * Logical {@code AND}, i.e. {@code 1 && 1} is {@code 1}, everything else is
	 * {@code 0}.
	 */
	AND,
	/**
	 * Logical {@code OR}, i.e. {@code 0 && 0} is {@code 0}, everything else is
	 * {@code 1}.
	 */
	OR,
	/**
	 * Logical {@code NOT}, i.e. {@code 0} is {@code 1} and {@code 1} is
	 * {@code 0}.
	 */
	NOT;
}
