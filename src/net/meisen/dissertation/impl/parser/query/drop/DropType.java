package net.meisen.dissertation.impl.parser.query.drop;

/**
 * The types which can be dropped.
 * 
 * @author pmeisen
 * 
 */
public enum DropType {
	/**
	 * Type specifying that a user is dropped.
	 */
	USER,
	/**
	 * Type specifying that a role is dropped.
	 */
	ROLE,
	/**
	 * Type specifying that a model is dropped.
	 */
	MODEL;
}
