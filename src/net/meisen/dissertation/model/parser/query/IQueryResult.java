package net.meisen.dissertation.model.parser.query;

/**
 * The interface marks a result of a query.
 * 
 * @author pmeisen
 * 
 */
public interface IQueryResult {

	/**
	 * Get the identifiers of the result. The method can return an empty array
	 * if no identifiers are collected.
	 * 
	 * @return the identifiers of the result
	 */
	public int[] getCollectedIds();
}
