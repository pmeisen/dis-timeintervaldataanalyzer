package net.meisen.dissertation.model.parser.query;

/**
 * A {@code QueryResult} which returns a single integer value as result.
 * 
 * @author pmeisen
 * 
 */
public interface IQueryResultSingleInteger extends IQueryResult {

	/**
	 * Gets the result of the query.
	 * 
	 * @return the result of the query
	 */
	public int getResult();
}
