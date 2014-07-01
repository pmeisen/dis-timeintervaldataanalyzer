package net.meisen.dissertation.model.parser.query;

/**
 * A {@code QueryResult} which provides a set of records as result.
 * 
 * @author pmeisen
 * 
 */
public interface IQueryResultSet extends IQueryResult, Iterable<Object[]> {

	/**
	 * Gets the types of a record of the set.
	 * 
	 * @return the types of a record of the set
	 */
	public Class<?>[] getTypes();

	/**
	 * Gets the names of a record of the set.
	 * 
	 * @return the names of a record of the set
	 */
	public String[] getNames();
}
