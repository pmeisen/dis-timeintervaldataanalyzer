package net.meisen.dissertation.model.parser.query;

/**
 * Factory to create {@code Query} instances for the specified string.
 * 
 * @author pmeisen
 * 
 */
public interface IQueryFactory {

	/**
	 * Parses the query's string and creates the {@code Query} instance.
	 * 
	 * @param queryString
	 *            the string to be parsed
	 * 
	 * @return the created {@code Query}
	 * 
	 * @see IQuery
	 */
	public IQuery parseQuery(final String queryString);
}
