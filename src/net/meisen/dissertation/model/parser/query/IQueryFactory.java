package net.meisen.dissertation.model.parser.query;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.exceptions.QueryParsingException;

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
	 * @throws QueryParsingException
	 *             if the parsing fails
	 * 
	 * @see IQuery
	 */
	public IQuery parseQuery(final String queryString)
			throws QueryParsingException;

	public IQueryResult evaluateQuery(final IQuery query)
			throws QueryEvaluationException;
}
