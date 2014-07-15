package net.meisen.dissertation.model.parser.query;

import net.meisen.dissertation.exceptions.PermissionException;
import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.exceptions.QueryParsingException;
import net.meisen.dissertation.server.CancellationException;

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
	public <T extends IQuery> T parseQuery(final String queryString)
			throws QueryParsingException;

	/**
	 * Evaluates the query and retrieves the {@code QueryResult}. The evaluation
	 * of the {@code Query} might need additional resources, which can be
	 * specified by a {@code ResourceResolver}. A {@code ResourceResolver} is an
	 * optional argument, i.e. it might be {@code null}. If a
	 * {@code ResourceResolver} is needed but not specified a
	 * {@code QueryEvaluationException} should be thrown.
	 * 
	 * @param query
	 *            the query to be evaluated
	 * @param resolver
	 *            it might be necessary for the evaluation of the {@code Query}
	 *            to retrieve additional resources, those can be retrieved from
	 *            the resolver (might be {@code null})
	 * 
	 * @return the result of the query
	 * 
	 * @throws QueryEvaluationException
	 *             if the evaluation fails
	 * @throws CancellationException
	 *             if the evaluation was cancelled from the client
	 * @throws PermissionException
	 *             if the current user is not allowed to evaluate the query
	 */
	public <T extends IQueryResult> T evaluateQuery(final IQuery query,
			final IResourceResolver resolver) throws QueryEvaluationException,
			CancellationException, PermissionException;
}
