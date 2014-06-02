package net.meisen.dissertation.model.parser.query;

import net.meisen.dissertation.model.data.TidaModel;

/**
 * A query instance used to retrieve data from the modeled {@code TidaSystem}.
 * 
 * @author pmeisen
 * 
 */
public interface IQuery {

	/**
	 * Gets the identifier of the model the query should be evaluated for.
	 * 
	 * @return the identifier of the model the query should be evaluated for
	 */
	public String getModelId();

	/**
	 * Sets the {@code modelId} for the query.
	 * 
	 * @param modelId
	 *            the {@code modelId} of the query
	 */
	public void setModelId(final String modelId);

	/**
	 * Evaluates the query against the specified {@code model}.
	 * 
	 * @param model
	 *            the model used for evaluation
	 * 
	 * @return the result of the query
	 */
	public IQueryResult evaluate(final TidaModel model);
}
