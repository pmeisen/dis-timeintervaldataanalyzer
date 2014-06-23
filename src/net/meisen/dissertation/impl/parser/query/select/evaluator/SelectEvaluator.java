package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.parser.query.select.ResultType;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResult;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * An evaluator used to evaluate select queries.
 * 
 * @author pmeisen
 * 
 */
public class SelectEvaluator {
	private final TidaModel model;

	/**
	 * Standard constructor, initializes the evaluator to be used with the
	 * specified {@code model}.
	 * 
	 * @param model
	 *            the model to be used with the evaluator
	 */
	public SelectEvaluator(final TidaModel model) {
		this.model = model;
	}

	/**
	 * Evaluates the specified {@code SelectQuery}.
	 * 
	 * @param query
	 *            the query to be evaluated
	 * @return the result of the query
	 */
	public SelectResult evaluate(final SelectQuery query) {

		// the result holder
		final SelectResult queryResult = createSelectResult(query);

		// determine the filter results
		final DescriptorLogicEvaluator descriptorEvaluator = new DescriptorLogicEvaluator(
				model);
		final DescriptorLogicResult filterResult = descriptorEvaluator
				.evaluateTree(query.getFilter());
		queryResult.setFilterResult(filterResult);

		// determine the different groups
		final GroupEvaluator groupEvaluator = new GroupEvaluator(model);
		final GroupResult groupResult = groupEvaluator
				.evaluateGroupExpression(query.getGroup());
		queryResult.setGroupResult(groupResult);

		// set the valid records
		final Bitmap validRecords = model.getValidRecords();
		queryResult.setValidRecords(validRecords);

		// determine the result
		queryResult.determineResult(model);

		return queryResult;
	}

	/**
	 * Factory method used to create the right {@code SelectQuery} instance
	 * depending on the {@code ResultType} of the query
	 * 
	 * @param query
	 *            the {@code SelectQuery} the result will be based one
	 * 
	 * @return the created {@code SelectResult}
	 * 
	 * @see SelectQuery#getResultType()
	 */
	protected SelectResult createSelectResult(final SelectQuery query) {
		final ResultType resultType = query.getResultType();

		if (ResultType.TIMESERIES.equals(resultType)) {
			return new SelectResultTimeSeries(query);
		} else if (ResultType.RECORDS.equals(resultType)) {
			return new SelectResultRecords(query);
		} else {
			throw new ForwardedRuntimeException(QueryEvaluationException.class,
					1018, resultType == null ? null : resultType.toString());
		}
	}
}
