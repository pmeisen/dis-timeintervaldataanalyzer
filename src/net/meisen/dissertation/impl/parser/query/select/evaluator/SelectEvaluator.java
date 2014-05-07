package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResult;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.data.TidaModel;

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
		final SelectResult queryResult = new SelectResult(query);

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

		// determine the slice
		final TimeSeriesEvaluator timeSeriesEvaluator = new TimeSeriesEvaluator(
				model);
		final TimeSeriesCollection timeSeriesCollection = timeSeriesEvaluator
				.evaluateInterval(query.getInterval(), query.getMeasures(),
						filterResult, groupResult);
		queryResult.setTimeSeriesResult(timeSeriesCollection);

		return queryResult;
	}
}
