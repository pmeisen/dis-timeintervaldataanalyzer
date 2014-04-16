package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.time.series.TimeSeriesResult;
import net.meisen.dissertation.model.data.TidaModel;

public class SelectEvaluator {
	private final TidaModel model;

	public SelectEvaluator(final TidaModel model) {
		this.model = model;
	}

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

		// determine the IntervalIndexDimensionSlices
		final TimeSeriesEvaluator timeSeriesEvaluator = new TimeSeriesEvaluator(
				model);
		final TimeSeriesResult timeSeriesResult = timeSeriesEvaluator
				.evaluateInterval(query.getInterval(), filterResult,
						groupResult);

		queryResult.setTimeSeriesResult(timeSeriesResult);

		// TODO Go On! now we have to merge...

		return queryResult;
	}
}
