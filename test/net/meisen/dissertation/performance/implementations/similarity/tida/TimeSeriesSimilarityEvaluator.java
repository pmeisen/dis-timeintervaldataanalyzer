package net.meisen.dissertation.performance.implementations.similarity.tida;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import net.meisen.dissertation.impl.parser.query.BaseIntervalValue;
import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.SelectResult;
import net.meisen.dissertation.impl.parser.query.select.SelectResultTimeSeries;
import net.meisen.dissertation.impl.parser.query.select.evaluator.ExpressionEvaluator;
import net.meisen.dissertation.impl.parser.query.select.evaluator.SelectEvaluator;
import net.meisen.dissertation.impl.parser.query.select.evaluator.TimeSeriesEvaluator;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.impl.time.series.TimeSeries;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.time.mapper.BaseMapper;

public class TimeSeriesSimilarityEvaluator extends TimeSeriesEvaluator {
	private final SelectEvaluator evaluator;

	private SelectResultTimeSeries templateResult = null;

	public TimeSeriesSimilarityEvaluator(final TidaModel model) {
		super(model);

		this.evaluator = new SelectEvaluator(model);
	}

	@Override
	protected TimeSeriesCollection createTimeSeriesResult(Object startPoint,
			boolean startInclusive, int amountOfGranules) {

		// create the result
		final TimeSeriesSimilarityCollection result = new TimeSeriesSimilarityCollection(
				amountOfGranules, startPoint.getClass(), indexFactory);
		result.setLabels(index, startPoint, startInclusive);

		return result;
	}

	@Override
	protected TimeSeriesCollection createTimeSeriesResult(
			final Set<TimeLevelMember> members) {
		final TimeSeriesSimilarityCollection result = new TimeSeriesSimilarityCollection(
				members.size(), String.class, indexFactory);
		result.setLabels(members);

		return result;
	}

	@Override
	protected boolean fillTimeSeries(final String groupId,
			final int timeSeriesPos, final ExpressionEvaluator evaluator,
			final TimeSeriesCollection tsc,
			final Iterable<DescriptorMathTree> it) {
		final TimeSeriesSimilarityCollection tssc = (TimeSeriesSimilarityCollection) tsc;

		// calculate each measure
		for (final DescriptorMathTree measure : it) {

			// get the value and check if it's useful
			final double value = evaluator.evaluateMeasure(measure);

			// get or create the series
			final String tsId = createTimeSeriesId(groupId, measure);
			final TimeSeries timeSeries = getTimeSeries(tssc, tsId);

			// get the template instance
			final TimeSeries templateSeries = getTimeSeries(
					templateResult.getTimeSeriesResult(), tsId);
			final double templateValue = templateSeries.getValue(timeSeriesPos);

			// calculate the distance
			final double dist = Math.abs(templateValue - value);
			final double totalDist = dist + tssc.getDistance();

			// get the value of the series
			tssc.setDistance(totalDist);
			timeSeries.setValue(timeSeriesPos, value);
		}

		return true;
	}

	public List<TimeSeriesSimilarityCollection> evaluateSimilarity(
			final SelectQuery templateQuery) {

		// make sure it's not currently processing
		if (this.templateResult != null) {

			// TODO
			throw new IllegalStateException("Cannot run several times");
		}

		// evaluate the query to get the template
		final SelectResult result = evaluator.evaluate(templateQuery);
		if (result instanceof SelectResultTimeSeries == false) {
			// TODO
			throw new IllegalStateException("Must be a TimeSeries");
		}
		this.templateResult = (SelectResultTimeSeries) result;

		// get the iterator to iterate over the different parts of the time-line
		final BaseMapper<?> mapper = intervalModel.getTimelineMapper();
		final Iterator<long[]> it = mapper
				.createTimelinePartitionIterator(templateQuery.getInterval());

		// evaluate each partition
		final List<TimeSeriesSimilarityCollection> results = new ArrayList<TimeSeriesSimilarityCollection>();
		while (it.hasNext()) {
			final long[] partition = it.next();

			// create the query
			final SelectQuery partitionQuery = new SelectQuery();
			partitionQuery.set(templateQuery);

			// create the interval for the query
			@SuppressWarnings({ "rawtypes", "unchecked" })
			final Interval<?> interval = new Interval(
					BaseIntervalValue.createVal(mapper.resolve(partition[0])),
					BaseIntervalValue.createVal(mapper.resolve(partition[1])));
			partitionQuery.setInterval(interval);

			final TimeSeriesSimilarityCollection res = (TimeSeriesSimilarityCollection) evaluateInterval(
					partitionQuery, templateResult);
			if (res != null) {
				results.add(res);
			}
		}

		// reset the evaluator
		reset();

		// return the results
		return results;
	}

	protected void reset() {
		this.templateResult = null;
	}
}
