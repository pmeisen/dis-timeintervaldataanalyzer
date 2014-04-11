package net.meisen.dissertation.impl.parser.query.select;

import net.meisen.dissertation.impl.parser.query.select.evaluator.DescriptorLogicEvaluator;
import net.meisen.dissertation.impl.parser.query.select.evaluator.TimeSeriesEvaluator;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLogicTree;
import net.meisen.dissertation.impl.time.series.TimeSeriesResult;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.dissertation.model.parser.query.IQuery;

public class SelectQuery implements IQuery {

	private final DescriptorLogicTree filter;

	private String modelId;
	private ResultType type;
	private Interval<?> interval;

	public SelectQuery() {
		filter = new DescriptorLogicTree();
	}

	public void setResultType(final ResultType type) {
		this.type = type;
	}

	public ResultType getResultType() {
		return type;
	}

	@Override
	public String toString() {
		return "select " + type + " in " + interval + " filter " + filter;
	}

	public Interval<?> getInterval() {
		return interval;
	}

	public void setInterval(final Interval<?> interval) {
		this.interval = interval;
	}

	public DescriptorLogicTree getFilter() {
		return filter;
	}

	public void optimize() {
		filter.optimize();
	}

	@Override
	public SelectQueryResult evaluate(final TidaModel model) {

		// the result holder
		final SelectQueryResult queryResult = new SelectQueryResult(this);

		// determine the filter results
		final DescriptorLogicEvaluator descriptorEvaluator = new DescriptorLogicEvaluator(
				model);
		final Bitmap filterBitmap = descriptorEvaluator.evaluateTree(filter);
		queryResult.setFilterResult(filterBitmap);

		// determine the IntervalIndexDimensionSlices
		final TimeSeriesEvaluator timeSeriesEvaluator = new TimeSeriesEvaluator(
				model);
		final TimeSeriesResult timeSeriesResult = timeSeriesEvaluator
				.evaluateFilteredInterval(interval, filterBitmap);
		queryResult.setTimeSeriesResult(timeSeriesResult);

		// TODO Go On! now we have to merge...

		return queryResult;
	}

	@Override
	public String getModelId() {
		return modelId;
	}

	/**
	 * Sets the {@code modelId} for the query.
	 * 
	 * @param modelId
	 *            the {@code modelId} of the query
	 */
	public void setModelId(final String modelId) {
		this.modelId = modelId;
	}

}
