package net.meisen.dissertation.impl.parser.query.select;

import net.meisen.dissertation.impl.parser.query.select.evaluator.DescriptorLogicResult;
import net.meisen.dissertation.impl.parser.query.select.evaluator.GroupResult;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.parser.query.IQueryResult;

/**
 * The result of the evaluation of a {@code SelectQuery}.
 * 
 * @see SelectQuery
 * 
 * @author pmeisen
 * 
 */
public class SelectResult implements IQueryResult {
	private final SelectQuery query;

	private DescriptorLogicResult filterResult;
	private GroupResult groupResult;
	private TimeSeriesCollection timeSeriesCollection;

	/**
	 * Standard constructor which is used to specify the {@code query} this
	 * result is valid for.
	 * 
	 * @param query
	 *            the {@code SelectQuery} this result is valid for
	 */
	public SelectResult(final SelectQuery query) {
		this.query = query;
	}

	/**
	 * Gets the query.
	 * 
	 * @return the query
	 */
	public SelectQuery getQuery() {
		return query;
	}

	/**
	 * Gets the result of the filter specified by the query.
	 * 
	 * @return the result of the filter specified by the query
	 */
	public DescriptorLogicResult getFilterResult() {
		return filterResult;
	}

	/**
	 * Sets the result of the filter specified by the query.
	 * 
	 * @param filterResult
	 *            the result of the filtering
	 */
	public void setFilterResult(final DescriptorLogicResult filterResult) {
		this.filterResult = filterResult;
	}

	/**
	 * Sets the {@code TimeSeries} specified by the query.
	 * 
	 * @param timeSeriesCollection
	 *            the {@code TimeSeries} specified by the query
	 */
	public void setTimeSeriesResult(final TimeSeriesCollection timeSeriesCollection) {
		this.timeSeriesCollection = timeSeriesCollection;
	}

	/**
	 * Gets the determined {@code TimeSeries} specified by the query.
	 * 
	 * @return the determined {@code TimeSeries}
	 */
	public TimeSeriesCollection getTimeSeriesResult() {
		return timeSeriesCollection;
	}

	/**
	 * Gets the result of the group defined by the query.
	 * 
	 * @return the result of the group
	 */
	public GroupResult getGroupResult() {
		return groupResult;
	}

	/**
	 * Sets the result of the group.
	 * 
	 * @param groupResult
	 *            the grouping result
	 */
	public void setGroupResult(final GroupResult groupResult) {
		this.groupResult = groupResult;
	}
}
