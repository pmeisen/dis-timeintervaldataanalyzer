package net.meisen.dissertation.impl.parser.query.select.evaluator;

import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.time.series.TimeSeriesResult;
import net.meisen.dissertation.model.parser.query.IQueryResult;

public class SelectResult implements IQueryResult {
	private final SelectQuery query;

	private DescriptorLogicResult filterResult;
	private GroupResult groupResult;
	private TimeSeriesResult timeSeriesResult;

	public SelectResult(final SelectQuery query) {
		this.query = query;
	}

	public DescriptorLogicResult getFilterResult() {
		return filterResult;
	}

	public void setFilterResult(final DescriptorLogicResult filterResult) {
		this.filterResult = filterResult;
	}

	public SelectQuery getQuery() {
		return query;
	}

	public void setTimeSeriesResult(final TimeSeriesResult timeSeriesResult) {
		this.timeSeriesResult = timeSeriesResult;
	}

	public TimeSeriesResult getTimeSeriesResult() {
		return timeSeriesResult;
	}

	public GroupResult getGroupResult() {
		return groupResult;
	}

	public void setGroupResult(GroupResult groupResult) {
		this.groupResult = groupResult;
	}
}
