package net.meisen.dissertation.impl.parser.query.select;

import net.meisen.dissertation.impl.time.series.TimeSeriesResult;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.parser.query.IQueryResult;

public class SelectQueryResult implements IQueryResult {
	private final SelectQuery query;

	private Bitmap filterResult;
	private TimeSeriesResult timeSeriesResult;

	public SelectQueryResult(final SelectQuery query) {
		this.query = query;
	}

	public Bitmap getFilterResult() {
		return filterResult;
	}

	public void setFilterResult(final Bitmap filterResult) {
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
}
