package net.meisen.dissertation.performance.implementations;

import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;

public interface IImplementation {

	public TimeSeriesCollection run(final String query);
	
	public TimeSeriesCollection run(final SelectQuery query);
}