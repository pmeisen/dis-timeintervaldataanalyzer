package net.meisen.dissertation.impl.parser.query.select;

/**
 * Enum definition of the different results possible when querying for data.
 * 
 * @author pmeisen
 * 
 */
public enum ResultType {
	/**
	 * The user expects a time-series as result.
	 */
	TIMESERIES,
	/**
	 * The user expects the records as result.
	 */
	RECORDS;
}
