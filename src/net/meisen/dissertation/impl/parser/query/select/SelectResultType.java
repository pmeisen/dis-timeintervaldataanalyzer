package net.meisen.dissertation.impl.parser.query.select;

/**
 * Enum definition of the different results possible when querying for data
 * using a select-statement.
 * 
 * @author pmeisen
 * 
 */
public enum SelectResultType {
	/**
	 * The user expects a time-series as result.
	 */
	TIMESERIES,
	/**
	 * The user expects the records as result.
	 */
	RECORDS;
}
