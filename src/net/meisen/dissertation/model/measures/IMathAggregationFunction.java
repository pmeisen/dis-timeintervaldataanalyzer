package net.meisen.dissertation.model.measures;

/**
 * An aggregation function which is capable to aggregate facts (e.g. double
 * values).
 * 
 * @author pmeisen
 * 
 */
public interface IMathAggregationFunction extends IAggregationFunction {

	/**
	 * The list of values to be aggregated.
	 * 
	 * @param results
	 *            the facts to be aggregated
	 * 
	 * @return the aggregated value
	 */
	public double aggregate(final IResultsHolder results);
}
