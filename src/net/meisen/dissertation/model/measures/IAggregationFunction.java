package net.meisen.dissertation.model.measures;

/**
 * Interface for an aggregation function. A concrete implementation has to be
 * stateless and thread-safe, i.e. the aggregation function has to be executable
 * several times, from several threads, for several parameters from the same
 * instance of the function. Generally the system generates exactly one instance
 * of the concrete {@code AggregationFunction}. Intermediate results are
 * retrieved by the system and passed to the function again if the aggregation
 * is triggered again.
 * 
 * @author pmeisen
 * 
 */
public interface IAggregationFunction {

	/**
	 * Gets the name of the function.
	 * 
	 * @return the name of the function
	 */
	public String getName();

	/**
	 * Gets the default value normally used if no facts are available.
	 * 
	 * @return the default value
	 */
	public double getDefaultValue();
}
