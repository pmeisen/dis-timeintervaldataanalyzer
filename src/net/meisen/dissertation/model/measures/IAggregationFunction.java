package net.meisen.dissertation.model.measures;

import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

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
	 * The usage-type of the function (i.e. its semantical use) defined by the
	 * query.
	 * 
	 * @param type
	 *            the type, e.g. {@link IMathAggregationFunction},
	 *            {@link IDimAggregationFunction},
	 *            {@link ILowAggregationFunction}
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the type is not supported by the concrete function
	 */
	public void setDefinedType(final Class<? extends IAggregationFunction> type)
			throws ForwardedRuntimeException;

	/**
	 * Get the defined usage-type of the function.
	 * 
	 * @return the usage-type of the function, e.g.
	 *         {@link IMathAggregationFunction}, {@link IDimAggregationFunction}
	 *         , {@link ILowAggregationFunction}
	 */
	public Class<? extends IAggregationFunction> getDefinedType();

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

	/**
	 * Gets the default value used if the result is NaN.
	 * 
	 * @return the NaN value
	 */
	public double getNaNValue();

	/**
	 * Creates a new instance of the function.
	 * 
	 * @return a new instance of {@code this}
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the instance cannot be created
	 */
	public IAggregationFunction create() throws ForwardedRuntimeException;
}
