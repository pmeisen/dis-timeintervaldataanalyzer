package net.meisen.dissertation.performance.implementations.similarity;

import java.util.Map;

/**
 * Interface used to change the {@code Calculator} used by an {@code EventTable}
 * .
 * 
 * @author pmeisen
 * 
 */
public interface IValueCalculator {

	/**
	 * The initializing value.
	 * 
	 * @return the value used for initialization.
	 */
	public double getInitValue();

	/**
	 * The default value used.
	 * 
	 * @return the default value
	 */
	public double getDefaultValue();

	/**
	 * By default a counter is implemented.
	 * 
	 * @param curValue
	 *            the old value
	 * @param record
	 *            the record to calculate the value for (measure)
	 * @return the calculated measure
	 */
	public double calcValue(final double curValue,
			final Map<String, Object> record);
}
