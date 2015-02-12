package net.meisen.dissertation.model.util;

/**
 * Iterator used to iterate over doubles. {@code Double.NaN} values have to be
 * appended last in any sorted version of the iterator.
 * 
 * @author pmeisen
 * 
 */
public interface IDoubleIterator {

	/**
	 * Is there more?
	 * 
	 * @return {@code true}, if there is more, {@code false} otherwise
	 */
	public boolean hasNext();

	/**
	 * Return the next double.
	 * 
	 * @return the double
	 */
	public double next();

}