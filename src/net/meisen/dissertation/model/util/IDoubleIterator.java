package net.meisen.dissertation.model.util;

/**
 * Iterator used to iterate over doubles.
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