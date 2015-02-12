package net.meisen.dissertation.model.measures;

import net.meisen.dissertation.model.util.IDoubleIterator;

/**
 * A holder of double values.
 * 
 * @author pmeisen
 * 
 */
public interface IDoubleHolder {

	/**
	 * Gets the amount of doubles contained in the holder.
	 * 
	 * @return the amount of doubles
	 */
	public int amount();

	/**
	 * The amount of non-NaN doubles.
	 * 
	 * @return amount of non-NaN doubles
	 */
	public int amountOfNonNaN();

	/**
	 * The amount of NaN doubles.
	 * 
	 * @return amount of NaN doubles
	 */
	public int amountOfNaN();

	/**
	 * Gets an iterator to iterate over the doubles. The order is not defined
	 * and can be of any order.
	 * 
	 * @param excludeNaN
	 *            {@code true} to exclude any {@code Double.NaN} values,
	 *            otherwise {@code false}
	 * 
	 * @return an iterator to iterate over the doubles
	 */
	public IDoubleIterator iterator(final boolean excludeNaN);

	/**
	 * Gets an iterator to iterate over the sorted doubles, {@code Double.NaN}
	 * are always appended to be last.
	 * 
	 * @return an iterator to iterate over the sorted doubles
	 */
	public IDoubleIterator sortedIterator();

	/**
	 * Gets an iterator to iterate over the sorted doubles (descending),
	 * {@code Double.NaN} are always appended to be last.
	 * 
	 * @return an iterator to iterate over the sorted doubles (descending)
	 */
	public IDoubleIterator descSortedIterator();
}
