package net.meisen.dissertation.model.indexes;

import java.util.Iterator;

/**
 * A collection which is optimized to perform range-queries, i.e. queries which
 * retrieve data between to data-points within the possible data-range.
 * 
 * @author pmeisen
 * 
 */
public interface IRangeQueryOptimized extends IIndexedCollection {

	/**
	 * Gets an iterator to iterate from data on the position {@code start} to
	 * the position {@code end}.
	 * 
	 * @param start
	 *            the start of the iteration
	 * @param end
	 *            the end of the iteration
	 * 
	 * @return the iterator to iterate over the data
	 */
	public Iterator<?> iterateByStartAndEnd(final Object start, final Object end);

	/**
	 * Gets an iterator to iterate from data on the position {@code start} to
	 * the position {@code end}.
	 * 
	 * @param start
	 *            the start of the iteration
	 * @param amount
	 *            the amount of data to iterate over
	 * 
	 * @return the iterator to iterate over the data
	 */
	public Iterator<?> iterateByRange(final Object start, final long amount);

	/**
	 * Gets the objects for the specified range.
	 * 
	 * @param start
	 *            the start (included)
	 * @param end
	 *            the end (included)
	 * 
	 * @return the returned objects added to the index, might include
	 *         {@code null} if data wasn't added yet for the value within the
	 *         range
	 */
	public Object[] getObjectsByStartAndEnd(final Object start, final Object end);

	/**
	 * Gets the objects for the specified range.
	 * 
	 * @param start
	 *            the start (included)
	 * @param amount
	 *            the amount of data to be retrieved from start
	 * 
	 * @return the returned objects added to the index, might include
	 *         {@code null} if data wasn't added yet for the value within the
	 *         range
	 */
	public Object[] getObjectsByRange(final Object start, final long amount);

	/**
	 * Sets the maximum value of the data-range. This method should be called
	 * prior to anything else so that the underlying concrete index
	 * implementation can adapt to the specified size.
	 * 
	 * @param max
	 *            the maximum value of the data-range
	 */
	public void setMaxValue(final long max);

	/**
	 * Gets the minimum value of the data-range.
	 * 
	 * @return the minimum value of the data-range
	 */
	public long getMinValue();

	/**
	 * Gets the maximum value of the data-range.
	 * 
	 * @return the maximum value of the data-range
	 */
	public long getMaxValue();
}
