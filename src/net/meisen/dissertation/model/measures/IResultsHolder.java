package net.meisen.dissertation.model.measures;

import net.meisen.dissertation.model.util.IDoubleIterator;

/**
 * A holder of intermediate results (i.e. facts calculated), which are not
 * connected to any record.
 * 
 * @author pmeisen
 * 
 */
public interface IResultsHolder {

	/**
	 * Gets the amount of results contained in the holder.
	 * 
	 * @return the amount of results
	 */
	public int amountOfResults();

	/**
	 * Gets an iterator to iterate over the results. The order is not defined
	 * and can be of any order.
	 * 
	 * @return an iterator to iterate over the results
	 */
	public IDoubleIterator resultsIterator();

	/**
	 * Gets an iterator to iterate over the sorted results.
	 * 
	 * @return an iterator to iterate over the sorted results
	 */
	public IDoubleIterator sortedResultsIterator();

	/**
	 * Gets an iterator to iterate over the sorted results (descending).
	 * 
	 * @return an iterator to iterate over the sorted results (descending)
	 */
	public IDoubleIterator descSortedResultsIterator();

	/**
	 * Adds the specified result.
	 * 
	 * @param result
	 *            the result to be added
	 */
	public void add(final double result);
}
