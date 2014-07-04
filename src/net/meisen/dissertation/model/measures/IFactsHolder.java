package net.meisen.dissertation.model.measures;

import net.meisen.dissertation.model.util.IDoubleIterator;
import net.meisen.dissertation.model.util.IIntIterator;

/**
 * A holder containing facts of records.
 * 
 * @author pmeisen
 * 
 */
public interface IFactsHolder {

	/**
	 * Get the fact of the record with the specified {@code recordId}. Returns
	 * {@link Double#NaN} if no value is known.
	 * 
	 * @param recordId
	 *            the identifier to get the value for
	 * 
	 * @return the fact associated to the record's identifier
	 */
	public double getFactOfRecord(final int recordId);

	/**
	 * Gets the amount of facts contained in the holder.
	 * 
	 * @return the amount of facts
	 */
	public int amountOfFacts();

	/**
	 * Gets an iterator to iterate over the identifiers of the records with
	 * fact-values.
	 * 
	 * @return an iterator to iterate over the identifiers
	 */
	public IIntIterator recordIdsIterator();

	/**
	 * Gets an iterator to iterate over the facts. The order is not defined and
	 * can be of any order.
	 * 
	 * @return an iterator to iterate over the facts
	 */
	public IDoubleIterator factsIterator();

	/**
	 * Gets an iterator to iterate over the sorted facts.
	 * 
	 * @return an iterator to iterate over the sorted facts
	 */
	public IDoubleIterator sortedFactsIterator();

	/**
	 * Gets an iterator to iterate over the sorted facts (descending).
	 * 
	 * @return an iterator to iterate over the sorted facts (descending)
	 */
	public IDoubleIterator descSortedFactsIterator();
}
