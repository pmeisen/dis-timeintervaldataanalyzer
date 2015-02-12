package net.meisen.dissertation.model.measures;

import net.meisen.dissertation.model.util.IIntIterator;

/**
 * A holder containing facts of records.
 * 
 * @author pmeisen
 * 
 */
public interface IFactsHolder extends IDoubleHolder {

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
	 * Gets an iterator to iterate over the identifiers of the records with
	 * fact-values.
	 * 
	 * @return an iterator to iterate over the identifiers
	 */
	public IIntIterator recordIdsIterator();

}
