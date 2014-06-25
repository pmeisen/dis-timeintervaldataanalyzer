package net.meisen.dissertation.model.cache;

import net.meisen.dissertation.model.indexes.datarecord.ProcessedDataRecord;

/**
 * A cache used for records.
 * 
 * @author pmeisen
 * 
 */
public interface IDataRecordCache extends ICache {

	/**
	 * Caches the specified {@code ProcessedDataRecord}.
	 * 
	 * @param record
	 *            the {@code ProcessedDataRecord} to be cached
	 */
	public void cache(final ProcessedDataRecord record);

	/**
	 * Retrieves the values of the record for the specified {@code recordId}.
	 * 
	 * @param recordId
	 *            the values of the record for the specified {@code recordId}
	 * 
	 * @return the values of the record for the specified {@code recordId}
	 */
	public Object[] get(final int recordId);

	/**
	 * Get the names of the different values of a record.
	 * 
	 * @return the names of the different values of a record
	 */
	public String[] getNames();

	/**
	 * Get the types of the different values of a record.
	 * 
	 * @return the types of the different values of a record
	 */
	public Class<?>[] getTypes();
}
