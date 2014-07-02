package net.meisen.dissertation.model.cache;

import java.util.Iterator;

import net.meisen.dissertation.impl.cache.BaseIdentifierCacheException;
import net.meisen.dissertation.model.indexes.datarecord.ProcessedDataRecord;
import net.meisen.dissertation.model.util.IIntIterator;

/**
 * A cache used for records.
 * 
 * @author pmeisen
 * 
 */
public interface IDataRecordCache extends ICache, Iterable<Integer> {

	/**
	 * Caches the specified {@code ProcessedDataRecord}.
	 * 
	 * @param record
	 *            the {@code ProcessedDataRecord} to be cached
	 */
	public void cache(final ProcessedDataRecord record);

	/**
	 * Caches the specified {@code Object-array} representation, created by the
	 * concrete implementation.
	 * 
	 * @param id
	 *            the identifier of the record to be cached
	 * @param record
	 *            the {@code ProcessedDataRecord} to be cached
	 */
	public void cache(final int id, final Object[] record);

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
	 * Gets an iterator used to iterate over the identifiers.
	 * 
	 * @return an iterator used to iterate over the identifiers
	 */
	public IIntIterator intIterator();

	@Override
	public Iterator<Integer> iterator();

	/**
	 * Sets the configuration of the cache. The method can only be called prior
	 * to initialization.
	 * 
	 * @param config
	 *            the configuration to be used
	 * 
	 * @throws BaseIdentifierCacheException
	 *             if the cache is initialized
	 */
	public void setConfig(final IDataRecordCacheConfig config)
			throws BaseIdentifierCacheException;

	/**
	 * Gets the amount of entries in the cache.
	 * 
	 * @return the amount of entries in the cache
	 */
	public int size();
}
