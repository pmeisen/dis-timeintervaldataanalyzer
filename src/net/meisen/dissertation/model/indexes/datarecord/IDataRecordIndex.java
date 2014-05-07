package net.meisen.dissertation.model.indexes.datarecord;

import net.meisen.dissertation.model.persistence.IPersistable;

/**
 * An {@code Index} is used to index {@code DataRecord} instances by a specified
 * {@code int}.
 * 
 * @author pmeisen
 * 
 */
public interface IDataRecordIndex extends IPersistable {

	/**
	 * Method to add a {@code DataRecord} to the index.
	 * 
	 * @param record
	 *            the {@code DataRecord} to be indexed
	 */
	public void index(final ProcessedDataRecord record);

	/**
	 * Method which should be called after all data is added to optimize the
	 * index considering e.g. storage
	 */
	public void optimize();
}
