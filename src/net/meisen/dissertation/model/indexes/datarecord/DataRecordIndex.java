package net.meisen.dissertation.model.indexes.datarecord;

import net.meisen.dissertation.model.datasets.IDataRecord;

/**
 * An {@code Index} is used to index {@code DataRecord} instances by a specified
 * {@code int}.
 * 
 * @author pmeisen
 * 
 */
public interface DataRecordIndex {

	/**
	 * Method to add a {@code DataRecord} to the index.
	 * 
	 * @param dataId
	 *            the id of the {@code DataRecord}
	 * @param record
	 *            the {@code DataRecord} to be indexed
	 */
	public void index(final int dataId, final IDataRecord record);

	/**
	 * Method which should be called after all data is added to optimize the
	 * index considering e.g. storage
	 */
	public void optimize();
}
