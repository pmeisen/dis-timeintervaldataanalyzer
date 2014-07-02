package net.meisen.dissertation.model.indexes.datarecord;

import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;

/**
 * A factory used to create {@code IDataRecord} instances based on an array
 * representation.
 * 
 * @author pmeisen
 * 
 */
public interface IDataRecordFactory {

	/**
	 * Initializes the factory for the specified {@code model}.
	 * 
	 * @param model
	 *            the model {@code this} instance should be initialized for
	 */
	public void initialize(final TidaModel model);

	/**
	 * Creates the {@code DataRecord} instance for the specified
	 * array-representation.
	 * 
	 * @param values
	 *            the array-representation used to create the {@code DataRecord}
	 *            for
	 * 
	 * @return the created {@code DataRecord} instance
	 */
	public IDataRecord create(final Object[] values);

	/**
	 * The meta-information of the factory used to create the instances.
	 * 
	 * @return the {@code DataRecordMeta} instance of the factory, cannot be
	 *         {@code null}
	 * 
	 * @see IDataRecordMeta
	 */
	public IDataRecordMeta getMeta();
}
