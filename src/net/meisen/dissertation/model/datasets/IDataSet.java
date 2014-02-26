package net.meisen.dissertation.model.datasets;

import net.meisen.dissertation.model.data.OfflineMode;

/**
 * A set of data, whereby the data is represented by so called
 * {@code DataRecords}.
 * 
 * @author pmeisen
 * 
 * @see IDataRecord
 * 
 */
public interface IDataSet extends Iterable<IDataRecord> {

	/**
	 * Checks if the {@code DataSet} has values for the specified {@code name}.
	 * 
	 * @param name
	 *            the name to be checked
	 * @return {@code true} if a value for the {@code name} can be retrieved
	 *         (i.e. no exception is thrown) for each {@code DataRecord} of the
	 *         set, otherwise {@code false}
	 */
	public boolean hasNamedValue(final String name);

	/**
	 * Checks if a position is valid for {@code this} {@code DataSet}. A
	 * position is {@code 1}-based.
	 * 
	 * @param position
	 *            the position to be checked
	 * @return {@code true} if a value for the {@code position} can be retrieved
	 *         (i.e. no exception is thrown) for each {@code DataRecord} of the
	 *         set, otherwise {@code false}
	 */
	public boolean isValidPosition(final int position);

	/**
	 * Determines if the data is available even if the system is in offlineMode.
	 * 
	 * @return {@code true} if the data is available even in offline mode,
	 *         otherwise {@code false}
	 * 
	 * @see OfflineMode
	 */
	public boolean isOfflineAvailable();
}
