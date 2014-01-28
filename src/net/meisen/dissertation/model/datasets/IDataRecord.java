package net.meisen.dissertation.model.datasets;

/**
 * A record holding some named data of a specific position.
 * 
 * @author pmeisen
 * 
 */
public interface IDataRecord {

	/**
	 * Checks if the {@code DataRecord} has a value for the specified name.
	 * 
	 * @param name
	 *            the name to be checked
	 * 
	 * @return {@code true} if a value for the specified name is assigned,
	 *         otherwise {@code false}
	 */
	public boolean hasNamedValue(final String name);

	/**
	 * Gets the value associated to the specified {@code position}. A position
	 * is {@code 1}-based, i.e. the first position has the number {@code 1}.
	 * 
	 * @param position
	 *            the position to get the data from
	 * 
	 * @return the data of the position
	 * 
	 * @throws RuntimeException
	 *             if the position is invalid
	 */
	public Object getValue(final int position) throws RuntimeException;

	/**
	 * Gets the value associated to the specified {@code name}.
	 * 
	 * @param name
	 *            the name of the data to be retrieved
	 * 
	 * @return the data of the position
	 * 
	 * @throws RuntimeException
	 *             if the position is invalid
	 */
	public Object getValue(final String name) throws RuntimeException;

	/**
	 * Gets the amount of data (i.e. positions)
	 * 
	 * @return the amount of data (i.e. positions)
	 */
	public int getSize();

}
