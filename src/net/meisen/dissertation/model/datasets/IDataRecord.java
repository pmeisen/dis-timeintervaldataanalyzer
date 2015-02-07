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
	 * Checks if a position is valid for {@code this} {@code DataRecord}. A
	 * position is {@code 1}-based.
	 * 
	 * @param position
	 *            the position to be checked
	 * 
	 * @return {@code true} if a value for the {@code position} can be retrieved
	 *         (i.e. no exception is thrown) for {@code this} {@code DataRecord}
	 *         , otherwise {@code false}
	 */
	public boolean isValidPosition(final int position);

	/**
	 * Gets the name of the specified position (1-based). The method might throw
	 * an exception if the position is invalid.
	 * 
	 * @param position
	 *            the position to get the name for
	 * @return the name of the position
	 * 
	 * @see #isValidPosition(int)
	 */
	public String getName(final int position);

	/**
	 * Gets the position of the specified name.
	 * 
	 * @param name
	 *            the name to get the position for
	 * 
	 * @return the position (1-based), or a value smaller than 1 if the name
	 *         could not be found
	 */
	public int getPosition(final String name);
}
