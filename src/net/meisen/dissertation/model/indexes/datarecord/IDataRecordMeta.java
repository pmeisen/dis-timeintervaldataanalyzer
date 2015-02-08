package net.meisen.dissertation.model.indexes.datarecord;

import net.meisen.dissertation.jdbc.protocol.DataType;

/**
 * Meta-information of a record handled within the database.
 * 
 * @author pmeisen
 * 
 */
public interface IDataRecordMeta {

	/**
	 * The types of a record.
	 * 
	 * @return the types of the record
	 * 
	 * @see DataType
	 */
	public DataType[] getDataTypes();

	/**
	 * Gets the identifier of the {@code DescriptorModel} assigned to the
	 * specified position. The method returns {@code null} if the position isn't
	 * a valid position of a {@code DescriptorModel's} value.
	 * 
	 * @param position
	 *            the position to get the identifier for
	 * 
	 * @return the identifier of the {@code DescriptorModel} at the
	 *         {@code position}; {@code null} if the position is not any of a
	 *         {@code DescriptorModel}
	 */
	public String getDescriptorModelId(final int position);

	/**
	 * Gets the types of a record. The method returns the {@code DataTypes'}
	 * representer.
	 * 
	 * @return the types of the record
	 * 
	 * @see DataType#getRepresentorClass()
	 */
	public Class<?>[] getTypes();

	/**
	 * Gets the names of a record. The method returns the names.
	 * 
	 * @return the names of the record
	 */
	public String[] getNames();

	/**
	 * Gets the position within a record containing the identifier of the
	 * record. The position is 1-based.
	 * 
	 * @return the position within a record containing the identifier of the
	 *         record
	 */
	public int getPosRecordId();

	/**
	 * Gets the position within a record containing the start value of the
	 * record. The position is 1-based.
	 * 
	 * @return the position within a record containing the start value of the
	 *         record
	 */
	public int getPosStart();

	/**
	 * Gets the position within a record containing the end value of the record.
	 * The position is 1-based.
	 * 
	 * @return the position within a record containing the end value of the
	 *         record
	 */
	public int getPosEnd();

	/**
	 * Gets the first position (including) of a {@code DescriptorModel's} value.
	 * The position is 1-based.
	 * 
	 * @return the first position of a {@code DescriptorModel's} value
	 */
	public int getFirstPosDescModelIds();

	/**
	 * Gets the last position (including) of a {@code DescriptorModel's} value.
	 * The position is 1-based.
	 * 
	 * @return the last position of a {@code DescriptorModel's} value
	 */
	public int getLastPosDescModelIds();

	/**
	 * Gets the amount of values containing values of {@code Descriptors}.
	 * 
	 * @return the amount of values containing values of {@code Descriptors}
	 */
	public int sizeOfDescModelIds();
}
