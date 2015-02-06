package net.meisen.dissertation.model.dataretriever;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import net.meisen.dissertation.exceptions.DataRetrieverException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

/**
 * A {@code DataRecord} is one array of data of a {@code DataCollection}. Each
 * data of the {@code DataRecord} has a specific names defined by the
 * {@code DataCollection} the record belongs to.
 * 
 * @author pmeisen
 * 
 * @param <D>
 *            the type of the names of the record
 */
public class DataRecord<D> {

	private final List<Object> record = new ArrayList<Object>();
	private final DataCollection<D> collection;

	/**
	 * Constructor to create a {@code DataRecord} of the specified
	 * {@code collection}.
	 * 
	 * @param collection
	 *            the {@code DataCollection} the {@code DataRecord} belongs to
	 */
	public DataRecord(final DataCollection<D> collection) {
		this(collection, null);
	}

	/**
	 * Constructor to create a {@code DataRecord} of the specified
	 * {@code collection} with the specified {@code data}.
	 * 
	 * @param collection
	 *            the {@code DataCollection} the {@code DataRecord} belongs to
	 * @param data
	 *            the data of the record
	 */
	public DataRecord(final DataCollection<D> collection, final Object[] data) {
		if (collection == null) {
			throw new NullPointerException("The collection cannot be null.");
		} else if (data == null) {
			// nothing to do
		} else if (collection.getRecordSize() < data.length) {
			throw new ForwardedRuntimeException(DataRetrieverException.class,
					1004, data.length, collection.getRecordSize());
		} else {

			// add all the specified values
			for (final Object d : data) {
				record.add(d);
			}
		}

		this.collection = collection;
	}

	/**
	 * Sets the {@code data} for the specified {@code name}.
	 * 
	 * @param name
	 *            the name to set the data for
	 * @param data
	 *            the data to be set
	 */
	public void setData(final D name, final Object data) {
		final int pos = collection.getPosOfName(name);
		if (pos == -1) {
			throw new ForwardedRuntimeException(DataRetrieverException.class,
					1003, name);
		}

		setDataByPos(pos, data);
	}

	/**
	 * Gets the data for the specified name.
	 * 
	 * @param name
	 *            the name to get the data for
	 * 
	 * @return the data for the specified name
	 */
	public Object getData(final D name) {
		final int pos = collection.getPosOfName(name);
		if (pos == -1) {
			throw new ForwardedRuntimeException(DataRetrieverException.class,
					1003, name);
		}

		return getDataByPos(pos);
	}

	/**
	 * Gets the name of the specified position. An exception is thrown if the
	 * specified position is invalid (i.e. out of bound).
	 * 
	 * @param pos
	 *            the position to get the name for
	 * 
	 * @return the name of the position
	 * 
	 * @throws IndexOutOfBoundsException
	 *             if the specified position is not within the range of the
	 *             {@link Collection}
	 */
	public D getNameOfPos(final int pos) {
		return collection.getNameOfPos(pos);
	}

	/**
	 * Get the defined names of the underlying {@code DataCollection}.
	 * 
	 * @return the defined names of the underlying {@code DataCollection}
	 */
	public Collection<D> getNames() {
		return collection.getNames();
	}

	/**
	 * Gets the size of a record, i.e. the amount of data fields.
	 * 
	 * @return the size of a record
	 */
	public int getSize() {
		return collection.getRecordSize();
	}

	/**
	 * Gets the data at the specified position. The position is {@code 0}-based.
	 * 
	 * @param pos
	 *            the position to get the data for
	 * 
	 * @return the data for the specified position
	 */
	public Object getDataByPos(final int pos) {
		return record.get(pos);
	}

	/**
	 * Sets the {@code data} within the {@code DataRecord} at the specified
	 * {@code pos}.
	 * 
	 * @param pos
	 *            the position to set the data for
	 * @param data
	 *            the data to be set
	 */
	public void setDataByPos(final int pos, final Object data) {
		record.add(pos, data);
	}

	/**
	 * Get all the data of the record.
	 * 
	 * @return all the data of the record as unmodifiable {@code List}
	 */
	public Collection<Object> getData() {
		return Collections.unmodifiableList(record);
	}

	@Override
	public String toString() {
		return (record == null ? null : record.toString()) + " ("
				+ (collection == null ? null : collection.toString()) + ")";
	}
}
