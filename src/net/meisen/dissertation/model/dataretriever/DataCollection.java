package net.meisen.dissertation.model.dataretriever;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * A data collection is a collection which contains data addressed by a specific
 * name (i.e. equal to a {@link Map}). But instead this collection supports a
 * lazy data retrieval. This means that the data collected - e.g. from an
 * underlying database - is collected when asked for, whereby the meta
 * information of the data (i.e. the names) are known after creation.
 * 
 * @author pmeisen
 * 
 * @param <D> the type of the names of the collection
 */
public abstract class DataCollection<D> implements Iterable<DataRecord<D>> {

	private List<D> names = null;

	/**
	 * Constructor to create a {@code DataCollection} without any names, the
	 * names have to be defined using {@link #setNames(Object[])} prior to any
	 * other usage.
	 */
	protected DataCollection() {
		this(null);
	}

	/**
	 * Constructor to create a {@code DataCollection} with the specified names.
	 * If the {@code names} are {@code null} {@link #setNames(Object[])} has to
	 * be called prior to any other usage.
	 * 
	 * @param names
	 *            the list of names used for the data of the collection
	 */
	public DataCollection(final D[] names) {
		setNames(names);
	}
	
	@Override
	public abstract DataIterator<D> iterator();

	/**
	 * Set the names of the data of the collection. The names can only be set
	 * once and only if those are not set during construction (see
	 * {@link #DataCollection(Object[])}). If not set during construction, the
	 * names have to be set directly after it (i.e. no other method should be
	 * called prior to the setting).
	 * 
	 * @param names
	 *            the names to be set, cannot be {@code null}
	 */
	protected void setNames(final D[] names) {
		setNames(names == null ? (Collection<D>) null : Arrays.asList(names));
	}

	/**
	 * Set the names of the data of the collection. The names can only be set
	 * once and only if those are not set during construction (see
	 * {@link #DataCollection(Object[])}). If not set during construction, the
	 * names have to be set directly after it (i.e. no other method should be
	 * called prior to the setting).
	 * 
	 * @param names
	 *            the names to be set, cannot be {@code null}
	 */
	protected void setNames(final Collection<D> names) {
		if (this.names != null) {
			throw new IllegalStateException(
					"The names of a DataCollection cannot be modified after those are defined once.");
		} else if (names == null) {
			return;
		}

		final List<D> nameList = new ArrayList<D>();
		nameList.addAll(names);

		this.names = Collections.unmodifiableList(nameList);
	}

	/**
	 * Validates if the names are set, if not set an exception is thrown.
	 */
	protected void validate() {
		if (this.names == null) {
			throw new IllegalStateException(
					"Please specify the names of the DataCollection prior to any other usage, i.e. during construction or using the setNames method.");
		}
	}

	/**
	 * Get the defined names for the collection.
	 * 
	 * @return the defined names for the collection
	 */
	public Collection<D> getNames() {
		validate();

		return names;
	}

	/**
	 * Gets the size of a record contained in this collection.
	 * 
	 * @return the size of a record contained in this collection
	 */
	public int getRecordSize() {
		validate();

		return names.size();
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
	public D getNameOfPos(final int pos) throws IndexOutOfBoundsException {
		validate();

		return net.meisen.general.genmisc.collections.Collections.get(pos,
				names);
	}

	/**
	 * Gets the position of a name, i.e. the position of the data within the
	 * record for the specified {@code name}. The method returns {@code -1} if
	 * the name is not defined.
	 * 
	 * @param name
	 *            the name to get the position for
	 * 
	 * @return the position or {@code -1} if not found
	 */
	public int getPosOfName(final D name) {
		validate();

		return net.meisen.general.genmisc.collections.Collections.getPosition(
				names, name);
	}

	/**
	 * This method should be called whenever the collection is not used anymore.
	 * It releases all bound resources of the {@code DataCollection}.
	 */
	public abstract void release();

	/**
	 * Gets a {@code Collection} containing all the {@code DataRecords} of
	 * {@code this} collection.
	 * 
	 * @return a {@code Collection} containing all the {@code DataRecords} of
	 *         {@code this} collection
	 */
	public List<DataRecord<D>> get() {
		final Iterator<DataRecord<D>> it = iterator();
		final ArrayList<DataRecord<D>> data = new ArrayList<DataRecord<D>>();

		while (it.hasNext()) {
			data.add(it.next());
		}

		// close the iterator if it is one that has to be closed
		if (it instanceof ICloseableDataIterator) {
			((ICloseableDataIterator) it).close();
		}

		return data;
	}

	/**
	 * Transforms the {@code DataCollection} into a {@code Collection} of the
	 * first field of {@code this}. This would be equal to call {@link #get()}
	 * and retrieve all values of the field named by {@code getNameOfPos(0)}.
	 * 
	 * @return a {@code Collection} of the first field of {@code this}
	 */
	public <T> Collection<T> transform() {
		return transform(null);
	}

	/**
	 * Checks if the specified position is valid considering the defined
	 * {@code DataCollection}.
	 * 
	 * @param pos
	 *            the position to be validated
	 * 
	 * @return {@code true} if the position is a valid position, otherwise
	 *         {@code false}
	 */
	public boolean isValidPosition(final int pos) {
		return pos > -1 && pos < getRecordSize();
	}

	/**
	 * Transforms the {@code DataCollection} into a {@code Collection} of the
	 * field defined by {@code position} of {@code this}. This would be equal to
	 * call {@link #get()} and retrieve all values of the field named by
	 * {@code getNameOfPos(position)}.
	 * 
	 * @param position
	 *            the position to be included in the {@code Collection}
	 * 
	 * @return a {@code Collection} of the field defined by {@code position} of
	 *         {@code this}
	 */
	public <T> Collection<T> transform(final int position) {
		final ArrayList<T> data = new ArrayList<T>();
		final Iterator<DataRecord<D>> it = iterator();
		if (it == null) {
			return data;
		}

		while (it.hasNext()) {
			final DataRecord<D> rec = it.next();

			@SuppressWarnings("unchecked")
			final T value = (T) rec.getDataByPos(position);
			data.add(value);
		}

		// close the iterator if it is one that has to be closed
		if (it instanceof ICloseableDataIterator) {
			((ICloseableDataIterator) it).close();
		}

		return data;
	}

	/**
	 * Transforms the {@code DataCollection} into a {@code Collection} of the
	 * field with the name {@code fieldName}. This would be equal to call
	 * {@link #get()} and retrieve all values of the field with the name
	 * {@code fieldName}.
	 * 
	 * @param fieldName
	 *            the name of the field to be included in the {@code Collection}
	 * 
	 * @return a {@code Collection} of the field with the name {@code fieldName}
	 */
	public <T> Collection<T> transform(final D fieldName) {
		final int position;

		// define the position
		if (getRecordSize() < 1) {
			position = -1;
		} else if (fieldName == null) {
			position = 0;
		} else {
			position = getPosOfName(fieldName);
		}

		// check the position
		if (position == -1) {
			throw new IllegalArgumentException("The field '" + fieldName
					+ "' cannot be found within the query.");
		}

		return transform(position);
	}
	
	@Override
	public String toString() {
		return names == null ? null : names.toString();
	}
}
