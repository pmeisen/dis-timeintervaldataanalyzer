package net.meisen.dissertation.models.impl.dataretriever;

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
 * @param <D>
 */
public abstract class DataCollection<D> {

	private List<D> names = null;

	public DataCollection() {
		this(null);
	}

	public DataCollection(final D[] names) {
		setNames(names);
	}

	protected void setNames(final D[] names) {
		if (this.names != null) {
			throw new IllegalStateException(
					"The names of a DataCollection cannot be modified after those are defined once.");
		} else if (names == null) {
			return;
		}

		this.names = Collections.unmodifiableList(Arrays.asList(names));
	}

	protected void validate() {
		if (this.names == null) {
			throw new IllegalStateException(
					"Please specify the names of the DataCollection prior to any other usage, i.e. during construction or using the setNames method.");
		}
	}

	public Collection<D> getNames() {
		validate();
		
		return names;
	}

	public int getRecordSize() {
		validate();
		
		return names.size();
	}

	public D getNameOfPos(final int pos) {
		validate();

		return net.meisen.general.genmisc.collections.Collections.get(pos,
				names);
	}

	public int getPosOfName(final D name) {
		validate();

		return net.meisen.general.genmisc.collections.Collections.getPosition(
				names, name);
	}

	public abstract DataIterator<D> iterate();

	public abstract void release();

	public Collection<DataRecord<D>> get() {
		validate();

		final Iterator<DataRecord<D>> it = iterate();
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

	public <T> Collection<T> transform() {
		return transform(null);
	}

	public <T> Collection<T> transform(final int position) {
		validate();

		final ArrayList<T> data = new ArrayList<T>();
		final Iterator<DataRecord<D>> it = iterate();
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
}
