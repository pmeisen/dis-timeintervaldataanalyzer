package net.meisen.dissertation.models.impl.dataretriever;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

public abstract class DataCollection<D> {

	private final List<D> names;

	public DataCollection(final D[] names) {
		if (names == null) {
			throw new NullPointerException("The names cannot be null.");
		}

		this.names = Collections.unmodifiableList(Arrays.asList(names));
	}

	public Collection<D> getNames() {
		return names;
	}

	public int getRecordSize() {
		return names.size();
	}

	public D getNameOfPos(final int pos) {
		return net.meisen.general.genmisc.collections.Collections.get(pos,
				names);
	}

	public int getPosOfName(final D name) {
		return net.meisen.general.genmisc.collections.Collections.getPosition(
				names, name);
	}

	public abstract DataIterator<D> iterate();

	public abstract void release();

	public Collection<DataRecord<D>> get() {
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
