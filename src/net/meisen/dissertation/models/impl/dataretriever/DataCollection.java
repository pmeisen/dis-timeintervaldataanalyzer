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

	public abstract Iterator<DataRecord<D>> open();

	public abstract void close();

	public Collection<DataRecord<D>> get() {
		final Iterator<DataRecord<D>> it = open();
		final ArrayList<DataRecord<D>> data = new ArrayList<DataRecord<D>>();

		while (it.hasNext()) {
			data.add(it.next());
		}

		return data;
	}

	public <T> Collection<T> transform() {
		if (getRecordSize() != 1) {
			throw new IllegalStateException(
					"A dataCollection must be of size 1 to be transformable into a Collection.");
		}

		final ArrayList<T> data = new ArrayList<T>();
		final Iterator<DataRecord<D>> it = open();
		if (it == null) {
			return data;
		}

		while (it.hasNext()) {
			final DataRecord<D> rec = it.next();

			@SuppressWarnings("unchecked")
			final T value = (T) rec.getDataByPos(0);
			data.add(value);
		}

		return data;
	}
}
