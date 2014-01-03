package net.meisen.dissertation.models.impl.dataretriever;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class DataRecord<D> {

	private final List<Object> record = new ArrayList<Object>();
	private final DataCollection<D> collection;

	public DataRecord(final DataCollection<D> collection) {
		this(collection, null);
	}

	public DataRecord(final DataCollection<D> collection, final Object[] data) {
		if (collection == null) {
			throw new NullPointerException("The collection cannot be null.");
		} else if (data == null) {
			// nothing to do
		} else if (collection.getRecordSize() < data.length) {
			throw new IllegalArgumentException(
					"The specified data must be valid according to the collection specification ("
							+ data.length + " > " + collection.getRecordSize()
							+ ".");
		} else {

			// add all the specified values
			for (final Object d : data) {
				record.add(d);
			}
		}
		
		this.collection = collection;
	}

	public void setData(final D name, final Object data) {
		final int pos = collection.getPosOfName(name);
		if (pos == -1) {
			throw new IllegalArgumentException("The specified name '" + name
					+ "' is not defined within the collection.");
		}

		record.add(pos, data);
	}

	public Object getData(final D name) {
		final int pos = collection.getPosOfName(name);
		if (pos == -1) {
			throw new IllegalArgumentException("The specified name '" + name
					+ "' is not defined within the collection.");
		}

		return record.get(pos);
	}

	public D getNameOfPos(final int pos) {
		return collection.getNameOfPos(pos);
	}

	public Collection<D> getNames() {
		return collection.getNames();
	}

	public int getSize() {
		return collection.getRecordSize();
	}

	public Object getDataByPos(final int pos) {
		return record.get(pos);
	}

	public Collection<Object> getData() {
		return record;
	}
}
