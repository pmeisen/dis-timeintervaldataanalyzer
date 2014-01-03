package net.meisen.dissertation.models.impl.dataretriever;

import java.util.Iterator;

public class DataCollectionIterator<D extends Object> implements
		Iterator<DataRecord<D>> {

	public void close() {
		// close the iterator
	}
	
	@Override
	public boolean hasNext() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public DataRecord<D> next() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public final void remove() {
		throw new IllegalStateException(
				"A DataCollectionIterator does not support the remove operation.");
	}
}
