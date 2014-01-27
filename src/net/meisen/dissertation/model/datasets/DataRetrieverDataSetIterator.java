package net.meisen.dissertation.model.datasets;

import java.util.Iterator;

import net.meisen.dissertation.model.dataretriever.DataCollection;
import net.meisen.dissertation.model.dataretriever.DataIterator;
import net.meisen.dissertation.model.dataretriever.ICloseableDataIterator;

public class DataRetrieverDataSetIterator implements Iterator<IDataRecord>,
		IClosableIterator {
	private final DataIterator<String> it;

	public DataRetrieverDataSetIterator(final DataCollection<String> collection) {
		this.it = collection.iterate();
	}

	@Override
	public boolean hasNext() {
		return it.hasNext();
	}

	@Override
	public IDataRecord next() {
		return new DataRetrieverDataSetRecord(it.next());
	}

	@Override
	public void remove() {
		it.remove();
	}

	@Override
	public void close() {
		if (it instanceof ICloseableDataIterator) {
			((ICloseableDataIterator) it).close();
		}
	}
}
