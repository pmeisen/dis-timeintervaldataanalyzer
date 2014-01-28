package net.meisen.dissertation.model.datasets;

import net.meisen.dissertation.model.dataretriever.DataCollection;
import net.meisen.dissertation.model.dataretriever.DataIterator;
import net.meisen.dissertation.model.dataretriever.ICloseableDataIterator;

/**
 * Iterator to iterate over a {@code DataRetrieverDataSet}. This is more or less
 * just a wrapper for a {@code DataIterator}.
 * 
 * @author pmeisen
 * 
 * @see DataRetrieverDataSet
 * 
 */
public class DataRetrieverDataSetIterator implements
		IClosableIterator<IDataRecord> {
	private final DataIterator<String> it;

	/**
	 * The constructor which specifies which {@code DataCollection} {@code this}
	 * instance wraps.
	 * 
	 * @param collection
	 */
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
