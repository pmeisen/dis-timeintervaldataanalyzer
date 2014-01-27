package net.meisen.dissertation.model.datasets;

import java.util.Iterator;

public class SingleStaticDataSetIterator implements Iterator<IDataRecord> {

	private SingleStaticDataSet dataSet;
	private int current = 0;

	public SingleStaticDataSetIterator(final SingleStaticDataSet dataSet) {
		this.dataSet = dataSet;
	}

	@Override
	public boolean hasNext() {
		return dataSet == null ? false : current < 1;
	}

	@Override
	public IDataRecord next() {
		if (!hasNext()) {
			throw new IllegalStateException(
					"The iterator has no further elements.");
		}

		current++;
		return dataSet;
	}

	@Override
	public void remove() {
		throw new UnsupportedOperationException(
				"Cannot remove any elements from a SingleStaticDataSet.");
	}
}
