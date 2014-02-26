package net.meisen.dissertation.impl.datasets;

import java.util.Iterator;

import net.meisen.dissertation.model.datasets.IDataRecord;

/**
 * This class defines an {@code Iterator} used to iterate over a
 * {@code SingleStaticDataSet}.
 * 
 * @author pmeisen
 * 
 */
public class SingleStaticDataSetIterator implements Iterator<IDataRecord> {

	private SingleStaticDataSet dataSet;
	private int current = 0;

	/**
	 * Creates an {@code Iterator} used to iterate over the specified
	 * {@code  SingleStaticDataSet}.
	 * 
	 * @param dataSet
	 *            the {@code SingleStaticDataSet} to iterate over
	 */
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
