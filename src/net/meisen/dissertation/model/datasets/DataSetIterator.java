package net.meisen.dissertation.model.datasets;

import java.util.Collection;
import java.util.Iterator;

public class DataSetIterator implements Iterator<IDataRecord>,
		IClosableIterator {

	private final IDataSet[] dataSets;
	private final int amount;

	private Iterator<IDataRecord> curIterator = null;
	private int curDataSet = 0;

	public DataSetIterator(final IDataSet dataSet) {
		this(dataSet == null ? null : new IDataSet[] { dataSet });
	}

	public DataSetIterator(final Collection<IDataSet> dataSets) {
		this(dataSets == null ? null : dataSets.toArray(new IDataSet[] {}));
	}

	public DataSetIterator(final IDataSet... dataSets) {
		final int amount;
		if (dataSets == null) {
			amount = 0;
		} else {
			int counter = 0;
			for (final IDataSet dataSet : dataSets) {
				if (dataSet != null) {
					counter++;
				}
			}
			amount = counter;
		}

		// now initialize the array of dataSets
		this.dataSets = new IDataSet[amount];
		int counter = 0;
		for (final IDataSet dataSet : dataSets) {
			if (dataSet != null) {
				this.dataSets[counter] = dataSet;
				counter++;
			}
		}

		this.amount = amount;
	}

	@Override
	public final void remove() {
		throw new UnsupportedOperationException(
				"A DataIterator does not support the remove operation.");
	}

	@Override
	public boolean hasNext() {

		if (amount <= curDataSet) {
			return false;
		} else {

			// get the current dataSet
			final IDataSet dataSet = dataSets[curDataSet];

			// if we don't have an iterator get the current one
			if (curIterator == null) {
				curIterator = dataSet.iterate();
				return hasNext();
			}
			// check if the iterator has a next one
			else if (curIterator.hasNext()) {
				return true;
			}
			// get to the next dataSet and check it
			else {

				// make sure the actual one is closed
				close();

				// now get the next one
				curIterator = null;
				curDataSet++;
				return hasNext();
			}
		}
	}

	@Override
	public IDataRecord next() {
		if (hasNext()) {
			return curIterator.next();
		} else {
			throw new IllegalStateException(
					"There is no next-value, please verify by using hasNext()");
		}
	}

	@Override
	public void close() {
		if (curIterator != null && curIterator instanceof IClosableIterator) {
			((IClosableIterator) curIterator).close();
		}
	}
}
