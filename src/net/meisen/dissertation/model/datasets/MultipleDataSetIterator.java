package net.meisen.dissertation.model.datasets;

import java.util.Collection;
import java.util.Iterator;

import net.meisen.dissertation.model.data.OfflineMode;

/**
 * A {@code MultipleDataSetIterator} is used to iterate over the
 * {@code DataRecord} instances of different {@code DataSets}.
 * 
 * @author pmeisen
 * 
 */
public class MultipleDataSetIterator implements IClosableIterator<IDataRecord> {

	private final IDataSet[] dataSets;
	private final int amount;

	private Iterator<IDataRecord> curIterator = null;
	private int curDataSet = 0;

	/**
	 * A constructor which creates an {@code Iterator} for just the specified
	 * {@code DataSet}. Instead it might make more sense to just call
	 * {@code IDataSet#iterate()}.
	 * 
	 * @param mode
	 *            the {@code OfflineMode} to be used, i.e. which datasets to
	 *            iterate
	 * @param dataSet
	 *            the {@code DataSet} to iterate over
	 */
	public MultipleDataSetIterator(final OfflineMode mode,
			final IDataSet dataSet) {
		this(mode, dataSet == null ? null : new IDataSet[] { dataSet });
	}

	/**
	 * Constructor to create an {@code Iterator} to iterate over several
	 * {@code DataSet} instances.
	 * 
	 * @param mode
	 *            the {@code OfflineMode} to be used, i.e. which datasets to
	 *            iterate
	 * @param dataSets
	 *            the several {@code DataSet} instances to iterate over
	 */
	public MultipleDataSetIterator(final OfflineMode mode,
			final Collection<IDataSet> dataSets) {
		this(mode, dataSets == null ? null : dataSets
				.toArray(new IDataSet[] {}));
	}

	/**
	 * Constructor to create an {@code Iterator} to iterate over several
	 * {@code DataSet} instances.
	 * 
	 * @param mode
	 *            the {@code OfflineMode} to be used, i.e. which datasets to
	 *            iterate
	 * @param dataSets
	 *            the several {@code DataSet} instances to iterate over
	 */
	public MultipleDataSetIterator(final OfflineMode mode,
			final IDataSet... dataSets) {
		final boolean checkOfflineAvailability = OfflineMode.TRUE.equals(mode);

		final int amount;
		if (dataSets == null) {
			amount = 0;
		} else {
			int counter = 0;
			for (final IDataSet dataSet : dataSets) {
				if (dataSet != null
						&& (!checkOfflineAvailability || dataSet
								.isOfflineAvailable())) {
					counter++;
				}
			}
			amount = counter;
		}

		// now initialize the array of dataSets
		this.dataSets = new IDataSet[amount];
		int counter = 0;
		for (final IDataSet dataSet : dataSets) {
			if (dataSet != null
					&& (!checkOfflineAvailability || dataSet
							.isOfflineAvailable())) {
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

			// if we don't have an iterator get the current one
			if (curIterator == null) {
				
				// get the current dataSet
				final IDataSet dataSet = dataSets[curDataSet];
				curIterator = dataSet.iterator();
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
			((IClosableIterator<?>) curIterator).close();
		}
	}

	/**
	 * This method should normally never be used because it needs to <b>iterate
	 * over all the data</b>, therefore it makes sense to implement the
	 * iteration and get the count meanwhile (if not only the count is needed).
	 * 
	 * @return the counted data
	 */
	public int count() {
		int counter = 0;
		
		for (final IDataSet dataSet : dataSets) {
			final Iterator<IDataRecord> it = dataSet.iterator();
			while (it.hasNext()) {
				counter++;
				it.next();
			}
		}
		
		return counter;
	}
}
