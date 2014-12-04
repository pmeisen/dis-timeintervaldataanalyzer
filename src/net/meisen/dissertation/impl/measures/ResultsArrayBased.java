package net.meisen.dissertation.impl.measures;

import gnu.trove.iterator.TDoubleIterator;
import gnu.trove.list.array.TDoubleArrayList;
import net.meisen.dissertation.model.measures.IResultsHolder;
import net.meisen.dissertation.model.util.IDoubleIterator;

/**
 * A concrete implementation of a {@code ResultHolder} based on an optimized
 * array-list.
 * 
 * @author pmeisen
 * 
 * @see IResultsHolder
 * @see TDoubleArrayList
 * 
 */
public class ResultsArrayBased implements IResultsHolder {
	final TDoubleArrayList list = new TDoubleArrayList();

	@Override
	public int amountOfResults() {
		return list.size();
	}

	@Override
	public IDoubleIterator resultsIterator() {
		return new IDoubleIterator() {
			final TDoubleIterator it = list.iterator();

			@Override
			public boolean hasNext() {
				return it.hasNext();
			}

			@Override
			public double next() {
				return it.next();
			}
		};
	}

	@Override
	public IDoubleIterator sortedResultsIterator() {
		list.sort();

		return new IDoubleIterator() {
			final TDoubleIterator it = list.iterator();

			@Override
			public boolean hasNext() {
				return it.hasNext();
			}

			@Override
			public double next() {
				return it.next();
			}
		};
	}

	@Override
	public IDoubleIterator descSortedResultsIterator() {
		list.sort();

		return new IDoubleIterator() {
			int pos = list.size() - 1;

			@Override
			public boolean hasNext() {
				return pos > -1;
			}

			@Override
			public double next() {
				final double value = list.get(pos);
				pos--;
				return value;
			}
		};
	}

	@Override
	public void add(final double result) {
		list.add(result);
	}

}
