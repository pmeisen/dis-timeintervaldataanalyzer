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
	private final TDoubleArrayList list = new TDoubleArrayList();
	private final TDoubleArrayList nan = new TDoubleArrayList();

	private boolean sorted = false;

	@Override
	public int amount() {
		return amountOfNonNaN() + amountOfNaN();
	}

	@Override
	public int amountOfNonNaN() {
		return list.size();
	}

	@Override
	public int amountOfNaN() {
		return nan.size();
	}

	@Override
	public IDoubleIterator iterator(final boolean excludeNaN) {

		if (excludeNaN) {
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
		} else {
			return createIterator();
		}
	}

	@Override
	public IDoubleIterator sortedIterator() {
		sort();
		return createIterator();
	}

	@Override
	public IDoubleIterator descSortedIterator() {
		sort();

		return new IDoubleIterator() {
			final TDoubleIterator nanIt = nan.iterator();

			int pos = list.size() - 1;

			@Override
			public boolean hasNext() {
				return pos > -1 || nanIt.hasNext();
			}

			@Override
			public double next() {
				if (pos > -1) {
					final double value = list.get(pos);
					pos--;
					return value;
				} else if (nanIt.hasNext()) {
					return nanIt.next();
				} else {
					throw new IllegalStateException("No next value available.");
				}
			}
		};
	}

	@Override
	public void add(final double result) {
		sorted = false;

		if (Double.isNaN(result)) {
			nan.add(result);
		} else {
			list.add(result);
		}
	}

	/**
	 * Sorts the list of values if not sorted yet.
	 */
	protected void sort() {
		if (!sorted) {
			list.sort();
			sorted = true;
		}
	}

	/**
	 * Helper method to create an iterator to iterate over the full list,
	 * appending the NaN values last.
	 * 
	 * @return the iterator
	 */
	protected IDoubleIterator createIterator() {

		return new IDoubleIterator() {
			final TDoubleIterator it = list.iterator();
			final TDoubleIterator nanIt = nan.iterator();

			@Override
			public boolean hasNext() {
				return it.hasNext() || nanIt.hasNext();
			}

			@Override
			public double next() {
				if (it.hasNext()) {
					return it.next();
				} else if (nanIt.hasNext()) {
					return nanIt.next();
				} else {
					throw new IllegalStateException("No next value available.");
				}
			}
		};
	}

	@Override
	public String toString() {
		return list.toString();
	}
}
