package net.meisen.dissertation.impl.measures;

import gnu.trove.impl.Constants;
import gnu.trove.iterator.TDoubleIterator;
import gnu.trove.iterator.TIntIterator;
import gnu.trove.map.hash.TIntDoubleHashMap;

import java.util.Arrays;

import net.meisen.dissertation.model.measures.IFactsHolder;
import net.meisen.dissertation.model.util.IDoubleIterator;
import net.meisen.dissertation.model.util.IIntIterator;

/**
 * An array based implementation used to hold the facts.
 * 
 * @author pmeisen
 * 
 */
public class MapFactsArrayBased implements IFactsHolder {
	private final TIntDoubleHashMap map;
	private final TIntDoubleHashMap nanMap;

	/**
	 * Default constructor.
	 */
	public MapFactsArrayBased() {
		this(-1, -1);
	}

	/**
	 * Constructor defining an initial capacity.
	 * 
	 * @param nonNaNcapacity
	 *            the initial capacity of non NaN values, might be {@code -1} if
	 *            unknown
	 * @param naNcapacity
	 *            the initial capacity of NaN values, might be {@code -1} if
	 *            unknown
	 */
	public MapFactsArrayBased(final int nonNaNcapacity, final int naNcapacity) {
		this.map = new TIntDoubleHashMap(
				nonNaNcapacity < 0 ? Constants.DEFAULT_CAPACITY
						: nonNaNcapacity, Constants.DEFAULT_LOAD_FACTOR, -1,
				Double.NaN);
		this.nanMap = new TIntDoubleHashMap(
				naNcapacity < 0 ? Constants.DEFAULT_CAPACITY : naNcapacity,
				Constants.DEFAULT_LOAD_FACTOR, -1, 0.00);
	}

	/**
	 * Set the {@code factValue} for all the identifiers referred to by the
	 * {@code it}.
	 * 
	 * @param it
	 *            the iterator referring to the identifiers of the records to
	 *            set the {@code factValue} for
	 * @param factValue
	 *            the value of the fact to be set
	 */
	public void setAll(final IIntIterator it, final double factValue) {
		if (it == null) {
			return;
		}

		while (it.hasNext()) {
			set(it.next(), factValue);
		}
	}

	/**
	 * Sets the value for the specified {@code recordId} to the specified
	 * {@code factValue}.
	 * 
	 * @param recordId
	 *            the identifier of the record the {@code value} belongs to
	 * @param factValue
	 *            the value of the fact
	 */
	public void set(final int recordId, final double factValue) {
		if (recordId < 0) {
			throw new IllegalArgumentException(
					"The record cannot have an identifier smaller than 0.");
		}

		if (Double.isNaN(factValue)) {
			nanMap.put(recordId, factValue);
		} else {
			map.put(recordId, factValue);
		}
	}

	@Override
	public int amount() {
		return map.size() + nanMap.size();
	}

	@Override
	public int amountOfNonNaN() {
		return map.size();
	}

	@Override
	public int amountOfNaN() {
		return nanMap.size();
	}

	@Override
	public double getFactOfRecord(final int recordId) {
		double val = nanMap.get(recordId);
		if (val == 0.00) {
			val = map.get(recordId);
		}

		return val;
	}

	@Override
	public IIntIterator recordIdsIterator() {

		return new IIntIterator() {
			final TIntIterator it = map.keySet().iterator();
			final TIntIterator nanIt = nanMap.keySet().iterator();

			@Override
			public boolean hasNext() {
				return it.hasNext() || nanIt.hasNext();
			}

			@Override
			public int next() {
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
	public IDoubleIterator iterator(final boolean excludeNaN) {

		if (excludeNaN) {
			return new IDoubleIterator() {
				final TDoubleIterator it = map.valueCollection().iterator();

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

			return new IDoubleIterator() {
				final TDoubleIterator it = map.valueCollection().iterator();
				final TDoubleIterator nanIt = nanMap.valueCollection()
						.iterator();

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
						throw new IllegalStateException(
								"No next value available.");
					}
				}
			};
		}
	}

	@Override
	public IDoubleIterator sortedIterator() {
		final double[] facts = map.values();
		Arrays.sort(facts);

		return new IDoubleIterator() {
			final TDoubleIterator nanIt = nanMap.valueCollection().iterator();

			int pos = 0;

			@Override
			public boolean hasNext() {
				return pos < facts.length || nanIt.hasNext();
			}

			@Override
			public double next() {

				if (pos < facts.length) {
					final int lastPos = pos;
					pos++;
					return facts[lastPos];
				} else if (nanIt.hasNext()) {
					return nanIt.next();
				} else {
					throw new IllegalStateException("No next value available.");
				}
			}
		};
	}

	@Override
	public IDoubleIterator descSortedIterator() {
		final double[] facts = map.values();
		Arrays.sort(facts);

		return new IDoubleIterator() {
			final TDoubleIterator nanIt = nanMap.valueCollection().iterator();

			int pos = facts.length - 1;

			@Override
			public boolean hasNext() {
				return pos > -1 || nanIt.hasNext();
			}

			@Override
			public double next() {

				if (pos > -1) {
					final int lastPos = pos;
					pos--;
					return facts[lastPos];
				} else if (nanIt.hasNext()) {
					return nanIt.next();
				} else {
					throw new IllegalStateException("No next value available.");
				}

			}
		};
	}
}
