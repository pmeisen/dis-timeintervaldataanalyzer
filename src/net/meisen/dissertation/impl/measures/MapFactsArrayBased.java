package net.meisen.dissertation.impl.measures;

import java.util.Arrays;

import gnu.trove.impl.Constants;
import gnu.trove.iterator.TDoubleIterator;
import gnu.trove.iterator.TIntIterator;
import gnu.trove.map.hash.TIntDoubleHashMap;
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

	/**
	 * Default constructor.
	 */
	public MapFactsArrayBased() {
		this(-1);
	}

	/**
	 * Constructor defining an initial capacity.
	 * 
	 * @param capacity
	 *            the initiali capacity, might be {@code -1} if unknown
	 */
	public MapFactsArrayBased(final int capacity) {
		this.map = new TIntDoubleHashMap(
				capacity < 0 ? Constants.DEFAULT_CAPACITY : capacity,
				Constants.DEFAULT_LOAD_FACTOR, -1, Double.NaN);
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

		map.put(recordId, factValue);
	}

	@Override
	public int amountOfFacts() {
		return map.size();
	}

	@Override
	public double getFactOfRecord(final int recordId) {

		// get the value stored in the map
		return map.get(recordId);
	}

	@Override
	public IIntIterator recordIdsIterator() {

		return new IIntIterator() {
			final TIntIterator it = map.keySet().iterator();

			@Override
			public boolean hasNext() {
				return it.hasNext();
			}

			@Override
			public int next() {
				return it.next();
			}
		};
	}

	@Override
	public IDoubleIterator factsIterator() {

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
	}

	@Override
	public IDoubleIterator sortedFactsIterator() {
		final double[] facts = map.values();
		Arrays.sort(facts);

		return new IDoubleIterator() {
			int pos = 0;

			@Override
			public boolean hasNext() {
				return pos < facts.length;
			}

			@Override
			public double next() {
				final int lastPos = pos;
				pos++;

				return facts[lastPos];
			}
		};
	}
}
