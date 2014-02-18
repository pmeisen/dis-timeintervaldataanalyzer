package net.meisen.dissertation.impl.indexes;

import java.util.Iterator;

/**
 * Iterator to iterate over a specific range of the {@code IntArrayCollection}.
 * 
 * @author pmeisen
 * 
 */
public class IntArrayIterator implements Iterator<Object> {
	private final int start;
	private final int amount;
	private final Object[] data;

	private int position = 0;

	/**
	 * Constructor to specify the data and the start and end to iterate over.
	 * 
	 * @param data
	 *            the data
	 * @param start
	 *            the start value
	 * @param amount
	 *            the amount of data to return
	 */
	public IntArrayIterator(final Object[] data, final int start,
			final int amount) {

		// check for valid values
		if (start < 0 || amount < 0 || data == null) {
			this.start = 0;
			this.amount = 0;
		} else {
			this.start = start;
			this.amount = amount;
		}

		this.data = data;
	}

	@Override
	public boolean hasNext() {
		return position < amount && getNextPos() < data.length;
	}

	private int getNextPos() {
		return start + position;
	}

	@Override
	public Object next() {
		final Object o = data[getNextPos()];
		position++;

		return o;
	}

	@Override
	public void remove() {
		throw new UnsupportedOperationException("Remove isn't supported.");
	}
};
