package net.meisen.dissertation.impl.indexes;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import net.meisen.dissertation.model.indexes.IRangeQueryOptimized;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.BaseIndexedCollection;
import net.meisen.general.genmisc.types.Numbers;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A collection which uses the position within an array as key. This
 * implementation should only be used if the data is distributed across the
 * whole data-range.
 * 
 * @author pmeisen
 * 
 */
public class IntArrayCollection extends BaseIndexedCollection implements
		IRangeQueryOptimized {
	private final static Logger LOG = LoggerFactory
			.getLogger(IntArrayCollection.class);
	private final static int MAX_END = Integer.MAX_VALUE - 1;

	private Object[] index;

	/**
	 * Constructor to create an {@code ArrayCollection} for the specified
	 * {@code key}.
	 * 
	 * @param key
	 *            the definition to create the {@code ArrayCollection} for
	 */
	public IntArrayCollection(final IndexKeyDefinition key) {
		this(key, 0);
	}

	/**
	 * Constructor to create an {@code ArrayCollection} for the specified
	 * {@code key}.
	 * 
	 * @param key
	 *            the definition to create the {@code ArrayCollection} for
	 * @param maxSize
	 *            the expected maximal size, i.e. key-value to be added
	 */
	public IntArrayCollection(final IndexKeyDefinition key, final int maxSize) {
		super(key);

		// create an array of zero size
		setMaxValue(maxSize);
	}

	@Override
	public void setMaxValue(final long max) {

		// get the new max
		final int _max = Numbers.castToInt(max + 1);

		// preserve the old index and create the new one
		final Object[] oldIndex = index;
		index = new Object[_max];

		// add the old values to the array
		if (oldIndex != null) {
			for (int i = 0; i < Math.min(_max, oldIndex.length); i++) {
				index[i] = oldIndex[i];
			}
		}
	}

	@Override
	public Object[] getObjectsByStartAndEnd(final Object start, final Object end) {
		final int s = cast(start);
		final int e = cast(end);

		if (s > e) {
			return new Object[] {};
		} else {
			return Arrays.copyOfRange(index, s, Math.min(index.length, e + 1));
		}
	}

	@Override
	public Object[] getObjectsByRange(final Object start, final long amount) {
		final int s = cast(start);
		final long e = s + (amount - 1);
		if (e > MAX_END) {
			return getObjectsByStartAndEnd(s, MAX_END);
		} else {
			return getObjectsByStartAndEnd(s, (int) e);
		}
	}

	@Override
	public IntArrayIterator iterateByStartAndEnd(final Object start,
			final Object end) {
		final int s = cast(start);
		final int e = cast(end);

		return createIterator(s, e - s + 1);
	}

	@Override
	public IntArrayIterator iterateByRange(final Object start, final long amount) {
		final int s = cast(start);
		if (amount > MAX_END) {
			return createIterator(s, MAX_END);
		} else {
			return createIterator(s, (int) amount);
		}
	}

	private IntArrayIterator createIterator(final int start, final int amount) {
		return new IntArrayIterator(index, start, amount);
	}

	@Override
	public void removeAll() {
		index = new Object[index.length];
	}

	@Override
	public boolean addObject(final Object object) {
		final int pos = castKey(object);

		if (pos >= index.length) {
			if (LOG.isInfoEnabled()) {
				LOG.info("The size of the ArrayCollection had to be extended (from "
						+ index.length
						+ " to "
						+ (pos + 1)
						+ "). Because of performance reasons it makes sense to change the maximum value programmatically prior to adding values.");
			}
			setMaxValue(pos + 1);
			return true;
		} else if (index[pos] == null) {
			index[pos] = object;
			return true;
		} else {
			return false;
		}
	}

	@Override
	public void removeObject(final Object object) {
		index[castKey(object)] = null;
	}

	/**
	 * Get the object at the specified {@code pos}.
	 * 
	 * @param pos
	 *            the position to retrieve the object for
	 * 
	 * @return the object at the specified position
	 */
	public Object getObject(final int pos) {

		if (pos > -1 && pos < index.length) {
			return index[pos];
		} else {
			return null;
		}
	}

	@Override
	public Object getObject(final Object... values) {
		return getObject(cast(values[0]));
	}

	@Override
	public int size() {
		int count = 0;
		for (int i = 0; i < index.length; i++) {
			if (index[i] != null) {
				count++;
			}
		}
		return count;
	}

	@Override
	public Collection<?> getAll() {
		final List<Object> list = new ArrayList<Object>();
		for (int i = 0; i < index.length; i++) {
			if (index[i] != null) {
				list.add(index[i]);
			}
		}
		return list;
	}

	private int cast(final Object value) {
		final Class<?> clazz = value.getClass();

		if (Integer.class.equals(clazz)) {
			return (Integer) value;
		} else if (Short.class.equals(clazz)) {
			return Numbers.castToInt((Short) value);
		} else if (Byte.class.equals(clazz)) {
			return Numbers.castToInt((Byte) value);
		} else if (Long.class.equals(clazz)) {
			return Numbers.castToInt((Long) value);
		} else {
			throw new IllegalArgumentException("Unsupported value '" + value
					+ "' cannot be converted.");
		}
	}

	private int castKey(final Object object) {
		return cast(getKeyDefinition().getObjectKey(object));
	}

	@Override
	public boolean containsObject(final Object object) {
		return getObject(castKey(object)) != null;
	}

	@Override
	public long getMinValue() {
		return 0;
	}

	@Override
	public long getMaxValue() {
		return index.length - 1;
	}
}
