package net.meisen.dissertation.model.indexes.mock;

import net.meisen.dissertation.model.indexes.IndexedCollectionIdResolver;

/**
 * An {@code IndexedCollectionIdResolver} which returns an increased value for
 * each call.
 * 
 * @author pmeisen
 * 
 */
public class ToIntIdResolver implements IndexedCollectionIdResolver<Integer> {
	private int i = 0;
	private Class<?> expected;

	/**
	 * Constructor specifying the {@code expected} type of objects.
	 * 
	 * @param expected
	 *            the type of objects to be expected
	 */
	public ToIntIdResolver(final Class<?> expected) {
		this.expected = expected;
	}

	/**
	 * Reset the resolver to start with 0 again.
	 * 
	 * @return the next value which would be used
	 */
	public int reset() {
		final int val = i;
		i = 0;
		return val;
	}

	@Override
	public Integer getId(final Object object) {
		if (expected.isAssignableFrom(object.getClass())) {
			return i++;
		} else {
			throw new IllegalArgumentException("Must be a '"
					+ expected.getClass().getName() + "'");
		}
	}

	@Override
	public Class<Integer> getIdType() {
		return Integer.class;
	}
};
