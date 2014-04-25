package net.meisen.dissertation.model.indexes;

import net.meisen.general.genmisc.types.Objects;

/**
 * A key which can be used for indexing
 * 
 * @author pmeisen
 * 
 * @param <T>
 *            the concrete implementation
 * 
 */
public abstract class IndexKey<T extends IndexKey<T>> implements Comparable<T> {

	@Override
	public abstract boolean equals(final Object o);

	@Override
	public abstract int hashCode();

	/**
	 * Get the values which make up the key.
	 * 
	 * @return the values which make up the key
	 */
	public abstract Object[] getValues();

	/**
	 * Helper implementation to compare to <i>random</i> objects.
	 * 
	 * @param o1
	 *            the object to be compared to {@code o2}
	 * @param o2
	 *            the object to be compared to {@code o1}
	 * 
	 * @return the result of the comparison, i.e.
	 *         <ul>
	 *         <li>{@code -1} if {@code o1 < o2}</li>
	 *         <li>{@code 00} if {@code o1 == o2}</li>
	 *         <li>{@code 01} if {@code o1 > o2}</li>
	 *         </ul>
	 */
	protected int compareObjects(final Object o1, final Object o2) {
		return Objects.compare(o1, o2);
	}
}
