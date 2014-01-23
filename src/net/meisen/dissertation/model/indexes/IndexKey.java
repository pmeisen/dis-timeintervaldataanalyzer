package net.meisen.dissertation.model.indexes;

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

		if (o1 == null && o2 == null) {
			return 0;
		} else if (o1 == null) {
			return -1;
		} else if (o2 == null) {
			return 1;
		} else {

			// get the classes
			final Class<?> o1Class = o1.getClass();
			final Class<?> o2Class = o2.getClass();
			if (o1 instanceof Comparable && o2 instanceof Comparable
					&& o1Class.equals(o2Class)) {

				@SuppressWarnings("unchecked")
				final Comparable<Comparable<?>> cmp1 = (Comparable<Comparable<?>>) o1;
				@SuppressWarnings("unchecked")
				final Comparable<Comparable<?>> cmp2 = (Comparable<Comparable<?>>) o2;

				// decide which comparison is used
				return cmp1.compareTo(cmp2);
			}

			// so the values aren't null and not comparable, so we have to do
			// some other comparison, which cannot lead to be equal
			final int cmpClass = o1Class.getName().compareTo(o2Class.getName());
			if (cmpClass != 0) {
				return cmpClass;
			}

			// both classes are of the same type but still they aren't equal
			// so let's use the string comparison of both
			final String o1String = o1.toString();
			final String o2String = o2.toString();
			final int cmpString = o1String.compareTo(o2String);
			if (cmpString != 0) {
				return cmpString;
			}

			// so last but not least, we just cannot do a lot and this has
			// probably side effects
			return o1.hashCode() < o2.hashCode() ? -1 : 1;
		}
	}
}
