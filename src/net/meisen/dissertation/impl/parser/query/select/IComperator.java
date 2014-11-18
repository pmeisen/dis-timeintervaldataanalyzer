package net.meisen.dissertation.impl.parser.query.select;

/**
 * A {@code Comperator} is used to compare a string value (which might contain
 * wild-chars) with the value specified by the {@code Comperator}.
 * 
 * @author pmeisen
 * 
 */
public interface IComperator {

	/**
	 * Matches the specified {@code cmp} value with the defined comparator
	 * value. A match is given, if:
	 * <ul>
	 * <li>both values are {@code null}</li>
	 * <li>a wildchar is contained and {@code cmp} fulfills the regular
	 * expression</li>
	 * <li>no wildchar is contained and {@code cmp} is equal to the
	 * comparison-value</li>
	 * </ul>
	 * 
	 * @param cmp
	 *            the value to compare to
	 * 
	 * @return {@code true} if the {@code cmp} matches the comparison-value,
	 *         otherwise {@code false}
	 */
	public boolean matches(final String cmp);
}
