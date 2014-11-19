package net.meisen.dissertation.impl.parser.query.select;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 
 * @author pmeisen
 * 
 */
public class DescriptorValue {
	private final static Pattern maskPattern = Pattern
			.compile("^\\*|([^\\\\])\\*");
	private final static Pattern replacePattern = Pattern
			.compile("(?:^\\*|\\\\\\*)");

	private String rawValue;
	private String value;
	private boolean containsWildchar;

	/**
	 * Constructor to create a {@code DescriptorValue} with the specified
	 * {@code value}.
	 * 
	 * @param value
	 *            the actual defined value or a regular expression if a
	 *            wildchars was contained in the comparison-value set by
	 *            {@link #setValue(String)}
	 */
	public DescriptorValue(final String value) {
		setValue(value);
	}

	/**
	 * The raw value, i.e. the none modified input used for
	 * {@link #setValue(String)}.
	 * 
	 * @return the raw value
	 */
	public String getRawValue() {
		return rawValue;
	}

	/**
	 * Gets the comparison-value, which might be a regular expression, if
	 * wildchars are contained (see {@link #containsWildchar()}).
	 * 
	 * @return the actual defined value or a regular expression if a wildchars
	 *         was contained in the comparison-value set by
	 *         {@link #setValue(String)}
	 */
	public String getValue() {
		return value;
	}

	/**
	 * Sets the comparison-value, which might contain wildchars (i.e. {@code *})
	 * and quoted wildchars (i.e. {@code \*}).
	 * 
	 * @param value
	 *            the comparison-value to be set
	 */
	public void setValue(final String value) {
		this.rawValue = value;

		if (value == null) {
			this.containsWildchar = false;
		} else {
			final Matcher m = maskPattern.matcher(value);
			this.containsWildchar = m.find();

			if (this.containsWildchar) {

				// create a regular expression for the wildchars
				final StringBuffer sb = new StringBuffer();
				do {
					m.appendReplacement(sb, "$1.*");
				} while (m.find());
				m.appendTail(sb);

				// keep the quoted stars, those are needed in regExp
				this.value = sb.toString();
			} else {
				this.value = replacePattern.matcher(value).replaceAll("*");
			}
		}
	}

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
	public boolean matches(final String cmp) {
		if (cmp == null && rawValue == null) {
			return true;
		} else if (cmp == null || rawValue == null) {
			// * includes null as well
			return usesAllMatcher();
		} else if (containsWildchar) {
			return cmp.matches(value);
		} else {
			return cmp.equals(value);
		}
	}

	/**
	 * Checks if the value matches every value, i.e. if it uses a general mask.
	 * 
	 * @return {@code true} if all values are selected by {@code this}
	 */
	public boolean usesAllMatcher() {
		return containsWildchar() && ".*".equals(getValue());
	}

	/**
	 * This method is used to check if the comparison-value contains any
	 * wildchars (i.e. {@code *}). If so {@code true} is returned, otherwise
	 * {@code false}.
	 * 
	 * @return {@code true} if the comparison-value contains wildchars,
	 *         otherwise {@code false}
	 */
	public boolean containsWildchar() {
		return containsWildchar;
	}

	@Override
	public String toString() {
		return rawValue + " (" + value + ")";
	}
}
