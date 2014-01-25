package net.meisen.dissertation.model.indexes.keys;

import net.meisen.dissertation.model.indexes.IndexKey;
import net.meisen.general.genmisc.types.Objects;

/**
 * A key which can be used for indexing several attributes of one {@code Object}
 * .
 * 
 * @author pmeisen
 * 
 */
public class CompositeIndexKey extends IndexKey<CompositeIndexKey> {

	private final Object[] values;

	/**
	 * Constructor to create a {@code CompositeIndexKey} with the specified
	 * value array.
	 * 
	 * @param values
	 *            the values of the key
	 */
	public CompositeIndexKey(final Object... values) {
		this.values = values;
	}

	/**
	 * Get a specific value of the key.
	 * 
	 * @param idx
	 *            the values to be retrieved (0-based)
	 * 
	 * @return the value at the specified position
	 */
	@SuppressWarnings("unchecked")
	public <T> T getValue(final int idx) {
		if (values == null) {
			throw new NullPointerException(
					"The values are null, therefore no value can be retrieved.");
		} else if (idx < values.length) {
			return (T) values[idx];
		} else {
			throw new IllegalArgumentException("The index '" + idx
					+ "' is out of bound (max. '" + values.length + "').");
		}
	}

	@Override
	public String toString() {
		String output = this.getClass().getName() + " (";
		String separator = "";
		for (final Object val : values) {
			output += separator + val.toString();
			if (separator.equals("")) {
				separator = ", ";
			}
		}
		output += ")";

		return output;
	}

	@Override
	public boolean equals(final Object o) {
		if (this == o) {
			return true;
		} else if (o == null) {
			return false;
		} else if (o instanceof CompositeIndexKey) {
			final CompositeIndexKey cmp = (CompositeIndexKey) o;

			// check the length
			if (cmp.values.length != values.length) {
				return false;
			}
			// check each object
			else {
				for (int i = 0; i < values.length; i++) {
					if (Objects.equals(values[i], cmp.values[i])) {
						continue;
					} else {
						return false;
					}
				}

				return true;
			}
		} else {
			return false;
		}
	}

	@Override
	public int hashCode() {
		return Objects.generateHashCode(1, 1, values);
	}

	@Override
	public int compareTo(final CompositeIndexKey key) {

		// first check null, null is always first
		if (values == null && key.values == null) {
			return 0;
		} else if (values == null) {
			return -1;
		} else if (key.values == null) {
			return 1;
		}

		// now check the amount of keys, more keys means prior to others
		final int thisSize = values.length;
		final int keySize = key.values.length;
		if (thisSize < keySize) {
			return -1;
		} else if (thisSize > keySize) {
			return 1;
		}

		// if we are equal stop here
		if (equals(key)) {
			return 0;
		}

		// finally we have the same amount of keys but the elements aren't equal
		for (int i = 0; i < thisSize; i++) {
			final Object thisValue = values[i];
			final Object keyValue = key.values[i];

			// check what we do if one of the keys is null
			final int cmp = compareObjects(thisValue, keyValue);
			if (cmp != 0) {
				return cmp;
			}
		}

		throw new IllegalStateException(
				"Objects aren't equal but a difference could not be found ('"
						+ this + "' comparedTo '" + key + "'");
	}

	@Override
	public Object[] getValues() {
		return values;
	}
}
