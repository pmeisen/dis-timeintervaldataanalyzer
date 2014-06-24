package net.meisen.dissertation.impl.cache;

/**
 * The different available types of {@code MapDb} instances.
 * 
 * @author pmeisen
 * 
 */
public enum MapDbType {
	/**
	 * B-Tree-map
	 */
	BTree,
	/**
	 * Hash-map
	 */
	HashMap;

	/**
	 * Determines the type to be used for the specified {@code name}. The name
	 * is case-insensitive. If no matching {@code MapDbType} can be found, the
	 * default type is returned.
	 * 
	 * @param name
	 *            the name of the type to be used
	 * 
	 * @return the type to be used for the specified {@code name}
	 * 
	 * @see #getDefault()
	 */
	public static MapDbType find(final String name) {
		for (final MapDbType type : values()) {
			if (type.name().equals(name)) {
				return type;
			}
		}

		return getDefault();
	}

	/**
	 * Gets the default type to be used.
	 * 
	 * @return the default type to be used
	 */
	public static MapDbType getDefault() {
		return BTree;
	}
}
