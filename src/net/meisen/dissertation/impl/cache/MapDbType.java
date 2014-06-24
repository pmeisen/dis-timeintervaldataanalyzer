package net.meisen.dissertation.impl.cache;

public enum MapDbType {
	BTree, HashMap;

	public static MapDbType find(final String name) {
		for (final MapDbType type : values()) {
			if (type.name().equals(name)) {
				return type;
			}
		}

		return getDefault();
	}

	public static MapDbType getDefault() {
		return BTree;
	}
}
