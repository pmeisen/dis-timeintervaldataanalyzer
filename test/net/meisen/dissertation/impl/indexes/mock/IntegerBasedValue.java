package net.meisen.dissertation.impl.indexes.mock;

import java.util.UUID;

/**
 * Helper method which has a unique int-id.
 * 
 * @author pmeisen
 * 
 */
public class IntegerBasedValue {
	/**
	 * The next id which will be used.
	 */
	public static int nextVal = 0;

	private int id;
	private String uuid;

	/**
	 * The constructor
	 */
	public IntegerBasedValue() {
		this.id = nextVal;
		this.uuid = UUID.randomUUID().toString();

		nextVal++;
	}

	/**
	 * Get the identifier.
	 * 
	 * @return the identifier
	 */
	public int getId() {
		return id;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof IntegerBasedValue) {
			return ((IntegerBasedValue) o).id == id
					&& ((IntegerBasedValue) o).uuid.equals(uuid);
		} else {
			return false;
		}
	}

	/**
	 * Resets the next identifier.
	 */
	public static void reset() {
		reset(0);
	}

	/**
	 * Resets the next identifier to the specified {@code next}.
	 * 
	 * @param next
	 *            the next identifier to be created
	 */
	public static void reset(final int next) {
		nextVal = next;
	}
}
