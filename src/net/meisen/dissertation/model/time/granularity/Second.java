package net.meisen.dissertation.model.time.granularity;

/**
 * 1 second
 */
public class Second implements ISecondBasedGranularity {
	private static final Second instance = new Second();

	private Second() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code Second}.
	 * 
	 * @return the one and only instance of a {@code Second}
	 */
	public static Second instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof Second) {
			return true;
		} else {
			return false;
		}
	}

	@Override
	public String toString() {
		return getClass().getSimpleName();
	}

	@Override
	public int seconds() {
		return 1;
	}

	@Override
	public int expFractionOfSeconds() {
		return -1;
	}
}
