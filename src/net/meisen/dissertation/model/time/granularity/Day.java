package net.meisen.dissertation.model.time.granularity;

/**
 * 24 hours, 1,440 minutes or 86,400 seconds
 */
public class Day implements ISecondBasedGranularity {
	private static final Day instance = new Day();

	private Day() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code Day}.
	 * 
	 * @return the one and only instance of a {@code Day}
	 */
	public static Day instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof Day) {
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
		return 86400;
	}

	@Override
	public int expFractionOfSeconds() {
		return -1;
	}
}
