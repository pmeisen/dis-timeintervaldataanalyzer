package net.meisen.dissertation.model.time.granularity;

/**
 * 0.1 seconds
 */
public class DeciSecond implements ISecondBasedGranularity {
	private static final DeciSecond instance = new DeciSecond();

	private DeciSecond() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code DeciSecond}.
	 * 
	 * @return the one and only instance of a {@code DeciSecond}
	 */
	public static DeciSecond instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof DeciSecond) {
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
		return -1;
	}

	@Override
	public int expFractionOfSeconds() {
		return 1;
	}
}
