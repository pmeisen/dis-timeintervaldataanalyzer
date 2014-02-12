package net.meisen.dissertation.model.time.granularity;

/**
 * 2 weeks, or 14 days
 */
public class FortNight implements ISecondBasedGranularity {
	private static final FortNight instance = new FortNight();

	private FortNight() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code FortNight}.
	 * 
	 * @return the one and only instance of a {@code FortNight}
	 */
	public static FortNight instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof FortNight) {
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
		return 1209600;
	}

	@Override
	public int expFractionOfSeconds() {
		return -1;
	}
}
