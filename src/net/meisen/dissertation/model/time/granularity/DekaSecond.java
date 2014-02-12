package net.meisen.dissertation.model.time.granularity;

/**
 * 10 seconds
 */
public class DekaSecond implements ISecondBasedGranularity {
	private static final DekaSecond instance = new DekaSecond();

	private DekaSecond() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code DekaSecond}.
	 * 
	 * @return the one and only instance of a {@code DekaSecond}
	 */
	public static DekaSecond instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof DekaSecond) {
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
		return 10;
	}

	@Override
	public int expFractionOfSeconds() {
		return -1;
	}
}
