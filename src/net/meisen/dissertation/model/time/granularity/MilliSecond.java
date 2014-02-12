package net.meisen.dissertation.model.time.granularity;

/**
 * 0.001 seconds
 */
public class MilliSecond implements ISecondBasedGranularity {
	private static final MilliSecond instance = new MilliSecond();

	private MilliSecond() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code MilliSecond}.
	 * 
	 * @return the one and only instance of a {@code MilliSecond}
	 */
	public static MilliSecond instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof MilliSecond) {
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
		return 3;
	}
}
