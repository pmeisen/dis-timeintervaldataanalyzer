package net.meisen.dissertation.model.time.granularity;

/**
 * 10^-9 seconds
 */
public class NanoSecond implements ISecondBasedGranularity {
	private static final NanoSecond instance = new NanoSecond();

	private NanoSecond() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code NanoSecond}.
	 * 
	 * @return the one and only instance of a {@code NanoSecond}
	 */
	public static NanoSecond instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof NanoSecond) {
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
		return 9;
	}
}
