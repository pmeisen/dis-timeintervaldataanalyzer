package net.meisen.dissertation.model.time.granularity;

/**
 * 10^-6 seconds
 */
public class MicroSecond implements ISecondBasedGranularity {
	private static final MicroSecond instance = new MicroSecond();

	private MicroSecond() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code MicroSecond}.
	 * 
	 * @return the one and only instance of a {@code MicroSecond}
	 */
	public static MicroSecond instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof MicroSecond) {
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
		return 6;
	}
}
