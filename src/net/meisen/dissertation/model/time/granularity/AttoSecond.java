package net.meisen.dissertation.model.time.granularity;

/**
 * 10^-18 seconds
 */
public class AttoSecond implements ISecondBasedGranularity {
	private static final AttoSecond instance = new AttoSecond();

	private AttoSecond() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code AttoSecond}.
	 * 
	 * @return the one and only instance of a {@code AttoSecond}
	 */
	public static AttoSecond instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof AttoSecond) {
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
		return 18;
	}
}
