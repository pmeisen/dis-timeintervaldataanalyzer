package net.meisen.dissertation.model.time.granularity;

/**
 * 10^-15 seconds
 */
public class FemtoSecond implements ISecondBasedGranularity {
	private static final FemtoSecond instance = new FemtoSecond();

	private FemtoSecond() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code FemtoSecond}.
	 * 
	 * @return the one and only instance of a {@code FemtoSecond}
	 */
	public static FemtoSecond instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof FemtoSecond) {
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
		return 15;
	}
}
