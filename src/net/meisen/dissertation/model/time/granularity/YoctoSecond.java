package net.meisen.dissertation.model.time.granularity;

/**
 * 10^-24 seconds
 */
public class YoctoSecond implements ISecondBasedGranularity {
	private static final YoctoSecond instance = new YoctoSecond();

	private YoctoSecond() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code YoctoSecond}.
	 * 
	 * @return the one and only instance of a {@code YoctoSecond}
	 */
	public static YoctoSecond instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof YoctoSecond) {
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
		return 24;
	}
}
