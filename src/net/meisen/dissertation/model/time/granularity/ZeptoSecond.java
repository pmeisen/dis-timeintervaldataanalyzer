package net.meisen.dissertation.model.time.granularity;

/**
 * 10^-21 seconds
 */
public class ZeptoSecond implements ISecondBasedGranularity {
	private static final ZeptoSecond instance = new ZeptoSecond();

	private ZeptoSecond() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code ZeptoSecond}.
	 * 
	 * @return the one and only instance of a {@code ZeptoSecond}
	 */
	public static ZeptoSecond instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof ZeptoSecond) {
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
		return 21;
	}
}
