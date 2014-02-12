package net.meisen.dissertation.model.time.granularity;

/**
 * 0.01 seconds
 */
public class CentiSecond implements ISecondBasedGranularity {
	private static final CentiSecond instance = new CentiSecond();

	private CentiSecond() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code CentiSecond}.
	 * 
	 * @return the one and only instance of a {@code CentiSecond}
	 */
	public static CentiSecond instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof CentiSecond) {
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
		return 2;
	}
}
