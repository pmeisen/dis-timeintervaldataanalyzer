package net.meisen.dissertation.model.time.granularity;

/**
 * 10^-12 seconds
 */
public class PicoSecond implements ISecondBasedGranularity {
	private static final PicoSecond instance = new PicoSecond();

	private PicoSecond() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code PicoSecond}.
	 * 
	 * @return the one and only instance of a {@code PicoSecond}
	 */
	public static PicoSecond instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof PicoSecond) {
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
		return 12;
	}
}
