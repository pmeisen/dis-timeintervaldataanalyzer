package net.meisen.dissertation.model.time.granularity;

/**
 * 60 seconds
 */
public class Minute implements ISecondBasedGranularity {
	private static final Minute instance = new Minute();

	private Minute() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code Minute}.
	 * 
	 * @return the one and only instance of a {@code Minute}
	 */
	public static Minute instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof Minute) {
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
		return 60;
	}

	@Override
	public int expFractionOfSeconds() {
		return -1;
	}
}
