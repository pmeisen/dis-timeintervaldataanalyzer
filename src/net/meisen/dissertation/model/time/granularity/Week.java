package net.meisen.dissertation.model.time.granularity;

/**
 * 7 days
 */
public class Week implements ISecondBasedGranularity {
	private static final Week instance = new Week();

	private Week() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code Week}.
	 * 
	 * @return the one and only instance of a {@code Week}
	 */
	public static Week instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof Week) {
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
		return 604800;
	}

	@Override
	public int expFractionOfSeconds() {
		return -1;
	}
}
