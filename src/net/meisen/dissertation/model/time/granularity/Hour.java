package net.meisen.dissertation.model.time.granularity;

/**
 * 60 minutes or 3,600 seconds
 */
public class Hour implements ISecondBasedGranularity {
	private static final Hour instance = new Hour();

	private Hour() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code Hour}.
	 * 
	 * @return the one and only instance of a {@code Hour}
	 */
	public static Hour instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof Hour) {
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
		return 3600;
	}

	@Override
	public int expFractionOfSeconds() {
		return -1;
	}
}
