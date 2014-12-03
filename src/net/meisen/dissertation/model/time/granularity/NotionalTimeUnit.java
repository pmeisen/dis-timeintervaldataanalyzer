package net.meisen.dissertation.model.time.granularity;

/**
 * A {@code NotionalTimeUnit} is a not further specified time-unit, i.e. can be
 * anything from a {@code MilliSecond} to a {@code Days}.
 * 
 * @author pmeisen
 * 
 */
public class NotionalTimeUnit implements ITimeGranularity {
	private static final NotionalTimeUnit instance = new NotionalTimeUnit();

	private NotionalTimeUnit() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code NotionalTimeUnit}.
	 * 
	 * @return the one and only instance of a {@code NotionalTimeUnit}
	 */
	public static NotionalTimeUnit instance() {
		return instance;
	}

	@Override
	public String toString() {
		return getClass().getSimpleName();
	}
}
