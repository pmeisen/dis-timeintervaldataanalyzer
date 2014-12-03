package net.meisen.dissertation.model.time.granularity;

import java.util.Date;

/**
 * 0.001 seconds
 */
public class MilliSecond implements ISecondBasedGranularity,
		IDateBasedGranularity {
	private static final MilliSecond instance = new MilliSecond();

	private MilliSecond() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code MilliSecond}.
	 * 
	 * @return the one and only instance of a {@code MilliSecond}
	 */
	public static MilliSecond instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof MilliSecond) {
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
		return 3;
	}

	@Override
	public DateFormat getFormat() {
		return DateFormat.MILLISECOND;
	}

	@Override
	public long determineRepresentor(final Date date) {
		return date.getTime();
	}

	@Override
	public Date resolveRepresenter(final long value) {
		return new Date(value);
	}

	/**
	 * Gets the instance of the {@code DateBasedHelper} to be used.
	 * 
	 * @return the instance of the {@code DateBasedHelper} to be used
	 */
	public DateBasedHelper helper() {
		return helper;
	}

	@Override
	public boolean isAssignableTo(char identifier) {
		return helper().isAssignableTo(getIdentifier(), identifier);
	}

	@Override
	public char getIdentifier() {
		return 'i';
	}
}
