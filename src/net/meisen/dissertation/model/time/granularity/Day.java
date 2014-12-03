package net.meisen.dissertation.model.time.granularity;

import java.util.Date;

/**
 * 24 hours, 1,440 minutes or 86,400 seconds
 */
public class Day implements ISecondBasedGranularity, IDateBasedGranularity {
	private static final Day instance = new Day();

	private Day() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code Days}.
	 * 
	 * @return the one and only instance of a {@code Days}
	 */
	public static Day instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof Day) {
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
		return 86400;
	}

	@Override
	public int expFractionOfSeconds() {
		return -1;
	}

	@Override
	public DateFormat getFormat() {
		return DateFormat.DAY;
	}

	@Override
	public long determineRepresentor(final Date date) {
		return Math.round(Math.floor(date.getTime() / (seconds() * 1000.0)));
	}

	@Override
	public Date resolveRepresenter(final long value) {
		return new Date(value * seconds() * 1000);
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
		return 'd';
	}
}
