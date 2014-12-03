package net.meisen.dissertation.model.time.granularity;

import java.util.Date;

/**
 * 60 seconds
 */
public class Minute implements ISecondBasedGranularity, IDateBasedGranularity {
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

	@Override
	public DateFormat getFormat() {
		return DateFormat.MINUTE;
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
		return 'n';
	}
}
