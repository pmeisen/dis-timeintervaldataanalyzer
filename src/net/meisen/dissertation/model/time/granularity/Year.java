package net.meisen.dissertation.model.time.granularity;

import java.text.ParseException;
import java.util.Date;

import net.meisen.general.genmisc.types.Dates;

/**
 * A {@code Year} is a {@code DateBasedGranularity}.
 * 
 * @author pmeisen
 * 
 * @see IDateBasedGranularity
 */
public class Year implements IDateBasedGranularity {
	private static final Year instance = new Year();

	private Year() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code Year}.
	 * 
	 * @return the one and only instance of a {@code Year}
	 */
	public static Year instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof Year) {
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
	public String[] getFormat() {
		return new String[] { "yyyy" };
	}

	@Override
	public long determineRepresentor(final long[] values) {
		return values[0];
	}

	@Override
	public Date resolveRepresenter(final long value) {
		try {
			return Dates.parseDate("" + value, "yyyy");
		} catch (final ParseException e) {
			throw new IllegalStateException("Unreachable point of code");
		}
	}
}
