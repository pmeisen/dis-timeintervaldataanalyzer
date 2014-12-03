package net.meisen.dissertation.model.time.granularity;

import java.text.ParseException;
import java.util.Date;

import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.genmisc.types.Numbers;

/**
 * A {@code Month} is a {@code DateBasedGranularity}.
 * 
 * @author pmeisen
 * 
 * @see IDateBasedGranularity
 */
public class Month implements IDateBasedGranularity {
	private static final Month instance = new Month();

	private Month() {
		// just to make it private
	}

	/**
	 * Gets the one and only instance of a {@code Month}.
	 * 
	 * @return the one and only instance of a {@code Month}
	 */
	public static Month instance() {
		return instance;
	}

	@Override
	public boolean equals(final Object o) {
		if (o instanceof Month) {
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
	public long determineRepresentor(final Date date) {
		final long[] values = helper().getFormats(
				new DateFormat[] { DateFormat.YEAR, DateFormat.MONTH }, date);
		return 12 * values[0] + values[1];
	}

	@Override
	public Date resolveRepresenter(final long value) {
		final int month = Numbers.castToInt(value % 12);
		final int year = Numbers.castToInt(value / 12);

		try {
			return Dates.parseDate(month + "." + year, "MM.yyyy");
		} catch (final ParseException e) {
			throw new IllegalStateException("Unreachable point of code");
		}
	}

	@Override
	public DateFormat getFormat() {
		return DateFormat.MONTH;
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
		return 'm';
	}
}
