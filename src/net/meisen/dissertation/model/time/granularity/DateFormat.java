package net.meisen.dissertation.model.time.granularity;

import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import net.meisen.general.genmisc.types.Dates;

/**
 * The date information available for {@link Date} instances.
 * 
 * @author pmeisen
 * 
 */
public enum DateFormat {

	/***
	 * a year
	 */
	YEAR("yyyy", Calendar.YEAR), /***
	 * a month
	 */
	MONTH("MM", Calendar.MONTH), /***
	 * a day
	 */
	DAY("dd", Calendar.DATE), /***
	 * a hour
	 */
	HOUR("HH", Calendar.HOUR), /***
	 * a minute
	 */
	MINUTE("mm", Calendar.MINUTE), /***
	 * a second
	 */
	SECOND("ss", Calendar.SECOND), /***
	 * a millisecond
	 */
	MILLISECOND("SSS", Calendar.MILLISECOND);

	private final String format;
	private final int calendarValue;

	private DateFormat(final String format, final int calendarValue) {
		this.format = format;
		this.calendarValue = calendarValue;
	}

	/**
	 * Retrieves the value from the specified {@code date}.
	 * 
	 * @param date
	 *            the date to retrieve the value from
	 * @return the value
	 */
	public int getValue(final Date date) {
		return Integer.parseInt(Dates.createStringFromDate(date, format));
	}

	/**
	 * Modifies the specified {@code date} by adding the specified {@code mod}
	 * units of this {@code DateFormat} to the {@code date}.
	 * 
	 * @param date
	 *            the date to be modified
	 * @param mod
	 *            the number of units to be added (or subtracted if negative)
	 * 
	 * @return the resulting date
	 */
	public Date modify(final Date date, final long mod) {
		final Calendar c = Calendar.getInstance(TimeZone
				.getTimeZone(Dates.GENERAL_TIMEZONE));
		c.setTime(date);

		// we want to support long modifications
		long restMod = mod;

		if (restMod > 0) {
			while (restMod > 0) {
				if (restMod <= Integer.MAX_VALUE) {
					c.add(calendarValue, (int) restMod);

					restMod = 0;
				} else {
					c.add(calendarValue, Integer.MAX_VALUE);
					restMod -= Integer.MAX_VALUE;
				}
			}
		} else if (restMod < 0) {
			while (restMod < 0) {
				if (restMod >= Integer.MIN_VALUE) {
					c.add(calendarValue, (int) restMod);

					restMod = 0;
				} else {
					c.add(calendarValue, Integer.MIN_VALUE);
					restMod -= Integer.MIN_VALUE;
				}
			}
		}

		return c.getTime();
	}

}
