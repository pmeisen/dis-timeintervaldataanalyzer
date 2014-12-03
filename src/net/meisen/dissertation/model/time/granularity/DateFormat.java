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

	/**
	 * a year
	 */
	YEAR("yyyy", "yyyy", "yyyy", Calendar.YEAR),
	/**
	 * a month
	 */
	MONTH("MM", "yyyyMM", "MM.yyyy", Calendar.MONTH),
	/**
	 * a day
	 */
	DAY("dd", "yyyyMMdd", "dd.MM.yyyy", Calendar.DATE),
	/**
	 * a hour
	 */
	HOUR("HH", "yyyyMMdd_HH", "dd.MM.yyyy HH", Calendar.HOUR),
	/**
	 * a minute
	 */
	MINUTE("mm", "yyyyMMdd_HHmm", "dd.MM.yyyy HH:mm", Calendar.MINUTE),
	/**
	 * a second
	 */
	SECOND("ss", "yyyyMMdd_HHmmss", "dd.MM.yyyy HH:mm:ss", Calendar.SECOND),
	/**
	 * a millisecond
	 */
	MILLISECOND("SSS", "yyyyMMdd_HHmmss_SSS", "dd.MM.yyyy HH:mm:ss,SSS",
			Calendar.MILLISECOND);

	private final String format;
	private final String idFormat;
	private final String printFormat;
	private final int calendarValue;

	private DateFormat(final String format, final String idFormat,
			final String printFormat, final int calendarValue) {
		this.format = format;
		this.idFormat = idFormat;
		this.printFormat = printFormat;
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
		return Integer.parseInt(Dates.createStringFromDate(date, getFormat()));
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
					c.add(getCalendarIndicator(), (int) restMod);

					restMod = 0;
				} else {
					c.add(getCalendarIndicator(), Integer.MAX_VALUE);
					restMod -= Integer.MAX_VALUE;
				}
			}
		} else if (restMod < 0) {
			while (restMod < 0) {
				if (restMod >= Integer.MIN_VALUE) {
					c.add(getCalendarIndicator(), (int) restMod);

					restMod = 0;
				} else {
					c.add(getCalendarIndicator(), Integer.MIN_VALUE);
					restMod -= Integer.MIN_VALUE;
				}
			}
		}

		return c.getTime();
	}

	/**
	 * Gets the indicator specified for the format.
	 * 
	 * @return the indicator specified for the format
	 */
	public int getCalendarIndicator() {
		return calendarValue;
	}

	/**
	 * A format usable to create a unique identifier.
	 * 
	 * @return format usable to create a unique identifier
	 */
	public String getIdFormat() {
		return idFormat;
	}

	/**
	 * Format usable for printing.
	 * 
	 * @return a format usable for printing
	 */
	public String getPrintFormat() {
		return printFormat;
	}

	/**
	 * Gets a short-format.
	 * 
	 * @return a short-format of the date
	 */
	public String getFormat() {
		return format;
	}

}
