package net.meisen.dissertation.impl.datasets;

import java.util.Date;
import java.util.TimeZone;

import net.meisen.general.genmisc.types.Dates;

/**
 * A {@code SingleStaticDataSetEntry} which is used within the configuration,
 * i.e. maps the dates to UTC.
 * 
 * @author pmeisen
 * 
 */
public class ConfiguredSingleStaticDataSetEntry extends
		SingleStaticDataSetEntry {

	/**
	 * Creates an entry with the specified {@code value}, {@code name} and
	 * {@code position}.
	 * 
	 * @param position
	 *            the position, whereby every value smaller than {@code 1} will
	 *            be mapped to {@code -1}.
	 * @param name
	 *            the name of the entry
	 * @param value
	 *            the value of the entry
	 */
	public ConfiguredSingleStaticDataSetEntry(final int position,
			final String name, final Object value) {
		super(position, name, value);
	}

	/**
	 * Method used to map the raw {code Date}-value to a valid {@code Date}
	 * value within the correct time-zone. A {@code Date} generated from the
	 * configuration is normally based on the system's time-zone, which is
	 * typically not UTC.
	 * 
	 * @param value
	 *            the value to be mapped
	 * 
	 * @return the mapped value
	 */
	@Override
	protected Date mapToTimezone(final Date value) {
		/*
		 * If it's a date map the date to UTC and assume it to be in the
		 * system's time-zone.
		 */
		return Dates.mapToTimezone(value, TimeZone.getDefault().getID(),
				Dates.GENERAL_TIMEZONE);
	}
}
