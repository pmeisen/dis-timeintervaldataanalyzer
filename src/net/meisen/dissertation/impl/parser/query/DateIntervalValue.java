package net.meisen.dissertation.impl.parser.query;

import java.util.Date;

/**
 * A {@code Date} used as a value within an interval.
 * 
 * @author pmeisen
 * 
 * @see Date
 * 
 */
public class DateIntervalValue extends BaseIntervalValue<Date> {

	/**
	 * Creates a {@code DateIntervalValue} for the specified {@code value}.
	 * 
	 * @param value
	 *            the {@code Date} of {@code this}
	 */
	public DateIntervalValue(final Date value) {
		super(value);
	}

	@Override
	public Class<Date> getType() {
		return Date.class;
	}
}
