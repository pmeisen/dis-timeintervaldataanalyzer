package net.meisen.dissertation.model.time.timeline;

import java.text.ParseException;
import java.util.Date;

import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.general.genmisc.types.Dates;

/**
 * A {@code TimelineDefinition} is used to define the timeline within the
 * system. That means where does time start and where does it end. Furthermore
 * it defines the granularity of the time.
 * 
 * @author pmeisen
 * 
 */
public class TimelineDefinition {
	/**
	 * The offset used if no date is defined as start ({@code -OFFSET}) or end (
	 * {@code +OFFSET}).
	 */
	public static final int OFFSET = 1;

	/**
	 * The position the date is used at
	 * 
	 * @author pmeisen
	 * 
	 */
	protected enum DatePosition {
		/**
		 * The date is used as start of the timeline.
		 */
		START,
		/**
		 * The date is used as end of the timeline.
		 */
		END;
	}

	private final Object start;
	private final Object end;
	private final ITimeGranularity granularity;

	private final Class<?> type;

	public TimelineDefinition(final String start, final String end,
			final ITimeGranularity granularity) {

		// check for nulls
		Object parsedStart = parse(start, null, DatePosition.START, null);
		Object parsedEnd = parse(end, null, DatePosition.END, null);

		// check if we have a hit yet
		if (parsedStart == null && parsedEnd == null) {
			throw new IllegalArgumentException("Unable to parse the start '"
					+ start + "' and end '" + end + "' value.");
		}
		// check if start is null
		else if (parsedStart == null) {
			parsedStart = parse(end, parsedEnd.getClass(), DatePosition.START,
					parsedEnd);
		}
		// check if end is null
		else if (parsedEnd == null) {
			parsedEnd = parse(end, parsedStart.getClass(), DatePosition.END,
					parsedStart);
		}
		// if end was null try to reparse it to be of the correct type
		else if (end == null && parsedEnd != null) {
			parsedEnd = parse(end, parsedStart.getClass(), DatePosition.END,
					parsedStart);
		}
		// if start was null try to reparse it to be of the correct type
		else if (start == null && parsedStart != null) {
				parsedStart = parse(start, parsedEnd.getClass(), DatePosition.START,
					parsedEnd);
		}
		// check if everything is fine
		else if (parsedEnd.getClass().equals(parsedStart.getClass())) {
			// everything is fine
		}

		// if we still have nulls we are done
		if (parsedStart == null && parsedEnd == null) {
			throw new IllegalArgumentException("Unable to parse the start '"
					+ start + "' and end '" + end + "' value.");
		} else {
			this.start = parsedStart;
			this.end = parsedEnd;
			this.type = parsedStart.getClass();
			this.granularity = granularity;
		}

		validate();
	}

	public TimelineDefinition(final Byte start, final Byte end,
			final ITimeGranularity granularity) {
		this(start == null ? null : start.longValue(), end == null ? null : end
				.longValue(), granularity);
	}

	public TimelineDefinition(final Short start, final Short end,
			final ITimeGranularity granularity) {
		this(start == null ? null : start.longValue(), end == null ? null : end
				.longValue(), granularity);
	}

	public TimelineDefinition(final Integer start, final Integer end,
			final ITimeGranularity granularity) {
		this(start == null ? null : start.longValue(), end == null ? null : end
				.longValue(), granularity);
	}

	public TimelineDefinition(final Long start, final Long end,
			final ITimeGranularity granularity) {
		if (start == null) {
			this.start = getDef(DatePosition.START,
					end == null ? null : new Date(end)).getTime();
		} else {
			this.start = start;
		}
		if (end == null) {
			this.end = getDef(DatePosition.END,
					start == null ? null : new Date(start)).getTime();
		} else {
			this.end = end;
		}
		this.type = Long.class;
		this.granularity = granularity;

		validate();
	}

	public TimelineDefinition(final Date start, final Date end,
			final ITimeGranularity granularity) {
		this.start = start == null ? getDef(DatePosition.START, end) : start;
		this.end = end == null ? getDef(DatePosition.END, start) : end;
		this.type = Date.class;
		this.granularity = granularity;

		validate();
	}

	private void validate() {

		// check null values
		if (getStart() == null) {
			throw new IllegalArgumentException(
					"The start value cannot be null.");
		} else if (getEnd() == null) {
			throw new IllegalArgumentException("The end value cannot be null.");
		} else if (getType() == null) {
			throw new IllegalArgumentException("The type cannot be null.");
		} else if (getGranularity() == null) {
			throw new IllegalArgumentException(
					"The granularity cannot be null.");
		}

		// check which one is larger
		if (Date.class.equals(getType())) {
			final Date start = getStart();
			final Date end = getEnd();

			if (start.getTime() > end.getTime()) {
				throw new IllegalArgumentException("The start '" + start
						+ "' cannot be larger than the end '" + end + "'");
			}
		} else if (Long.class.equals(getType())) {
			final long start = getStart();
			final long end = getEnd();

			if (start > end) {
				throw new IllegalArgumentException("The start '" + start
						+ "' cannot be larger than the end '" + end + "'");
			}
		} else {
			throw new IllegalArgumentException(
					"The type must be one of date or long.");
		}
	}

	public Class<?> getType() {
		return type;
	}

	@SuppressWarnings("unchecked")
	public <T> T getStart() {
		return (T) start;
	}

	@SuppressWarnings("unchecked")
	public <T> T getEnd() {
		return (T) end;
	}

	public Date getDef(final DatePosition datePosition, Date pit) {

		// get ten years +/- from pit
		pit = pit == null ? Dates.now() : pit;
		final int year = Integer.parseInt(Dates.formatDate(pit, "yyyy"));
		final String prefix = Dates.formatDate(pit, "dd.MM.");
		final String suffix = Dates.formatDate(pit, " HH:mm:ss,SSS");

		// determine the final date
		final String finalDate;
		if (DatePosition.START.equals(datePosition)) {
			finalDate = prefix + (year - OFFSET) + suffix;
		} else {
			finalDate = prefix + (year + OFFSET) + suffix;
		}

		final Date resDate;
		try {
			resDate = Dates.parseDate(finalDate, "dd.MM.yyyy HH:mm:ss,SSS");
		} catch (final ParseException e) {
			throw new IllegalStateException("This cannot be reached.", e);
		}

		return resDate;
	}

	protected Object parse(final String value, final Class<?> forced,
			final DatePosition datePosition, final Object objectPit) {

		// determine the pit to be used
		final Date pit;
		if (objectPit instanceof Date) {
			pit = (Date) objectPit;
		} else if (objectPit instanceof Long) {
			pit = new Date((Long) objectPit);
		} else {
			pit = null;
		}

		// check null
		if (value == null) {
			final Date resDate = getDef(datePosition, pit);
			if (Long.class.equals(forced)) {
				return resDate.getTime();
			} else {
				return resDate;
			}
		}

		// check for a date
		final Date date = Dates.isDate(value, Dates.GENERAL_TIMEZONE);
		if (date != null && Long.class.equals(forced)) {
			return date.getTime();
		} else if (date != null) {
			return date;
		}

		// check for a long
		final Long l;
		try {
			l = Long.parseLong(value);
		} catch (final NumberFormatException e) {
			// cannot be parsed as long
			return null;
		}
		if (l != null && Date.class.equals(forced)) {
			return new Date(l);
		} else if (l != null) {
			return l;
		}

		// if we came so far we have to stop the whole thing
		return null;
	}

	public ITimeGranularity getGranularity() {
		return granularity;
	}
}
