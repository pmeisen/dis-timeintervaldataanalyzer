package net.meisen.dissertation.model.time.timeline;

import java.util.Date;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.time.DateNormalizer;
import net.meisen.dissertation.model.time.DateNormalizer.RoundType;
import net.meisen.dissertation.model.time.granularity.DateFormat;
import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.granularity.Year;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.genmisc.types.Objects;

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
	 * The position the value is used at
	 * 
	 * @author pmeisen
	 * 
	 */
	protected enum Position {
		/**
		 * The value is used as start of the timeline.
		 */
		START,
		/**
		 * The value is used as end of the timeline.
		 */
		END;
	}

	private final ITimeGranularity granularity;
	private final Class<?> type;

	private Object start;
	private Object end;

	/**
	 * The default constructor defines a {@code Timeline} which starts today
	 * (i.e. a truncated now) and ends in the next year using a granularity of
	 * day and the default timeZone ({@link Dates#GENERAL_TIMEZONE}).
	 */
	public TimelineDefinition() {
		this((String) null, (String) null, DefaultValues
				.getDefaultGranularityInstance());
		setDuration(OFFSET, Position.START, Year.instance());
	}

	/**
	 * Constructor to create a definition based on strings. The strings are
	 * passed to be {@code Long} or {@code Date} values and try to be
	 * interpreted as best as possible.
	 * 
	 * @param start
	 *            the start
	 * @param end
	 *            the end
	 * @param granularity
	 *            the unit of the values
	 */
	public TimelineDefinition(final String start, final String end,
			final ITimeGranularity granularity) {

		// check for nulls
		Object parsedStart = parse(start, null, Position.START, null);
		Object parsedEnd = parse(end, null, Position.END, null);

		// check if we have a hit yet
		if (parsedStart == null && parsedEnd == null) {
			throw new IllegalArgumentException("Unable to parse the start '"
					+ start + "' and end '" + end + "' value.");
		}
		// check if start is null
		else if (parsedStart == null) {
			parsedStart = parse(end, parsedEnd.getClass(), Position.START,
					parsedEnd);
		}
		// check if end is null
		else if (parsedEnd == null) {
			parsedEnd = parse(end, parsedStart.getClass(), Position.END,
					parsedStart);
		}
		// if end was null try to reparse it to be of the correct type
		else if (end == null && parsedEnd != null) {
			parsedEnd = parse(end, parsedStart.getClass(), Position.END,
					parsedStart);
		}
		// if start was null try to reparse it to be of the correct type
		else if (start == null && parsedStart != null) {
			parsedStart = parse(start, parsedEnd.getClass(), Position.START,
					parsedEnd);
		}

		// if we still have nulls we are done
		if (parsedStart == null && parsedEnd == null) {
			throw new IllegalArgumentException("Unable to parse the start '"
					+ start + "' and end '" + end + "' value.");
		} else {
			this.start = parsedStart;
			this.end = parsedEnd;
			this.granularity = granularity;

			/*
			 * check if we really have one type, otherwise we assume that one is
			 * a Date the other one is the duration
			 */
			if (!parsedEnd.getClass().equals(parsedStart.getClass())) {
				this.type = Date.class;

				if (Date.class.equals(parsedEnd.getClass())) {
					setDuration((Long) parsedStart, Position.END);
				} else {
					setDuration((Long) parsedEnd, Position.START);
				}
			} else {
				this.type = parsedStart.getClass();
			}
		}

		validate();
	}

	/**
	 * Constructor to create a definition based on byte values.
	 * 
	 * @param start
	 *            the start
	 * @param end
	 *            the end
	 * @param granularity
	 *            the unit of the values
	 */
	public TimelineDefinition(final Byte start, final Byte end,
			final ITimeGranularity granularity) {
		this(start == null ? null : start.longValue(), end == null ? null : end
				.longValue(), granularity);
	}

	/**
	 * Constructor to create a definition based on short values.
	 * 
	 * @param start
	 *            the start
	 * @param end
	 *            the end
	 * @param granularity
	 *            the unit of the values
	 */
	public TimelineDefinition(final Short start, final Short end,
			final ITimeGranularity granularity) {
		this(start == null ? null : start.longValue(), end == null ? null : end
				.longValue(), granularity);
	}

	/**
	 * Constructor to create a definition based on int values.
	 * 
	 * @param start
	 *            the start
	 * @param end
	 *            the end
	 * @param granularity
	 *            the unit of the values
	 */
	public TimelineDefinition(final Integer start, final Integer end,
			final ITimeGranularity granularity) {
		this(start == null ? null : start.longValue(), end == null ? null : end
				.longValue(), granularity);
	}

	/**
	 * Constructor to create a definition based on long values.
	 * 
	 * @param start
	 *            the start
	 * @param end
	 *            the end
	 * @param granularity
	 *            the unit of the values
	 */
	public TimelineDefinition(final Long start, final Long end,
			final ITimeGranularity granularity) {
		if (start == null) {
			this.start = getDef(Position.START,
					end == null ? null : new Date(end)).getTime();
		} else {
			this.start = start;
		}
		if (end == null) {
			this.end = getDef(Position.END,
					start == null ? null : new Date(start)).getTime();
		} else {
			this.end = end;
		}
		this.type = Long.class;
		this.granularity = granularity;

		validate();
	}

	/**
	 * Constructor to create a definition based on a start and end date.
	 * 
	 * @param start
	 *            the start
	 * @param end
	 *            the end
	 * @param granularity
	 *            the granularity to be used between the dates
	 */
	public TimelineDefinition(final Date start, final Date end,
			final ITimeGranularity granularity) {
		this(start, end, granularity, null);
	}

	/**
	 * Constructor to create a definition based on a start and end date.
	 * 
	 * @param start
	 *            the start
	 * @param end
	 *            the end
	 * @param granularity
	 *            the granularity to be used between the dates
	 * @param timeZone
	 *            the timezone the date-values are specified in, needed when one
	 *            of the values is {@code null} so that now can be determined
	 *            correctly
	 */
	public TimelineDefinition(final Date start, final Date end,
			final ITimeGranularity granularity, final String timeZone) {
		this.start = start == null ? getDef(Position.START, end) : start;
		this.end = end == null ? getDef(Position.END, start) : end;
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
			final Long start = getStart();
			final Long end = getEnd();

			if (start > end) {
				throw new IllegalArgumentException("The start '" + start
						+ "' cannot be larger than the end '" + end + "'");
			}
		} else {
			throw new IllegalArgumentException(
					"The type must be one of date or long.");
		}
	}

	/**
	 * Gets the type of the start and end value of the definition.
	 * 
	 * @return the start and end value of the definition
	 */
	public Class<?> getType() {
		return type;
	}

	/**
	 * Get the start of the definition. The type is as returned by
	 * {@link #getType()}.
	 * 
	 * @return the start of the definition
	 */
	@SuppressWarnings("unchecked")
	public <T> T getStart() {
		return (T) start;
	}

	/**
	 * Get the start of the definition. The type is as returned by
	 * {@link #getType()}.
	 * 
	 * @return the end of the definition
	 */
	@SuppressWarnings("unchecked")
	public <T> T getEnd() {
		return (T) end;
	}

	/**
	 * Gets the default value used for the specified {@code position}, based on
	 * the specified {@code pit}. If no {@code pit} is defined, i.e.
	 * {@code null} is set, the current date will be used.
	 * 
	 * @param position
	 *            the position to determine the default value for
	 * @param pit
	 *            the anchor from which the default should be calculated, if not
	 *            defined (i.e. {@code null} is set) the current time will be
	 *            used
	 * 
	 * @return the default date
	 */
	public Date getDef(final Position position, final Date pit) {

		if (pit == null) {
			final Date defPit = Dates.truncDate(
					Dates.now(Dates.GENERAL_TIMEZONE), Dates.GENERAL_TIMEZONE);

			if (Position.START.equals(position)) {
				return defPit;
			} else {
				return DateFormat.YEAR.modify(defPit, OFFSET);
			}
		} else {

			// get ten years +/- from pit
			if (Position.START.equals(position)) {
				return DateFormat.YEAR.modify(pit, -1 * OFFSET);
			} else {
				return DateFormat.YEAR.modify(pit, OFFSET);
			}
		}
	}

	/**
	 * Gets the default value used for the specified {@code position}, based on
	 * the specified {@code pit}. If no {@code pit} is defined, i.e.
	 * {@code null} is set, the {@code 0} will be used.
	 * 
	 * @param position
	 *            the position to determine the default value for
	 * @param pit
	 *            the anchor from which the default should be calculated, if not
	 *            defined (i.e. {@code null} is set) the {@code 0} will be used
	 * 
	 * @return the default date
	 */
	public Long getDef(Position position, final Long pit) {

		if (pit == null) {
			final long defPit = 0l;

			if (Position.START.equals(position)) {
				return defPit;
			} else {
				return defPit + OFFSET;
			}
		} else {

			// define the direction
			if (Position.START.equals(position)) {
				return pit - OFFSET;
			} else {
				return pit + OFFSET;
			}
		}
	}

	/**
	 * Parses the {@code value} to the {@code forced} class. If the
	 * {@code forced} class is {@code null} the class will be determined as
	 * {@code Date} or {@code Long} depending on the structure of the
	 * {@code value}.
	 * 
	 * @param value
	 *            the value to be parsed
	 * @param forced
	 *            the forced {@code class} of the {@code value}
	 * @param position
	 *            the {@code Position}, i.e. if it's the start or the end
	 * @param pit
	 *            the object to be aligned to, this is used if {@code value} is
	 *            {@code null} in that case the {@code objectPit} determines the
	 *            start to calculate the end or start from
	 * 
	 * @return the parsed object, or {@code null} if parsing wasn't possible
	 * 
	 * @see Dates#isDate(String)
	 */
	protected Object parse(final String value, final Class<?> forced,
			final Position position, final Object pit) {

		// check null
		if (value == null) {
			if (Long.class.equals(forced)) {
				if (pit instanceof Date) {
					return getDef(position, ((Date) pit).getTime());
				} else if (pit instanceof Long) {
					return getDef(position, (Long) pit);
				} else {
					return getDef(position, (Long) null);
				}
			} else {
				if (pit instanceof Long) {
					return getDef(position, new Date((Long) pit));
				} else if (pit instanceof Date) {
					return getDef(position, (Date) pit);
				} else {
					return getDef(position, (Date) null);
				}
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

	/**
	 * Calculates a new end or start of the definition based on the current
	 * start ({@code position} must be {@link Position#START}) or the current
	 * end ({@code position} must be {@link Position#END}) value plus (if
	 * {@code position} is {@code START}) or minus (if {@code position} is
	 * {@code END}) the duration.
	 * 
	 * @param duration
	 *            the duration, i.e. the amount of values to be moved from the
	 *            position
	 * @param position
	 *            the position to start the moving from
	 */
	public void setDuration(final long duration, final Position position) {
		setDuration(duration, position, getGranularity());
	}

	/**
	 * Calculates a new end or start of the definition based on the current
	 * start ({@code position} must be {@link Position#START}) or the current
	 * end ({@code position} must be {@link Position#END}) value plus (if
	 * {@code position} is {@code START}) or minus (if {@code position} is
	 * {@code END}) the duration.
	 * 
	 * @param duration
	 *            the duration, i.e. the amount of values to be moved from the
	 *            position
	 * @param position
	 *            the position to start the moving from
	 * @param durationGranularity
	 *            defines the {@code TimeGranularity} of the duration, only
	 *            applied if the {@link #getType()} is {@code Date}.
	 */
	public void setDuration(final long duration, final Position position,
			final ITimeGranularity durationGranularity) {
		if (duration < 0) {
			throw new IllegalArgumentException(
					"The duration must be a value larger or equal to 0.");
		}
		// if no duration is set nothing has to be done
		else if (duration == 0) {
			return;
		} else if (position == null) {
			throw new NullPointerException("The position cannot be null.");
		}

		// calculate the duration
		final DateNormalizer normalizer = DateNormalizer.instance();
		if (Date.class.equals(getType())) {

			if (Position.START.equals(position)) {
				this.end = normalizer.addDuration(this.<Date> getStart(),
						duration, durationGranularity, RoundType.FLOOR);
			} else if (Position.END.equals(position)) {
				this.start = normalizer.addDuration(this.<Date> getEnd(), -1l
						* duration, durationGranularity, RoundType.FLOOR);
			}
		} else {

			// just calculate the difference
			if (Position.START.equals(position)) {
				this.end = this.<Long> getStart() + duration;
			} else if (Position.END.equals(position)) {
				this.start = this.<Long> getEnd() - duration;
			}
		}

		validate();
	}
	
	/**
	 * Gets the defined granularity.
	 * 
	 * @return the defined granularity
	 */
	public ITimeGranularity getGranularity() {
		return granularity;
	}

	@Override
	public boolean equals(final Object o) {
		if (o == null) {
			return false;
		} else if (o == this) {
			return true;
		} else if (o instanceof TimelineDefinition) {
			final TimelineDefinition cmp = (TimelineDefinition) o;

			return Objects.equals(cmp.type, type)
					&& Objects.equals(cmp.granularity, granularity)
					&& Objects.equals(cmp.start, start)
					&& Objects.equals(cmp.end, end);
		} else {
			return false;
		}
	}

	@Override
	public String toString() {
		final String fStart, fEnd;
		if (Date.class.isAssignableFrom(getType())) {
			fStart = Dates.formatDate((Date) start, "dd.MM.yyyy HH:mm:ss,SSS",
					Dates.GENERAL_TIMEZONE);
			fEnd = Dates.formatDate((Date) end, "dd.MM.yyyy HH:mm:ss,SSS",
					Dates.GENERAL_TIMEZONE);
		} else {
			fStart = "" + start;
			fEnd = "" + end;
		}

		return "[" + fStart + ", " + fEnd + "] : " + granularity + " : "
				+ Dates.GENERAL_TIMEZONE;
	}
}
