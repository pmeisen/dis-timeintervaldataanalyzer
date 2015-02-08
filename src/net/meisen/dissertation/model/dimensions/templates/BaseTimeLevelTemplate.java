package net.meisen.dissertation.model.dimensions.templates;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.TimeZone;

import net.meisen.dissertation.exceptions.TimeDimensionException;
import net.meisen.dissertation.model.data.IntervalModel;
import net.meisen.dissertation.model.dimensions.TimeLevelMember;
import net.meisen.dissertation.model.time.granularity.DateFormat;
import net.meisen.dissertation.model.time.granularity.IDateBasedGranularity;
import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Dates;

/**
 * Base implementation of a {@code TimeLevelTemplate} adding some helper methods
 * and the setting of the {@code IntervalModel}.
 * 
 * @author pmeisen
 * 
 */
public abstract class BaseTimeLevelTemplate implements ITimeLevelTemplate {
	/**
	 * Marker added as suffix to identifier to mark daylight-saving-time.
	 */
	public static final String DST_MARKER = "DST";

	/**
	 * Creates an iterator to iterate over the values of the specified
	 * {@code levelGranularity}.
	 * 
	 * @param model
	 *            the {@code IntervalModel} used
	 * @param timezone
	 *            the time-zone to be used
	 * @param levelGranularity
	 *            the granularity of the level to create the iterator for
	 * 
	 * @return an iterator useful for iteration
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the granularity does not support the iteration
	 */
	public Iterator<TimeLevelMember> createIterator(final IntervalModel model,
			final String timezone, final IDateBasedGranularity levelGranularity)
			throws ForwardedRuntimeException {
		final BaseMapper<?> mapper = model.getTimelineMapper();

		final long start = mapper.getNormStartAsLong();
		final long end = mapper.getNormEndAsLong();

		return createIterator(model, timezone, levelGranularity, start, end);
	}

	/**
	 * Creates an iterator to iterate over the values of the specified
	 * {@code levelGranularity}.
	 * 
	 * @param model
	 *            the {@code IntervalModel} used
	 * @param timezone
	 *            the time-zone to be used
	 * @param levelGranularity
	 *            the granularity of the level to create the iterator for
	 * @param s
	 *            the start value to start at
	 * @param e
	 *            the end value to stop the iteration at
	 * 
	 * @return an iterator useful for iteration
	 * 
	 * @throws ForwardedRuntimeException
	 *             if the granularity does not support the iteration
	 */
	public Iterator<TimeLevelMember> createIterator(final IntervalModel model,
			final String timezone,
			final IDateBasedGranularity levelGranularity, final long s,
			final long e) throws ForwardedRuntimeException {
		final BaseMapper<?> mapper = model.getTimelineMapper();
		final ITimeGranularity granularity = model.getTimelineDefinition()
				.getGranularity();

		// validate
		if (granularity instanceof IDateBasedGranularity == false) {
			throw new ForwardedRuntimeException(TimeDimensionException.class,
					1000, granularity.getClass().getSimpleName(), getClass()
							.getSimpleName());
		}

		/*
		 * Check if the granularity of the time-axis is less than day. If so, we
		 * have the chance to apply the time-zone. Otherwise, we have to work
		 * with UTC.
		 */
		final String tz;
		if (((IDateBasedGranularity) granularity).isAssignableTo('h')) {
			tz = timezone;
		} else {
			tz = Dates.GENERAL_TIMEZONE;
		}

		// determine the format to be used
		final DateFormat format = levelGranularity.getFormat();
		final boolean addDSTMarker = levelGranularity.isAssignableTo('h');
		final int indicator = format.getCalendarIndicator();

		// get the start and the end values

		final Date startDate = Dates.truncDate((Date) mapper.resolve(s), tz,
				indicator);
		final Date endDate = Dates.truncDate((Date) mapper.resolve(e), tz,
				indicator);

		// create the calendar
		final TimeZone tZone = TimeZone.getTimeZone(tz);
		final Calendar calendar = new GregorianCalendar();
		calendar.setTimeZone(tZone);
		calendar.setTime(startDate);

		// iterate over the days
		return new Iterator<TimeLevelMember>() {

			@Override
			public boolean hasNext() {
				return calendar.getTime().before(endDate)
						|| calendar.getTime().equals(endDate);
			}

			@Override
			public TimeLevelMember next() {

				// get the current and the next result
				final Date result = calendar.getTime();
				calendar.add(indicator, 1);
				final Date nextResult = calendar.getTime();

				// determine the start and end
				final long startRes = mapper.mapToLong(result);
				long endRes = mapper.mapToLong(nextResult);
				endRes = mapper.isLargerThanEnd(nextResult) ? endRes
						: endRes - 1;

				// create the id and the name
				String id, name;
				id = Dates.formatDate(result, format.getIdFormat(), tz);
				name = Dates.formatDate(result, format.getPrintFormat(), tz);
				if (addDSTMarker && tZone.inDaylightTime(result)) {
					id += "_" + DST_MARKER;
					name += " (" + DST_MARKER + ")";
				}

				final TimeLevelMember member = new TimeLevelMember(id,
						startRes, endRes);
				member.setName(name);
				return member;
			}

			@Override
			public void remove() {
				throw new IllegalStateException("Unsupported by this iterator.");
			}
		};
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		} else if (obj == null) {
			return false;
		} else if (getClass().equals(obj.getClass())) {
			final BaseTimeLevelTemplate template = (BaseTimeLevelTemplate) obj;
			return getId().equals(template.getId());
		} else {
			return false;
		}
	}
}
