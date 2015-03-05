package net.meisen.dissertation.impl.time.mapper;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.text.ParseException;
import java.util.Date;
import java.util.Iterator;

import net.meisen.dissertation.impl.parser.query.DateIntervalValue;
import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.IntervalType;
import net.meisen.dissertation.model.time.granularity.Day;
import net.meisen.dissertation.model.time.granularity.FortNight;
import net.meisen.dissertation.model.time.granularity.Hour;
import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.granularity.MicroSecond;
import net.meisen.dissertation.model.time.granularity.MilliSecond;
import net.meisen.dissertation.model.time.granularity.Minute;
import net.meisen.dissertation.model.time.granularity.Month;
import net.meisen.dissertation.model.time.granularity.NanoSecond;
import net.meisen.dissertation.model.time.granularity.NotionalTimeUnit;
import net.meisen.dissertation.model.time.granularity.Second;
import net.meisen.dissertation.model.time.granularity.Week;
import net.meisen.dissertation.model.time.granularity.Year;
import net.meisen.dissertation.model.time.mapper.BaseMapper;
import net.meisen.general.genmisc.types.Dates;

import org.junit.Test;

/**
 * Tests the implementation of a {@code DateMapper}.
 * 
 * @author pmeisen
 * 
 * @see DateMapper
 * 
 */
public class TestDateMapper {

	/**
	 * Tests the {@code DataMapper} for some {@code Granularities}.
	 * 
	 * @throws ParseException
	 *             if a number cannot be parsed
	 */
	@Test
	public void testSecondBasedStartAndEnd() throws ParseException {
		Date start, end;

		// create two test dates (start and end)
		start = Dates.parseDate("20.01.1981 08:07:12,123",
				"dd.MM.yyyy HH:mm:ss,SSS");
		end = Dates.parseDate("20.01.2181 08:07:55,987",
				"dd.MM.yyyy HH:mm:ss,SSS");

		//@formatter:off
		assertResult(start, end, NanoSecond.instance(),
				Dates.parseDate("20.01.1981 08:07:12,123", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("20.01.2181 08:07:55,987", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, MicroSecond.instance(),
				Dates.parseDate("20.01.1981 08:07:12,123", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("20.01.2181 08:07:55,987", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, MilliSecond.instance(),
				Dates.parseDate("20.01.1981 08:07:12,123", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("20.01.2181 08:07:55,987", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, Second.instance(),
				Dates.parseDate("20.01.1981 08:07:12,000", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("20.01.2181 08:07:55,000", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, Minute.instance(),
				Dates.parseDate("20.01.1981 08:07:00,000", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("20.01.2181 08:07:00,000", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, Hour.instance(),
				Dates.parseDate("20.01.1981 08:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("20.01.2181 08:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, Day.instance(),
				Dates.parseDate("20.01.1981 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("20.01.2181 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, Week.instance(),
				Dates.parseDate("15.01.1981 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("18.01.2181 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, FortNight.instance(),
				Dates.parseDate("15.01.1981 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("18.01.2181 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"));
		//@formatter:on

		// another test set
		start = Dates.parseDate("01.01.2000 23:59:59,999",
				"dd.MM.yyyy HH:mm:ss,SSS");
		end = Dates.parseDate("01.02.2000 23:59:59,999",
				"dd.MM.yyyy HH:mm:ss,SSS");

		//@formatter:off
		assertResult(start, end, NanoSecond.instance(),
				Dates.parseDate("01.01.2000 23:59:59,999", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("01.02.2000 23:59:59,999", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, MicroSecond.instance(),
				Dates.parseDate("01.01.2000 23:59:59,999", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("01.02.2000 23:59:59,999", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, MilliSecond.instance(),
				Dates.parseDate("01.01.2000 23:59:59,999", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("01.02.2000 23:59:59,999", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, Second.instance(),
				Dates.parseDate("01.01.2000 23:59:59,000", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("01.02.2000 23:59:59,000", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, Minute.instance(),
				Dates.parseDate("01.01.2000 23:59:00,000", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("01.02.2000 23:59:00,000", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, Hour.instance(),
				Dates.parseDate("01.01.2000 23:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("01.02.2000 23:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, Day.instance(),
				Dates.parseDate("01.01.2000 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("01.02.2000 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, Week.instance(),
				Dates.parseDate("30.12.1999 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("27.01.2000 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"));
		assertResult(start, end, FortNight.instance(),
				Dates.parseDate("23.12.1999 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("20.01.2000 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"));
		//@formatter:on
	}

	/**
	 * Test the parsing of a now {@code ISecondBasedGranularity}.
	 * 
	 * @throws ParseException
	 *             if a number cannot be parsed
	 */
	@Test
	public void testDateBasedStartAndEnd() throws ParseException {
		final Date start = Dates.parseDate("20.01.1981 08:07:12,123",
				"dd.MM.yyyy HH:mm:ss,SSS");
		final Date end = Dates.parseDate("20.01.2181 08:07:55,987",
				"dd.MM.yyyy HH:mm:ss,SSS");

		//@formatter:off
		assertResult(start, end, Year.instance(), 
				Dates.parseDate("01.01.1981 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("01.01.2181 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"));
		
		assertResult(start, end, Month.instance(), 
				Dates.parseDate("01.01.1981 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"),
				Dates.parseDate("01.01.2181 00:00:00,000", "dd.MM.yyyy HH:mm:ss,SSS"));
		//@formatter:on
	}

	/**
	 * Test the usage of any other {@code TimeGranularity}, which isn't more
	 * specified.
	 * 
	 * @throws ParseException
	 *             if a number cannot be parsed
	 */
	@Test
	public void testOtherStartAndEnd() throws ParseException {
		final Date start = Dates.parseDate("20.01.1981 08:07:12,123",
				"dd.MM.yyyy HH:mm:ss,SSS");
		final Date end = Dates.parseDate("20.01.2181 08:07:55,987",
				"dd.MM.yyyy HH:mm:ss,SSS");

		// test a NotionalTimeUnit
		DateMapper mapper;
		mapper = new DateMapper(start, end, NotionalTimeUnit.instance());
		assertEquals(start.getTime(), mapper.getStart());
		assertEquals(end.getTime(), mapper.getEnd());
	}

	/**
	 * Tests the implementation of a {@code DateMapper} using a interval range
	 * within 127, i.e. in {@code byte} range.
	 * 
	 * @throws ParseException
	 *             if a date is invalid
	 */
	@Test
	public void testToByteMapper() throws ParseException {
		final DateMapper mapper;
		final Date start, end;

		// test byte range
		start = Dates.parseDate("20.01.1981 00:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS");
		end = Dates.parseDate("20.01.1981 10:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS");
		mapper = new DateMapper(start, end, Hour.instance());
		assertEquals(Byte.class, mapper.getTargetType());
		assertEquals(10, mapper.getNormEndAsByte());
		assertEquals(0, mapper.getNormStartAsByte());

		// check the results for mapping
		byte resByte;
		resByte = mapper.mapToByte(Dates.parseDate("20.01.1981 00:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(0, resByte);
		resByte = mapper.mapToByte(Dates.parseDate("20.01.1981 10:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(10, resByte);
		resByte = mapper.mapToByte(Dates.parseDate("20.01.1981 20:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(mapper.getNormEndAsByte(), resByte);
		resByte = mapper.mapToByte(Dates.parseDate("20.01.2081 20:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(mapper.getNormEndAsByte(), resByte);
		resByte = mapper.mapToByte(Dates.parseDate("20.01.1970 20:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(mapper.getNormStartAsByte(), resByte);
	}

	/**
	 * Tests the implementation of a {@code DateMapper} using a interval range
	 * in {@code short} range.
	 * 
	 * @throws ParseException
	 *             if a date is invalid
	 */
	@Test
	public void testToShortMapper() throws ParseException {
		final DateMapper mapper;
		final Date start, end;

		// test byte range
		start = Dates.parseDate("01.01.1900 00:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS");
		end = Dates.parseDate("31.12.1900 23:59:59,999",
				"dd.MM.yyyy HH:mm:ss,SSS");
		mapper = new DateMapper(start, end, Day.instance());
		assertEquals(Short.class, mapper.getTargetType());
		assertEquals(364, mapper.getNormEndAsShort());
		assertEquals(0, mapper.getNormStartAsShort());

		// check the results for mapping
		short resShort;
		resShort = mapper.mapToShort(Dates.parseDate("01.01.1900 00:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(0, resShort);
		resShort = mapper.mapToShort(Dates.parseDate("31.12.1900 23:59:59,999",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(364, resShort);
		resShort = mapper.mapToShort(Dates.parseDate("28.02.1900 00:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(58, resShort);
		resShort = mapper.mapToShort(Dates.parseDate("20.01.2081 20:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(mapper.getNormEndAsShort(), resShort);
		resShort = mapper.mapToShort(Dates.parseDate("20.01.1800 20:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(mapper.getNormStartAsShort(), resShort);
	}

	/**
	 * Tests the implementation of a {@code DateMapper} using a interval range
	 * in {@code integer} range.
	 * 
	 * @throws ParseException
	 *             if a date is invalid
	 */
	@Test
	public void testToIntMapper() throws ParseException {
		final DateMapper mapper;
		final Date start, end;

		// test byte range
		start = Dates.parseDate("01.01.1900 00:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS");
		end = Dates.parseDate("31.12.2000 23:59:59,999",
				"dd.MM.yyyy HH:mm:ss,SSS");
		mapper = new DateMapper(start, end, Day.instance());
		assertEquals(Integer.class, mapper.getTargetType());
		assertEquals(36889, mapper.getNormEndAsInt());
		assertEquals(0, mapper.getNormStartAsInt());

		// check the results for mapping
		int resInt;
		resInt = mapper.mapToInt(Dates.parseDate("01.01.1900 00:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(0, resInt);
		resInt = mapper.mapToInt(Dates.parseDate("31.12.1900 23:59:59,999",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(364, resInt);
		resInt = mapper.mapToInt(Dates.parseDate("31.12.1901 23:59:59,999",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(729, resInt);
		resInt = mapper.mapToInt(Dates.parseDate("28.02.1900 00:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(58, resInt);
		resInt = mapper.mapToInt(Dates.parseDate("20.01.1999 20:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(36178, resInt);
		resInt = mapper.mapToInt(Dates.parseDate("20.01.2081 20:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(mapper.getNormEndAsInt(), resInt);
		resInt = mapper.mapToInt(Dates.parseDate("20.01.1800 20:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(mapper.getNormStartAsInt(), resInt);
	}

	/**
	 * Tests the {@link DateMapper#shiftToLong(Object, int, boolean)}
	 * implementation.
	 * 
	 * @throws ParseException
	 *             if a date cannot be parsed
	 */
	@Test
	public void testShift() throws ParseException {
		DateMapper mapper;
		Date start, end;
		long res;

		// test byte range
		start = Dates.parseDate("01.01.1900 00:00:00,000",
				"dd.MM.yyyy HH:mm:ss,SSS");
		end = Dates.parseDate("31.12.2000 23:59:59,999",
				"dd.MM.yyyy HH:mm:ss,SSS");

		mapper = new DateMapper(start, end, Day.instance());

		res = mapper.shiftToLong(Dates.parseDate("01.01.1900", "dd.MM.yyyy"),
				1, true);
		assertEquals(0l, res);
		res = mapper.shiftToLong(Dates.parseDate("02.01.1900", "dd.MM.yyyy"),
				1, true);
		assertEquals(0l, res);
		res = mapper.shiftToLong(Dates.parseDate("02.01.1900", "dd.MM.yyyy"),
				1, false);
		assertEquals(2l, res);
		res = mapper.shiftToLong(Dates.parseDate("02.01.1900", "dd.MM.yyyy"),
				9, false);
		assertEquals(10l, res);

		res = mapper.shiftToLong(Dates.parseDate("31.12.2000", "dd.MM.yyyy"),
				5, false);
		assertEquals(Dates.parseDate("31.12.2000", "dd.MM.yyyy"),
				mapper.resolve(res));
		res = mapper.shiftToLong(Dates.parseDate("24.12.2000", "dd.MM.yyyy"),
				10, false);
		assertEquals(Dates.parseDate("31.12.2000", "dd.MM.yyyy"),
				mapper.resolve(res));
	}

	/**
	 * Tests the implementation of the base-implementation
	 * {@link BaseMapper#getBounds(Interval)}).
	 */
	@Test
	public void testBounds() {
		final DateMapper mapper = new DateMapper(Dates.isDate("20.01.1981",
				Dates.GENERAL_TIMEZONE), Dates.isDate("20.01.1981 23:59:00",
				Dates.GENERAL_TIMEZONE), Minute.instance());

		final DateIntervalValue val1 = new DateIntervalValue(Dates.isDate(
				"20.01.1981 08:00:00", Dates.GENERAL_TIMEZONE));
		final DateIntervalValue val2 = new DateIntervalValue(Dates.isDate(
				"20.01.1981 08:07:00", Dates.GENERAL_TIMEZONE));

		final IntervalType in = IntervalType.INCLUDE;
		final IntervalType ex = IntervalType.EXCLUDE;

		Interval<?> interval;
		long[] bounds;

		interval = new Interval<Date>(val1, ex, val2, in);
		bounds = mapper.getBounds(interval);
		assertEquals(481, bounds[0]);
		assertEquals(487, bounds[1]);

		interval = new Interval<Date>(val1, ex, val2, ex);
		bounds = mapper.getBounds(interval);
		assertEquals(481, bounds[0]);
		assertEquals(486, bounds[1]);

		interval = new Interval<Date>(val1, in, val2, in);
		bounds = mapper.getBounds(interval);
		assertEquals(480, bounds[0]);
		assertEquals(487, bounds[1]);

		interval = new Interval<Date>(val1, in, val2, ex);
		bounds = mapper.getBounds(interval);
		assertEquals(480, bounds[0]);
		assertEquals(486, bounds[1]);
	}

	/**
	 * Tests the creation of the partition iterator.
	 */
	@Test
	public void testPartitionIterator() {
		int counter;
		DateMapper mapper;
		Date tmpltStart, tmpltEnd, tlStart, tlEnd;
		Iterator<long[]> it;

		// create an invalid time-line
		tlStart = Dates.isDate("01.01.2008", Dates.GENERAL_TIMEZONE);
		tlEnd = Dates.isDate("01.01.2008", Dates.GENERAL_TIMEZONE);
		mapper = new DateMapper(tlStart, tlEnd, Minute.instance());

		// get the iterator for a whole month
		tmpltStart = Dates.isDate("01.01.2008", Dates.GENERAL_TIMEZONE);
		tmpltEnd = Dates.isDate("31.01.2008 23:59:00", Dates.GENERAL_TIMEZONE);
		it = mapper.createTimelinePartitionIterator(new Interval<Date>(
				new DateIntervalValue(tmpltStart), new DateIntervalValue(
						tmpltEnd)));
		assertTrue(it.hasNext());
		assertPartition(0, 0, it.next());
		assertFalse(it.hasNext());

		// create a valid time-line for a month
		tlStart = tmpltStart;
		tlEnd = tmpltEnd;
		mapper = new DateMapper(tlStart, tlEnd, Minute.instance());
		it = mapper.createTimelinePartitionIterator(new Interval<Date>(
				new DateIntervalValue(tmpltStart), new DateIntervalValue(
						tmpltEnd)));
		assertTrue(it.hasNext());
		assertPartition(mapper.getNormStartAsLong(), mapper.getNormEndAsLong(),
				it.next());
		assertFalse(it.hasNext());

		// use a day as template
		tmpltStart = Dates.isDate("02.01.2008", Dates.GENERAL_TIMEZONE);
		tmpltEnd = Dates.isDate("02.01.2008 23:59:00", Dates.GENERAL_TIMEZONE);
		it = mapper.createTimelinePartitionIterator(new Interval<Date>(
				new DateIntervalValue(tmpltStart), new DateIntervalValue(
						tmpltEnd)));
		counter = 0;
		while (it.hasNext()) {
			final long[] partition = it.next();
			assertEquals(1440l, partition[1] - partition[0] + 1);

			counter++;
		}
		assertEquals(31, counter);

		// use a day with offset as template
		tmpltStart = Dates
				.isDate("02.01.2008 01:00:00", Dates.GENERAL_TIMEZONE);
		tmpltEnd = Dates.isDate("03.01.2008 00:59:00", Dates.GENERAL_TIMEZONE);
		it = mapper.createTimelinePartitionIterator(new Interval<Date>(
				new DateIntervalValue(tmpltStart), new DateIntervalValue(
						tmpltEnd)));
		counter = 0;
		while (it.hasNext()) {
			final long[] partition = it.next();
			assertEquals(1440l, partition[1] - partition[0] + 1);

			counter++;
		}
		assertEquals(30, counter);

		// create a valid time-line for a year
		tlStart = Dates.isDate("01.01.2008", Dates.GENERAL_TIMEZONE);
		tlEnd = Dates.isDate("31.12.2008 23:59:00", Dates.GENERAL_TIMEZONE);
		tmpltStart = Dates.isDate("02.01.2008", Dates.GENERAL_TIMEZONE);
		tmpltEnd = Dates.isDate("02.01.2008 23:59:00", Dates.GENERAL_TIMEZONE);
		mapper = new DateMapper(tlStart, tlEnd, Minute.instance());
		it = mapper.createTimelinePartitionIterator(new Interval<Date>(
				new DateIntervalValue(tmpltStart), new DateIntervalValue(
						tmpltEnd)));
		counter = 0;
		while (it.hasNext()) {
			final long[] partition = it.next();
			assertEquals(1440l, partition[1] - partition[0] + 1);

			counter++;
		}
		assertEquals(366, counter);

		// use a minute as template
		tmpltStart = Dates.isDate("02.01.2008", Dates.GENERAL_TIMEZONE);
		tmpltEnd = Dates.isDate("02.01.2008 00:00:00", Dates.GENERAL_TIMEZONE);
		mapper = new DateMapper(tlStart, tlEnd, Minute.instance());
		it = mapper.createTimelinePartitionIterator(new Interval<Date>(
				new DateIntervalValue(tmpltStart), new DateIntervalValue(
						tmpltEnd)));
		counter = 0;
		while (it.hasNext()) {
			final long[] partition = it.next();
			assertEquals(1l, partition[1] - partition[0] + 1);

			counter++;
		}
		assertEquals(366 * 1440, counter);

	}

	private void assertPartition(final long expStart, final long expEnd,
			final long[] partition) {
		assertEquals(expStart, partition[0]);
		assertEquals(expEnd, partition[1]);
	}

	private void assertResult(final Date start, final Date end,
			final ITimeGranularity g, final Date expStart, final Date expEnd) {
		final DateMapper mapper = new DateMapper(start, end, g);

		final Date mappedStart = mapper.resolve(0);
		final Date mappedEnd = mapper.resolve(mapper.getEnd()
				- mapper.getStart());

		assertEquals(expStart, mappedStart);
		assertEquals(expEnd, mappedEnd);
	}
}
