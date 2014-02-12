package net.meisen.dissertation.model.time.timeline;

import static org.junit.Assert.assertEquals;

import java.text.ParseException;
import java.util.Date;

import net.meisen.dissertation.model.time.granularity.MilliSecond;
import net.meisen.dissertation.model.time.timeline.TimelineDefinition.DatePosition;
import net.meisen.general.genmisc.types.Dates;

import org.junit.Test;

public class TestTimelineDefinition {

	/**
	 * Tests the implementation of
	 * {@link TimelineDefinition#getDef(DatePosition, Date)}.
	 * 
	 * @throws ParseException
	 *             if the parsing fails
	 */
	@Test
	public void testDefCalculation() throws ParseException {
		Date pit;
		int year;
		final TimelineDefinition def = new TimelineDefinition(1, 1,
				MilliSecond.instance());
		final int off = TimelineDefinition.OFFSET;

		// create the pit and check the results
		year = 1981;
		pit = Dates.parseDate("20.01." + year, "dd.MM.yyyy");
		assertEquals(Dates.parseDate("20.01." + (year - off), "dd.MM.yyyy"),
				def.getDef(DatePosition.START, pit));
		assertEquals(Dates.parseDate("20.01." + (year + off), "dd.MM.yyyy"),
				def.getDef(DatePosition.END, pit));

		// create the pit and check the results
		year = 100;
		pit = Dates.parseDate("20.01." + year, "dd.MM.yyyy");
		assertEquals(Dates.parseDate("20.01." + (year - off), "dd.MM.yyyy"),
				def.getDef(DatePosition.START, pit));
		assertEquals(Dates.parseDate("20.01." + (year + off), "dd.MM.yyyy"),
				def.getDef(DatePosition.END, pit));

		// create the pit and check the results
		year = 2090;
		pit = Dates.parseDate("20.01." + year, "dd.MM.yyyy");
		assertEquals(Dates.parseDate("20.01." + (year - off), "dd.MM.yyyy"),
				def.getDef(DatePosition.START, pit));
		assertEquals(Dates.parseDate("20.01." + (year + off), "dd.MM.yyyy"),
				def.getDef(DatePosition.END, pit));

	}

	/**
	 * Tests the usage of a integer constructor.
	 */
	@Test
	public void testIntegerTimeline() {
		final TimelineDefinition def = new TimelineDefinition(-100, 5,
				MilliSecond.instance());
		assertEquals(Long.class, def.getType());
		assertEquals(-100l, def.getStart());
		assertEquals(5l, def.getEnd());
	}

	/**
	 * Tests the usage of a long constructor.
	 */
	@Test
	public void testLongTimeline() {
		final TimelineDefinition def = new TimelineDefinition(10l, 100l,
				MilliSecond.instance());
		assertEquals(Long.class, def.getType());
		assertEquals(10l, def.getStart());
		assertEquals(100l, def.getEnd());
	}

	/**
	 * Tests the usage of a date constructor.
	 */
	@Test
	public void testDateTimeline() {
		final Date now = new Date();
		final TimelineDefinition def = new TimelineDefinition(now, now,
				MilliSecond.instance());

		assertEquals(Date.class, def.getType());
		assertEquals(now, def.getStart());
		assertEquals(now, def.getEnd());
	}

	/**
	 * Tests the usage of a date constructor with null.
	 * 
	 * @throws ParseException
	 *             if the parsing fails
	 */
	@Test
	public void testDateWithNullValue() throws ParseException {
		final Date pit = Dates.parseDate("10.10.2010 01:22:33",
				"dd.MM.yyyy HH:mm:ss");
		final TimelineDefinition def = new TimelineDefinition(pit, null,
				MilliSecond.instance());

		assertEquals(Date.class, def.getType());
		assertEquals(pit, def.getStart());
		assertEquals(def.getDef(DatePosition.END, pit), def.getEnd());
	}

	/**
	 * Tests the usage of a long constructor with null.
	 * 
	 * @throws ParseException
	 *             if the parsing fails
	 */
	@Test
	public void testLongWithNullValue() throws ParseException {
		final TimelineDefinition def = new TimelineDefinition(null,
				31536000000l, MilliSecond.instance());

		assertEquals(Long.class, def.getType());
		assertEquals(0l, def.getStart());
		assertEquals(31536000000l, def.getEnd());
	}

	@Test
	public void testStringsWithDate() throws ParseException {
		final TimelineDefinition def = new TimelineDefinition("01.01.2000",
				"01.10.2001", MilliSecond.instance());

		assertEquals(Date.class, def.getType());
		assertEquals(Dates.parseDate("01.01.2000", "dd.MM.yyyy"),
				def.getStart());
		assertEquals(Dates.parseDate("01.10.2001", "dd.MM.yyyy"), def.getEnd());
	}

	@Test
	public void testStringsWithDateAndEndNull() throws ParseException {
		final TimelineDefinition def = new TimelineDefinition("01.01.2000",
				null, MilliSecond.instance());

		assertEquals(Date.class, def.getType());
		assertEquals(Dates.parseDate("01.01.2000", "dd.MM.yyyy"),
				def.getStart());
		assertEquals(Dates.parseDate("01.01.2001", "dd.MM.yyyy"), def.getEnd());
	}

	@Test
	public void testStringsWithDateAndStartNull() throws ParseException {
		final TimelineDefinition def = new TimelineDefinition(null,
				"01.01.2001", MilliSecond.instance());

		assertEquals(Date.class, def.getType());
		assertEquals(Dates.parseDate("01.01.2000", "dd.MM.yyyy"),
				def.getStart());
		assertEquals(Dates.parseDate("01.01.2001", "dd.MM.yyyy"), def.getEnd());
	}
}
