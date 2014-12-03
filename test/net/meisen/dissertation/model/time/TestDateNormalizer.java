package net.meisen.dissertation.model.time;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.text.ParseException;
import java.util.Date;

import net.meisen.dissertation.model.time.DateNormalizer;
import net.meisen.dissertation.model.time.DateNormalizer.RoundType;
import net.meisen.dissertation.model.time.granularity.Day;
import net.meisen.dissertation.model.time.granularity.DeciSecond;
import net.meisen.dissertation.model.time.granularity.FortNight;
import net.meisen.dissertation.model.time.granularity.Hour;
import net.meisen.dissertation.model.time.granularity.IDateBasedGranularity;
import net.meisen.dissertation.model.time.granularity.ISecondBasedGranularity;
import net.meisen.dissertation.model.time.granularity.ITimeGranularity;
import net.meisen.dissertation.model.time.granularity.MilliSecond;
import net.meisen.dissertation.model.time.granularity.Minute;
import net.meisen.dissertation.model.time.granularity.NanoSecond;
import net.meisen.dissertation.model.time.granularity.Second;
import net.meisen.dissertation.model.time.granularity.Week;
import net.meisen.dissertation.model.time.granularity.YoctoSecond;
import net.meisen.dissertation.model.time.granularity.ZeptoSecond;
import net.meisen.general.genmisc.types.Dates;

import org.junit.Test;

/**
 * Tests the implementation of {@code HierarchyOfTime}.
 * 
 * @author pmeisen
 * 
 */
public class TestDateNormalizer {

	/**
	 * Tests the implementation of
	 * {@link DateNormalizer#compare(ISecondBasedGranularity, ISecondBasedGranularity)}
	 * .
	 */
	@Test
	public void testCompare() {
		final DateNormalizer n = DateNormalizer.instance();

		assertEquals(0, n.compare(Second.instance(), Second.instance()));
		assertEquals(0,
				n.compare(MilliSecond.instance(), MilliSecond.instance()));
		assertEquals(0, n.compare(DeciSecond.instance(), DeciSecond.instance()));
		assertEquals(0, n.compare(Week.instance(), Week.instance()));

		assertEquals(1, n.compare(Week.instance(), Day.instance()));
		assertEquals(-1, n.compare(Day.instance(), Week.instance()));

		assertEquals(-1, n.compare(MilliSecond.instance(), Week.instance()));
		assertEquals(1, n.compare(Week.instance(), MilliSecond.instance()));

		assertEquals(1,
				n.compare(DeciSecond.instance(), MilliSecond.instance()));
		assertEquals(-1,
				n.compare(MilliSecond.instance(), DeciSecond.instance()));
	}

	/**
	 * Tests the implementation of
	 * {@link DateNormalizer#isMoreDetailed(ITimeGranularity, ITimeGranularity)}
	 * .
	 */
	@Test
	public void testIsMoreDetailled() {
		final DateNormalizer n = DateNormalizer.instance();

		// equal instances for all values
		assertFalse(n.isMoreDetailed(DeciSecond.instance(),
				DeciSecond.instance()));
		assertFalse(n.isMoreDetailed(MilliSecond.instance(),
				MilliSecond.instance()));
		assertFalse(n.isMoreDetailed(Second.instance(), Second.instance()));
		assertFalse(n.isMoreDetailed(Day.instance(), Day.instance()));
		assertFalse(n.isMoreDetailed(Week.instance(), Week.instance()));

		// check some values
		assertTrue(n.isMoreDetailed(MilliSecond.instance(),
				DeciSecond.instance()));
		assertFalse(n.isMoreDetailed(Second.instance(), DeciSecond.instance()));
		assertTrue(n.isMoreDetailed(DeciSecond.instance(), Second.instance()));
		assertFalse(n.isMoreDetailed(DeciSecond.instance(),
				MilliSecond.instance()));
		assertTrue(n.isMoreDetailed(Day.instance(), Week.instance()));
		assertFalse(n.isMoreDetailed(Week.instance(), Day.instance()));
	}

	/**
	 * Tests the implementation of
	 * {@link DateNormalizer#getMultiplier(ISecondBasedGranularity, ISecondBasedGranularity)}
	 * .
	 */
	@Test
	public void testGetMultiplier() {
		final DateNormalizer n = DateNormalizer.instance();
		double m;

		// equal instances for all values
		m = n.getMultiplier(DeciSecond.instance(), DeciSecond.instance());
		assertEquals(1.0, m, 0.0);
		m = n.getMultiplier(MilliSecond.instance(), MilliSecond.instance());
		assertEquals(1.0, m, 0.0);
		m = n.getMultiplier(YoctoSecond.instance(), YoctoSecond.instance());
		assertEquals(1.0, m, 0.0);
		m = n.getMultiplier(ZeptoSecond.instance(), ZeptoSecond.instance());
		assertEquals(1.0, m, 0.0);

		m = n.getMultiplier(MilliSecond.instance(), DeciSecond.instance());
		assertEquals(0.01, m, 0.0);

		m = n.getMultiplier(DeciSecond.instance(), MilliSecond.instance());
		assertEquals(100.0, m, 0.0);

		m = n.getMultiplier(MilliSecond.instance(), Second.instance());
		assertEquals(0.001, m, 0.0);

		m = n.getMultiplier(Second.instance(), MilliSecond.instance());
		assertEquals(1000.0, m, 0.0);

		m = n.getMultiplier(FortNight.instance(), Second.instance());
		assertEquals(1209600.0, m, 0.0);

		m = n.getMultiplier(Second.instance(), FortNight.instance());
		assertEquals(1.0 / 1209600.0, m, 0.0);

		m = n.getMultiplier(MilliSecond.instance(), Minute.instance());
		assertEquals(1.0 / (1000.0 * 60.0), m, 0.0);

		m = n.getMultiplier(Minute.instance(), MilliSecond.instance());
		assertEquals(1000.0 * 60.0, m, 0.0);

		m = n.getMultiplier(NanoSecond.instance(), YoctoSecond.instance());
		assertEquals(1000000000000000.0, m, 0.0);

		m = n.getMultiplier(YoctoSecond.instance(), NanoSecond.instance());
		assertEquals(1.0 / 1000000000000000.0, m, 0.0);

		m = n.getMultiplier(Week.instance(), YoctoSecond.instance());
		assertEquals(6.048 * 100000000000000000000000000000.0, m, 0.0);
	}

	/**
	 * Tests the normalization of dates.
	 * 
	 * @throws ParseException
	 *             if a date is invalid
	 */
	@Test
	public void testNormalize() throws ParseException {

		// @formatter:off
		
		// test Day
		assertDate("01.11.2014 00:00:00", "01.11.2014 00:00:00", Day.instance());
		assertDate("01.11.2014 13:00:00", "01.11.2014 00:00:00", Day.instance());
		assertDate("01.11.2014 00:00:00", "01.11.2014 00:00:00", Day.instance());
		assertDate("01.11.2014 12:00:00", "01.11.2014 00:00:00", Day.instance());
		assertDate("01.11.2014 00:00:00", "01.11.2014 00:00:00", Day.instance());
		assertDate("01.11.2014 06:00:00", "01.11.2014 00:00:00", Day.instance());

		// test Hour
		assertDate("01.11.2014 06:00:00", "01.11.2014 06:00:00", Hour.instance());
		assertDate("01.11.2014 06:12:00", "01.11.2014 06:00:00", Hour.instance());
		
		// test Minutes
		assertDate("01.11.2014 06:00:00", "01.11.2014 06:00:00", Minute.instance());
		assertDate("01.11.2014 06:12:12", "01.11.2014 06:12:00", Minute.instance());
		assertDate("02.11.2014 02:12:33", "02.11.2014 02:12:00", Minute.instance());
		assertDate("02.11.2014 03:12:56", "02.11.2014 03:12:00", Minute.instance());
		// @formatter:on
	}

	private void assertDate(final String dateString,
			final String resDateString, final IDateBasedGranularity granularity)
			throws ParseException {
		final DateNormalizer n = DateNormalizer.instance();
		final Date date = Dates.parseDate(dateString, "dd.MM.yyyy HH:mm:ss",
				Dates.GENERAL_TIMEZONE);
		final long norm = n.normalize(date, granularity, RoundType.FLOOR);
		final Date deDate = n.denormalize(norm, granularity);

		final Date resDate = Dates.parseDate(resDateString,
				"dd.MM.yyyy HH:mm:ss", Dates.GENERAL_TIMEZONE);
		assertEquals(resDate, deDate);
	}
}
