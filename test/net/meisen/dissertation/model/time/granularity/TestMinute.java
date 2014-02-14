package net.meisen.dissertation.model.time.granularity;

import static org.junit.Assert.assertEquals;

import java.util.Date;

import net.meisen.general.genmisc.types.Dates;

import org.junit.Test;

/**
 * Tests the implementation of {@link Minute}.
 * 
 * @author pmeisen
 * 
 */
public class TestMinute {

	private final Minute min = Minute.instance();

	private Date getDate(final String date) {
		return Dates.isDate(date, Dates.GENERAL_TIMEZONE);
	}

	/**
	 * Test the {@link Minute#determineRepresentor(Date)} and
	 * {@link Minute#resolveRepresenter(long)}.
	 */
	@Test
	public void testDetermineRepresentorAndResolving() {

		long rep;
		Date date;

		// check the beginning
		rep = min.determineRepresentor(new Date(0));
		assertEquals(0, rep);
		date = min.resolveRepresenter(rep);
		assertEquals(new Date(0), date);

		// check some simple date
		rep = min.determineRepresentor(getDate("01.01.1970 01:30:20"));
		assertEquals(90, rep);
		date = min.resolveRepresenter(rep);
		assertEquals(getDate("01.01.1970 01:30:00"), date);

		rep = min.determineRepresentor(getDate("20.01.1982 18:07:55"));
		assertEquals(6339967, rep);
		date = min.resolveRepresenter(rep);
		assertEquals(getDate("20.01.1982 18:07:00"), date);

		rep = min.determineRepresentor(getDate("21.01.1982 18:07:55"));
		assertEquals(6339967 + 60 * 24, rep);
		date = min.resolveRepresenter(rep);
		assertEquals(getDate("21.01.1982 18:07:00"), date);
	}

	/**
	 * Test the {@link Minute#determineRepresentor(Date)} and the distance
	 * resulting from two resolutions.
	 */
	@Test
	public void testDistanceOfRepresentors() {
		long rep1, rep2;

		rep1 = min.determineRepresentor(getDate("29.02.2008 23:59:59"));
		rep2 = min.determineRepresentor(getDate("01.03.2008 00:00:00"));
		assertEquals(1l, rep2 - rep1);

		rep1 = min.determineRepresentor(getDate("28.02.2008 23:59:59"));
		rep2 = min.determineRepresentor(getDate("01.03.2008 00:00:00"));
		assertEquals(60 * 24 + 1, rep2 - rep1);

		rep1 = min.determineRepresentor(getDate("28.02.2007 23:59:59"));
		rep2 = min.determineRepresentor(getDate("01.03.2007 00:00:00"));
		assertEquals(1l, rep2 - rep1);

		rep1 = min.determineRepresentor(getDate("29.02.2008 23:59:59"));
		rep2 = min.determineRepresentor(getDate("01.03.2009 00:00:00"));
		assertEquals(365 * 24 * 60 + 1, rep2 - rep1);
	}
}
