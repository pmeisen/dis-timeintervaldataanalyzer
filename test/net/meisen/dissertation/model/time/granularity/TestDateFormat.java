package net.meisen.dissertation.model.time.granularity;

import static org.junit.Assert.assertEquals;

import java.text.ParseException;
import java.util.Date;

import net.meisen.general.genmisc.types.Dates;

import org.junit.Test;

/**
 * Tests the implementation of a {@code DateFormat}.
 * 
 * @author pmeisen
 * 
 */
public class TestDateFormat {

	/**
	 * Tests the implementation of the {@link DateFormat#modify(Date, long)}.
	 * 
	 * @throws ParseException
	 *             if a date cannot be parsed
	 */
	@Test
	public void testModification() throws ParseException {
		Date testDate, mod;

		testDate = Dates.parseDate("20.01.1981 08:07:52,666",
				"dd.MM.yyyy HH:mm:ss,SSS");

		mod = DateFormat.YEAR.modify(testDate, 1);
		assertEquals(Dates.parseDate("20.01.1982 08:07:52,666",
				"dd.MM.yyyy HH:mm:ss,SSS"), mod);

		mod = DateFormat.DAY.modify(testDate, 12);
		assertEquals(Dates.parseDate("01.02.1981 08:07:52,666",
				"dd.MM.yyyy HH:mm:ss,SSS"), mod);

		mod = DateFormat.DAY.modify(testDate, 12);
		assertEquals(Dates.parseDate("01.02.1981 08:07:52,666",
				"dd.MM.yyyy HH:mm:ss,SSS"), mod);

		mod = DateFormat.HOUR.modify(testDate, 25);
		assertEquals(Dates.parseDate("21.01.1981 09:07:52,666",
				"dd.MM.yyyy HH:mm:ss,SSS"), mod);

		mod = DateFormat.MINUTE.modify(testDate, 60 * 24 * 100 + 30);
		assertEquals(Dates.parseDate("30.04.1981 08:37:52,666",
				"dd.MM.yyyy HH:mm:ss,SSS"), mod);

		mod = DateFormat.MILLISECOND.modify(testDate, 25 * 60 * 60 * 1000);
		assertEquals(Dates.parseDate("21.01.1981 09:07:52,666",
				"dd.MM.yyyy HH:mm:ss,SSS"), mod);

		testDate = new Date(0);

		mod = DateFormat.MILLISECOND.modify(testDate, Integer.MAX_VALUE + 1l);
		assertEquals(new Date(((long) Integer.MAX_VALUE) + 1l), mod);
	}
}
