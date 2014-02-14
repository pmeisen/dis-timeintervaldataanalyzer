package net.meisen.dissertation.model.time.granularity;

import static org.junit.Assert.assertEquals;

import java.text.ParseException;
import java.util.Date;

import net.meisen.dissertation.model.time.granularity.IDateBasedGranularity.DateBasedHelper;
import net.meisen.general.genmisc.types.Dates;

import org.junit.Test;

/**
 * Tests the implementation of {@link DateBasedHelper}.
 * 
 * @author pmeisen
 * 
 */
public class TestDateBasedHelper {

	/**
	 * Tests the implementation of
	 * {@link DateBasedHelper#getFormats(DateFormat[], Date)}.
	 * 
	 * @throws ParseException
	 *             if the date cannot be parsed
	 */
	@Test
	public void testGetFormats() throws ParseException {
		final DateBasedHelper helper = new DateBasedHelper();
		long[] val;

		// 01.01.1970
		val = helper.getFormats(new DateFormat[] { DateFormat.YEAR,
				DateFormat.MONTH }, new Date(0));
		assertEquals(1970, val[0]);
		assertEquals(1, val[1]);

		// 20.01.1981
		val = helper.getFormats(new DateFormat[] { DateFormat.YEAR,
				DateFormat.MONTH }, Dates.parseDate("20.01.1981 08:07:23,666",
				"dd.MM.yyyy HH:mm:ss,SSS"));
		assertEquals(1981, val[0]);
		assertEquals(1, val[1]);

		// 29.02.2008
		val = helper
				.getFormats(new DateFormat[] { DateFormat.YEAR,
						DateFormat.MONTH },
						Dates.parseDate("29.02.2008", "dd.MM.yyyy"));
		assertEquals(2008, val[0]);
		assertEquals(2, val[1]);
	}
}
