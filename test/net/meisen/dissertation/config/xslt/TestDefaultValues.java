package net.meisen.dissertation.config.xslt;

import static org.junit.Assert.assertEquals;

import java.util.Date;

import org.junit.Test;

/**
 * Test the {@code DefaultValues} implementation.
 * 
 * @author pmeisen
 * 
 */
public class TestDefaultValues {
	
	/**
	 * Tests the implementation of
	 * {@link DefaultValues#determineValueClass(String)}.
	 */
	@Test
	public void testDetermineValueClass() {
		testDetermineValueClass(String.class, null);
		testDetermineValueClass(String.class, "");
		testDetermineValueClass(Integer.class, "564841316");
		testDetermineValueClass(Integer.class, "" + Integer.MAX_VALUE);
		testDetermineValueClass(Integer.class, "" + Integer.MIN_VALUE);
		testDetermineValueClass(Long.class, "" + (Integer.MAX_VALUE + 1l));
		testDetermineValueClass(Double.class, ".21312");
		testDetermineValueClass(Double.class, "12.0");
		testDetermineValueClass(Double.class, "-.43");
		testDetermineValueClass(Double.class, "" + Double.MAX_VALUE);
		testDetermineValueClass(Double.class, "" + Double.MIN_VALUE);

		testDetermineValueClass(Date.class, "12.01.2012");
		testDetermineValueClass(Date.class, "20/01/1981 12:00:00");
	}

	private void testDetermineValueClass(final Class<?> c, final String s) {
		if (Double.class.equals(c)) {
			assertEquals("double", DefaultValues.determineValueClass(s));
		} else if (Integer.class.equals(c)) {
			assertEquals("int", DefaultValues.determineValueClass(s));
		} else if (Long.class.equals(c)) {
			assertEquals("long", DefaultValues.determineValueClass(s));
		} else {
			assertEquals(c.getName(), DefaultValues.determineValueClass(s));
		}
	}
}
