package net.meisen.dissertation.impl.parser.query.select;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Tests the implementation of a {@code DescriptorValue}.
 * 
 * @author pmeisen
 * 
 */
public class TestDescriptorValue {

	/**
	 * Tests the usage of a {@code null} text.
	 */
	@Test
	public void testNullComperator() {
		final DescriptorValue cmp = new DescriptorValue(null);

		assertFalse(cmp.containsWildchar());
		assertNull(cmp.getRawValue());
		assertNull(cmp.getValue());
		assertTrue(cmp.matches(null));
		assertFalse(cmp.matches(""));
	}

	/**
	 * Tests the usage of an empty text.
	 */
	@Test
	public void testEmptyValue() {
		final DescriptorValue cmp = new DescriptorValue("");

		assertFalse(cmp.containsWildchar());
		assertEquals("", cmp.getRawValue());
		assertEquals("", cmp.getValue());
		assertTrue(cmp.matches(""));
		assertFalse(cmp.matches(null));
		assertFalse(cmp.matches(" "));
	}

	/**
	 * Tests the usage of simple values
	 */
	@Test
	public void testSimpleValue() {
		final DescriptorValue cmp = new DescriptorValue("hallo");

		assertFalse(cmp.containsWildchar());
		assertEquals("hallo", cmp.getRawValue());
		assertEquals("hallo", cmp.getValue());
		assertTrue(cmp.matches("hallo"));
		assertFalse(cmp.matches(null));
		assertFalse(cmp.matches(" hallo "));
		assertFalse(cmp.matches("Hallo"));
		assertFalse(cmp.matches("hallO"));
		assertFalse(cmp.matches("HALLO"));
		assertFalse(cmp.matches("whatever"));
	}

	/**
	 * Tests the usage of a quoted {@code *} at the start of the text.
	 */
	@Test
	public void testQuotedWildcharAtStart() {
		final DescriptorValue cmp = new DescriptorValue("\\*nosuffix");

		assertFalse(cmp.containsWildchar());
		assertEquals("\\*nosuffix", cmp.getRawValue());
		assertEquals("*nosuffix", cmp.getValue());
		assertTrue(cmp.matches("*nosuffix"));
		assertFalse(cmp.matches("\\*nosuffix"));
		assertFalse(cmp.matches("nosuffix"));
		assertFalse(cmp.matches(null));
	}

	/**
	 * Tests the usage of a quoted {@code *} in the middle of the text.
	 */
	@Test
	public void testQuotedWildcharAtMiddle() {
		final DescriptorValue cmp = new DescriptorValue("no\\*infix");

		assertFalse(cmp.containsWildchar());
		assertEquals("no\\*infix", cmp.getRawValue());
		assertEquals("no*infix", cmp.getValue());
		assertTrue(cmp.matches("no*infix"));
		assertFalse(cmp.matches("noinfix"));
		assertFalse(cmp.matches(null));
	}

	/**
	 * Tests the usage of a quoted {@code *} at the end of the text.
	 */
	@Test
	public void testQuotedWildcharAtEnd() {
		final DescriptorValue cmp = new DescriptorValue("noprefix\\*");

		assertFalse(cmp.containsWildchar());
		assertEquals("noprefix\\*", cmp.getRawValue());
		assertEquals("noprefix*", cmp.getValue());
		assertTrue(cmp.matches("noprefix*"));
		assertFalse(cmp.matches("noprefix\\*"));
		assertFalse(cmp.matches("noprefix\\\\*"));
		assertFalse(cmp.matches("noprefix"));
		assertFalse(cmp.matches("noprefixwhatsoever"));
		assertFalse(cmp.matches(null));
	}

	/**
	 * Tests the usage of a unquoted {@code *} at the start of the text.
	 */
	@Test
	public void testWildcharAtStart() {
		final DescriptorValue cmp = new DescriptorValue("*suffix");

		assertTrue(cmp.containsWildchar());
		assertEquals("*suffix", cmp.getRawValue());
		assertEquals(".*suffix", cmp.getValue());
		assertTrue(cmp.matches("suffix"));
		assertTrue(cmp.matches("asdassuffix"));
		assertTrue(cmp.matches("98suffix"));
		assertFalse(cmp.matches("908suffixlala"));
		assertFalse(cmp.matches(null));
	}

	/**
	 * Tests the usage of a unquoted {@code *} in the middle of the text.
	 */
	@Test
	public void testWildcharAtMiddle() {
		final DescriptorValue cmp = new DescriptorValue("in*fix");

		assertTrue(cmp.containsWildchar());
		assertEquals("in*fix", cmp.getRawValue());
		assertEquals("in.*fix", cmp.getValue());
		assertTrue(cmp.matches("infix"));
		assertTrue(cmp.matches("inasdadafix"));
		assertTrue(cmp.matches("in23131asd123fix"));
		assertFalse(cmp.matches("iasd1nfix"));
		assertFalse(cmp.matches(null));
	}

	/**
	 * Tests the usage of a unquoted {@code *} at the end of the text.
	 */
	@Test
	public void testWildcharAtEnd() {
		final DescriptorValue cmp = new DescriptorValue("prefix*");

		assertTrue(cmp.containsWildchar());
		assertEquals("prefix*", cmp.getRawValue());
		assertEquals("prefix.*", cmp.getValue());
		assertTrue(cmp.matches("prefix"));
		assertTrue(cmp.matches("prefixANDAVALUE"));
		assertTrue(cmp.matches("prefix12"));
		assertFalse(cmp.matches("lalaprefix12"));
		assertFalse(cmp.matches(null));
	}

	/**
	 * Tests the usage of several unquoted {@code *}.
	 */
	@Test
	public void testSeveralWildchars() {
		final DescriptorValue cmp = new DescriptorValue(
				"* Hi *, I just wanted to say *. Greetings *!");

		assertTrue(cmp.containsWildchar());
		assertEquals("* Hi *, I just wanted to say *. Greetings *!",
				cmp.getRawValue());
		assertEquals(".* Hi .*, I just wanted to say .*. Greetings .*!",
				cmp.getValue());
		assertTrue(cmp
				.matches("29.04.2010 Hi Peter, I just wanted to say Hello. Greetings Philipp!"));
		assertFalse(cmp.matches(null));
	}

	/**
	 * Tests the usage of quoted and unquoted {@code *}.
	 */
	@Test
	public void testMixedQuotedAndWildchars() {
		final DescriptorValue cmp = new DescriptorValue(
				"This is a \\*, which might be seen by *!");

		assertTrue(cmp.containsWildchar());
		assertEquals("This is a \\*, which might be seen by *!",
				cmp.getRawValue());
		assertEquals("This is a \\*, which might be seen by .*!",
				cmp.getValue());
		assertTrue(cmp.matches("This is a *, which might be seen by me!"));
		assertFalse(cmp.matches(null));
	}
}
