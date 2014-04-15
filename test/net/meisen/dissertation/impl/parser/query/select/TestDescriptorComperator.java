package net.meisen.dissertation.impl.parser.query.select;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Tests the implementation of a {@code DescriptorComperator}.
 * 
 * @author pmeisen
 * 
 */
public class TestDescriptorComperator {

	/**
	 * Tests the usage of a {@code null} text.
	 */
	@Test
	public void testNullComperator() {
		final DescriptorComperator cmp = new DescriptorComperator("myId", null);

		assertEquals("myId", cmp.getId());
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
		final DescriptorComperator cmp = new DescriptorComperator("theId", "");

		assertEquals("theId", cmp.getId());
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
		final DescriptorComperator cmp = new DescriptorComperator("theId",
				"hallo");

		assertEquals("theId", cmp.getId());
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
		final DescriptorComperator cmp = new DescriptorComperator(
				"quotedWildchar", "\\*nosuffix");

		assertEquals("quotedWildchar", cmp.getId());
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
		final DescriptorComperator cmp = new DescriptorComperator(
				"quotedWildchar", "no\\*infix");

		assertEquals("quotedWildchar", cmp.getId());
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
		final DescriptorComperator cmp = new DescriptorComperator(
				"quotedWildchar", "noprefix\\*");

		assertEquals("quotedWildchar", cmp.getId());
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
		final DescriptorComperator cmp = new DescriptorComperator("Wildchar",
				"*suffix");

		assertEquals("Wildchar", cmp.getId());
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
		final DescriptorComperator cmp = new DescriptorComperator("Wildchar",
				"in*fix");

		assertEquals("Wildchar", cmp.getId());
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
		final DescriptorComperator cmp = new DescriptorComperator("Wildchar",
				"prefix*");

		assertEquals("Wildchar", cmp.getId());
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
		final DescriptorComperator cmp = new DescriptorComperator("Wildchar",
				"* Hi *, I just wanted to say *. Greetings *!");

		assertEquals("Wildchar", cmp.getId());
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
		final DescriptorComperator cmp = new DescriptorComperator(
				"MixedWildchar", "This is a \\*, which might be seen by *!");

		assertEquals("MixedWildchar", cmp.getId());
		assertTrue(cmp.containsWildchar());
		assertEquals("This is a \\*, which might be seen by *!",
				cmp.getRawValue());
		assertEquals("This is a \\*, which might be seen by .*!",
				cmp.getValue());
		assertTrue(cmp.matches("This is a *, which might be seen by me!"));
		assertFalse(cmp.matches(null));
	}
}
