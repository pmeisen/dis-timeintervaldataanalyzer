package net.meisen.dissertation.impl.naturals;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import net.meisen.dissertation.impl.naturals.LongNaturals;

import org.junit.Test;

/**
 * Tests the implementation of @{code LongNaturals}.
 * 
 * @author pmeisen
 * 
 */
public class TestLongNaturals {

	/**
	 * Tests the addition of {@code 0 + 10} as example addition with {@code 0}
	 */
	@Test
	public void testNullAddition() {
		final LongNaturals summand1 = new LongNaturals(0l);
		final LongNaturals summand2 = new LongNaturals(10l);

		assertEquals(new LongNaturals(10l), summand1.add(summand2));
		assertEquals(new LongNaturals(0l), summand1);
		assertEquals(new LongNaturals(10l), summand2);
	}

	/**
	 * Tests the addition of {@code 5 + 10} as example addition
	 */
	@Test
	public void testSimpleAddition() {
		final LongNaturals summand1 = new LongNaturals(5l);
		final LongNaturals summand2 = new LongNaturals(10l);

		assertEquals(new LongNaturals(15l), summand1.add(summand2));
		assertEquals(new LongNaturals(5l), summand1);
		assertEquals(new LongNaturals(10l), summand2);
	}

	/**
	 * Tests the subtraction of {@code 15 -10} as example subtraction
	 */
	@Test
	public void testSubstraction() {
		final LongNaturals minuend = new LongNaturals(15l);
		final LongNaturals subtrahend = new LongNaturals(10l);

		assertEquals(new LongNaturals(5l), minuend.subtract(subtrahend));
		assertEquals(new LongNaturals(15l), minuend);
		assertEquals(new LongNaturals(10l), subtrahend);
	}

	/**
	 * Tests the equality checking
	 */
	@Test
	public void testEquality() {
		final LongNaturals val = new LongNaturals(15);

		assertFalse(val.equals(null));
		assertTrue(val.equals(val));
		assertTrue(val.equals(15));
		assertTrue(val.equals(15l));
		assertFalse(val.equals(16l));
		assertFalse(val.equals(27));
	}

	/**
	 * Tests the implementation of {@code compareTo(LongNaturals)}.
	 */
	@Test
	public void testCompareTo() {
		final LongNaturals c1 = new LongNaturals(15l);
		final LongNaturals c2 = new LongNaturals(20l);

		assertEquals(0, c1.compareTo(c1));
		assertEquals(0, c2.compareTo(c2));
		assertEquals(-1, c1.compareTo(c2));
		assertEquals(1, c2.compareTo(c1));
		assertEquals(-1, c1.compareTo(c1.add(new LongNaturals(1))));
		assertEquals(0, c1.compareTo(c1.add(new LongNaturals(0))));
	}
}
