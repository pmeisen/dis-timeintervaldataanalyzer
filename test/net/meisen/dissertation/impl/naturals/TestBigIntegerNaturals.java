package net.meisen.dissertation.impl.naturals;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import net.meisen.dissertation.impl.naturals.BigIntegerNaturals;

import org.junit.Test;

/**
 * Tests the implementation of {@code BigIntegerNaturals}.
 * 
 * @author pmeisen
 * 
 */
public class TestBigIntegerNaturals {

	/**
	 * Tests the equality checking
	 */
	@Test
	public void testEquality() {
		final BigIntegerNaturals val = new BigIntegerNaturals(15);

		assertFalse(val.equals(null));
		assertTrue(val.equals(val));
		assertTrue(val.equals(15));
		assertTrue(val.equals(15l));
		assertFalse(val.equals(16l));
		assertFalse(val.equals(27));
	}

	/**
	 * Tests the addition of {@code 0 + 10} as example addition with {@code 0}
	 */
	@Test
	public void testNullAddition() {
		final BigIntegerNaturals summand1 = new BigIntegerNaturals(0l);
		final BigIntegerNaturals summand2 = new BigIntegerNaturals(10l);

		assertEquals(new BigIntegerNaturals(10), summand1.add(summand2));
		assertEquals(new BigIntegerNaturals(0l), summand1);
		assertEquals(new BigIntegerNaturals(10), summand2);
	}

	/**
	 * Tests the addition of {@code 5 + 10} as example addition
	 */
	@Test
	public void testSimpleAddition() {
		final BigIntegerNaturals summand1 = new BigIntegerNaturals(5l);
		final BigIntegerNaturals summand2 = new BigIntegerNaturals(10l);

		assertEquals(new BigIntegerNaturals(15l), summand1.add(summand2));
		assertEquals(new BigIntegerNaturals(5), summand1);
		assertEquals(new BigIntegerNaturals(10), summand2);
	}

	/**
	 * Tests the subtraction of {@code 15 -10} as example subtraction
	 */
	@Test
	public void testSubstraction() {
		final BigIntegerNaturals minuend = new BigIntegerNaturals(15l);
		final BigIntegerNaturals subtrahend = new BigIntegerNaturals(10l);

		assertEquals(new BigIntegerNaturals(5), minuend.subtract(subtrahend));
		assertEquals(new BigIntegerNaturals(15l), minuend);
		assertEquals(new BigIntegerNaturals(10), subtrahend);
	}

	/**
	 * Tests the implementation of {@code compareTo(BigIntegerNaturals)}.
	 */
	@Test
	public void testCompareTo() {
		final BigIntegerNaturals c1 = new BigIntegerNaturals(15l);
		final BigIntegerNaturals c2 = new BigIntegerNaturals("1000000");

		assertEquals(0, c1.compareTo(c1));
		assertEquals(0, c2.compareTo(c2));
		assertEquals(-1, c1.compareTo(c2));
		assertEquals(1, c2.compareTo(c1));
		assertEquals(-1, c1.compareTo(c1.add(new BigIntegerNaturals(1))));
		assertEquals(0, c1.compareTo(c1.add(new BigIntegerNaturals(0))));
	}
}
