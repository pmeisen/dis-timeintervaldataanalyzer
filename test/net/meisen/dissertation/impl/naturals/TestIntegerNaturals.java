package net.meisen.dissertation.impl.naturals;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import net.meisen.dissertation.impl.naturals.IntegerNaturals;

import org.junit.Test;

/**
 * Tests the implementation of the {@code IntegerNaturals}.
 * 
 * @author pmeisen
 * 
 */
public class TestIntegerNaturals {

	/**
	 * Tests the addition of {@code 0 + 10} as example addition with {@code 0}
	 */
	@Test
	public void testNullAddition() {
		final IntegerNaturals summand1 = new IntegerNaturals(0);
		final IntegerNaturals summand2 = new IntegerNaturals(10);

		assertEquals(new IntegerNaturals(10), summand1.add(summand2));
		assertEquals(new IntegerNaturals(0), summand1);
		assertEquals(new IntegerNaturals(10), summand2);
	}

	/**
	 * Tests the addition of {@code 5 + 10} as example addition
	 */
	@Test
	public void testSimpleAddition() {
		final IntegerNaturals summand1 = new IntegerNaturals(5);
		final IntegerNaturals summand2 = new IntegerNaturals(10);

		assertEquals(new IntegerNaturals(15), summand1.add(summand2));
		assertEquals(new IntegerNaturals(5), summand1);
		assertEquals(new IntegerNaturals(10), summand2);
	}

	/**
	 * Tests the subtraction of {@code 15 -10} as example subtraction
	 */
	@Test
	public void testSubstraction() {
		final IntegerNaturals minuend = new IntegerNaturals(15);
		final IntegerNaturals subtrahend = new IntegerNaturals(10);

		assertEquals(new IntegerNaturals(5), minuend.subtract(subtrahend));
		assertEquals(new IntegerNaturals(15), minuend);
		assertEquals(new IntegerNaturals(10), subtrahend);
	}
	
	/**
	 * Tests the equality checking
	 */
	@Test
	public void testEquality() {
		final IntegerNaturals val = new IntegerNaturals(15);

		assertFalse(val.equals(null));
		assertTrue(val.equals(val));
		assertTrue(val.equals(15));
		assertFalse(val.equals(15l));
		assertFalse(val.equals(16l));
		assertFalse(val.equals(27));
	}
	
	/**
	 * Tests the implementation of {@code compareTo(LongNaturals)}.
	 */
	@Test
	public void testCompareTo() {
		final IntegerNaturals c1 = new IntegerNaturals(15);
		final IntegerNaturals c2 = new IntegerNaturals(20);

		assertEquals(0, c1.compareTo(c1));
		assertEquals(0, c2.compareTo(c2));
		assertEquals(-1, c1.compareTo(c2));
		assertEquals(1, c2.compareTo(c1));
		assertEquals(-1, c1.compareTo(c1.add(new IntegerNaturals(1))));
		assertEquals(0, c1.compareTo(c1.add(new IntegerNaturals(0))));
	}
}
