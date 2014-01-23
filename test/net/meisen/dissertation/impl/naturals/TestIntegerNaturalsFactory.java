package net.meisen.dissertation.impl.naturals;

import static org.junit.Assert.assertEquals;

import net.meisen.dissertation.impl.naturals.IntegerNaturals;
import net.meisen.dissertation.impl.naturals.IntegerNaturalsFactory;

import org.junit.Test;

/**
 * Tests the implementation of {@code IntegerNaturalsFactory},
 * 
 * @author pmeisen
 * 
 */
public class TestIntegerNaturalsFactory {

	/**
	 * Check the generation of any value
	 */
	@Test
	public void testGeneration() {
		final IntegerNaturalsFactory factory = new IntegerNaturalsFactory();

		assertEquals(new IntegerNaturals(0), factory.generate(0));
		assertEquals(new IntegerNaturals(14), factory.generate(14));
		assertEquals(new IntegerNaturals(27), factory.generate(27));
	}

	/**
	 * Check the generation of zero.
	 */
	@Test
	public void testZero() {
		final IntegerNaturalsFactory factory = new IntegerNaturalsFactory();

		assertEquals(new IntegerNaturals(0), factory.getZero());
	}
}
