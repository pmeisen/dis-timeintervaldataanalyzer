package net.meisen.dissertation.impl.naturals;

import static org.junit.Assert.assertEquals;

import net.meisen.dissertation.impl.naturals.LongNaturals;
import net.meisen.dissertation.impl.naturals.LongNaturalsFactory;

import org.junit.Test;

/**
 * Tests the implementation of {@code LongNaturalsFactory},
 * 
 * @author pmeisen
 * 
 */
public class TestLongNaturalsFactory {

	/**
	 * Check the generation of any value
	 */
	@Test
	public void testGeneration() {
		final LongNaturalsFactory factory = new LongNaturalsFactory();

		assertEquals(new LongNaturals(0), factory.generate(0));
		assertEquals(new LongNaturals(14), factory.generate(14l));
		assertEquals(new LongNaturals(27), factory.generate(27l));
	}

	/**
	 * Check the generation of zero.
	 */
	@Test
	public void testZero() {
		final LongNaturalsFactory factory = new LongNaturalsFactory();

		assertEquals(new LongNaturals(0), factory.getZero());
	}
}
