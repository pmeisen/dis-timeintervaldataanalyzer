package net.meisen.dissertation.models.impl.naturals;

import static org.junit.Assert.assertEquals;

import java.math.BigInteger;

import org.junit.Test;

/**
 * Tests the implementation of {@code BigIntegerNaturalsFactory},
 * 
 * @author pmeisen
 * 
 */
public class TestBigIntegerNaturalsFactory {

	/**
	 * Check the generation of any value
	 */
	@Test
	public void testGeneration() {
		final BigIntegerNaturalsFactory factory = new BigIntegerNaturalsFactory();

		assertEquals(new BigIntegerNaturals(0), factory.generate(0));
		assertEquals(new BigIntegerNaturals(14), factory.generate(14l));
		assertEquals(new BigIntegerNaturals(27), factory.generate(27l));
		assertEquals(new BigIntegerNaturals(1500),
				factory.generate(BigInteger.valueOf(1500)));
	}

	/**
	 * Check the generation of zero.
	 */
	@Test
	public void testZero() {
		final BigIntegerNaturalsFactory factory = new BigIntegerNaturalsFactory();

		assertEquals(new BigIntegerNaturals(0), factory.getZero());
	}
}
