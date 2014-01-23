package net.meisen.dissertation.model.time;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import net.meisen.dissertation.impl.naturals.BigIntegerNaturals;
import net.meisen.dissertation.impl.naturals.BigIntegerNaturalsFactory;
import net.meisen.dissertation.model.time.RawTimeInterval;
import net.meisen.dissertation.model.time.RawTimeIntervalFactory;

import org.junit.Test;

/**
 * Tests the implementation of the {@code RawTimeIntervalFactory}.
 * 
 * @author pmeisen
 * 
 */
public class TestRawTimeIntervalFactory {

	/**
	 * Tests the factory with a {@code BigIntegerNaturalsFactory}.
	 */
	@Test
	public void testBigIntegerIntervalCreation() {
		final BigIntegerNaturalsFactory naturalsFactory = new BigIntegerNaturalsFactory();
		final RawTimeIntervalFactory<BigIntegerNaturals> factory = new RawTimeIntervalFactory<BigIntegerNaturals>(
				naturalsFactory);

		final RawTimeInterval<BigIntegerNaturals> interval = factory
				.createInterval(5, 5);
		assertNotNull(interval);
		assertEquals(naturalsFactory.generate(5), interval.getDuration());
	}
}
