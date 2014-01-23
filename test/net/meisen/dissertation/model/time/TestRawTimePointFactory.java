package net.meisen.dissertation.model.time;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import net.meisen.dissertation.impl.naturals.IntegerNaturals;
import net.meisen.dissertation.impl.naturals.IntegerNaturalsFactory;
import net.meisen.dissertation.model.time.RawTimePoint;
import net.meisen.dissertation.model.time.RawTimePointFactory;

import org.junit.Test;

/**
 * Tests the implementation of the {@code RawTimePointFactory}.
 * 
 * @author pmeisen
 * 
 */
public class TestRawTimePointFactory {

	/**
	 * Tests the factory with a {@code IntegerNaturalsFactory}.
	 */
	@Test
	public void testIntegerPointCreation() {
		final IntegerNaturalsFactory naturalsFactory = new IntegerNaturalsFactory();
		final RawTimePointFactory<IntegerNaturals> factory = new RawTimePointFactory<IntegerNaturals>(
				naturalsFactory);

		// check if 5 is created
		final RawTimePoint<IntegerNaturals> pointFive = factory.createPoint(5);
		assertNotNull(pointFive);
		assertEquals(naturalsFactory.generate(5), pointFive.getOffset());

		// check if 5 is created
		final RawTimePoint<IntegerNaturals> pointZero = factory.createPoint(0);
		assertNotNull(pointZero);
		assertEquals(naturalsFactory.getZero(), pointZero.getOffset());
	}
}
