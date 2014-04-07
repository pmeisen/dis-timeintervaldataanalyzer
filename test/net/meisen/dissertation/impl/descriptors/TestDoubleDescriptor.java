package net.meisen.dissertation.impl.descriptors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Locale;
import java.util.UUID;

import net.meisen.dissertation.impl.descriptors.DoubleDescriptor;
import net.meisen.dissertation.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.model.descriptors.DescriptorModel;

import org.junit.Test;

/**
 * Tests the implementation of the {@code DoubleDescriptor}.
 * 
 * @author pmeisen
 * 
 */
public class TestDoubleDescriptor {

	/**
	 * Tests the usage of an id based on a {@code UUID}.
	 */
	@Test
	public void testWithUUID() {
		final UUID id = UUID.randomUUID();
		final DoubleDescriptor<UUID> descriptor = new DoubleDescriptor<UUID>(
				null, id);

		assertEquals(id, descriptor.getId());
		assertEquals(0.0, descriptor.getValue(), 0.0);
	}

	/**
	 * Tests the usage of an id based on a {@code Integer}.
	 */
	@Test
	public void testWithInteger() {
		final int id = 5;
		final DoubleDescriptor<Integer> descriptor = new DoubleDescriptor<Integer>(
				null, id);

		assertEquals(new Integer(id), descriptor.getId());
		assertEquals(0.0, descriptor.getValue(), 0.0);
	}

	/**
	 * Tests the equality implementation
	 */
	@Test
	public void testEquality() {
		final DoubleDescriptor<Integer> d1 = new DoubleDescriptor<Integer>(
				null, 3, 5.0);
		final DoubleDescriptor<Integer> d2 = new DoubleDescriptor<Integer>(
				null, 3, 5.0);

		final DescriptorModel<Integer> model = new DescriptorModel<Integer>(
				"myId", DoubleDescriptor.class, new IntegerIdsFactory());
		final DoubleDescriptor<Integer> d3 = new DoubleDescriptor<Integer>(
				model, 3, 5.0);
		final DoubleDescriptor<Integer> d4 = new DoubleDescriptor<Integer>(
				model, 3, 5.0);

		final DoubleDescriptor<Integer> d5 = new DoubleDescriptor<Integer>(
				null, 5, 8.0);

		// null equality
		assertFalse(d1.equals(null));

		// identity
		assertTrue(d1.equals(d1));
		assertTrue(d2.equals(d2));
		assertTrue(d3.equals(d3));
		assertTrue(d4.equals(d4));
		assertTrue(d5.equals(d5));

		// check equality
		assertTrue(d1.equals(d2));
		assertTrue(d2.equals(d1));
		assertTrue(d3.equals(d4));
		assertTrue(d4.equals(d3));

		// check inequality
		assertFalse(d1.equals(d3));
		assertFalse(d3.equals(d1));
		assertFalse(d2.equals(d3));
		assertFalse(d3.equals(d2));
		assertFalse(d5.equals(d1));
		assertFalse(d1.equals(d5));
		assertFalse(d5.equals(d3));
		assertFalse(d3.equals(d5));
	}

	/**
	 * Tests the formatting of a double as unique string.
	 */
	@Test
	public void testUniqueString() {
		final Locale def = Locale.getDefault();

		final DoubleDescriptor<Integer> desc1, desc2, desc3, desc4, desc5;
		desc1 = new DoubleDescriptor<Integer>(null, 1, 5.000);
		desc2 = new DoubleDescriptor<Integer>(null, 2, 5.02000000);
		desc3 = new DoubleDescriptor<Integer>(null, 3, 0005.00120);
		desc4 = new DoubleDescriptor<Integer>(null, 4, 390128308.000212938000);
		desc5 = new DoubleDescriptor<Integer>(null, 5, 0001001.2319010000);

		Locale.setDefault(Locale.GERMAN);
		assertEquals("5", desc1.getUniqueString());
		assertEquals("5.02", desc2.getUniqueString());
		assertEquals("5.0012", desc3.getUniqueString());
		assertEquals("390128308.00021", desc4.getUniqueString());
		assertEquals("1001.2319", desc5.getUniqueString());

		Locale.setDefault(Locale.FRANCE);
		assertEquals("5", desc1.getUniqueString());
		assertEquals("5.02", desc2.getUniqueString());
		assertEquals("5.0012", desc3.getUniqueString());
		assertEquals("390128308.00021", desc4.getUniqueString());
		assertEquals("1001.2319", desc5.getUniqueString());

		Locale.setDefault(Locale.US);
		assertEquals("5", desc1.getUniqueString());
		assertEquals("5.02", desc2.getUniqueString());
		assertEquals("5.0012", desc3.getUniqueString());
		assertEquals("390128308.00021", desc4.getUniqueString());
		assertEquals("1001.2319", desc5.getUniqueString());

		Locale.setDefault(def);
	}
}
