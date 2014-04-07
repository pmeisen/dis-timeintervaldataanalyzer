package net.meisen.dissertation.impl.descriptors;

import static org.junit.Assert.assertEquals;

import java.util.Locale;
import java.util.UUID;


import net.meisen.dissertation.impl.descriptors.IntegerDescriptor;

import org.junit.Test;

/**
 * Tests the implementation of the {@code IntegerDescriptor}.
 * 
 * @author pmeisen
 * 
 */
public class TestIntegerDescriptor {

	/**
	 * Tests the usage of an id based on a {@code UUID}.
	 */
	@Test
	public void testWithUUID() {
		final UUID id = UUID.randomUUID();
		IntegerDescriptor<UUID> descriptor = new IntegerDescriptor<UUID>(null,
				id);

		assertEquals(id, descriptor.getId());
		assertEquals(new Integer(0), descriptor.getValue());
	}

	/**
	 * Tests the usage of an id based on a {@code Integer}.
	 */
	@Test
	public void testWithInteger() {
		final int id = 5;
		IntegerDescriptor<Integer> descriptor = new IntegerDescriptor<Integer>(
				null, id);

		assertEquals(new Integer(id), descriptor.getId());
		assertEquals(new Integer(0), descriptor.getValue());
	}
	
	/**
	 * Tests the formatting of an integer as unique string.
	 */
	@Test
	public void testUniqueString() {
		final Locale def = Locale.getDefault();

		final IntegerDescriptor<Integer> desc1, desc2, desc3, desc4, desc5;
		desc1 = new IntegerDescriptor<Integer>(null, 1, 5);
		desc2 = new IntegerDescriptor<Integer>(null, 2, 0);
		desc3 = new IntegerDescriptor<Integer>(null, 3, -1200);
		desc4 = new IntegerDescriptor<Integer>(null, 4, 13000);
		desc5 = new IntegerDescriptor<Integer>(null, 5, 1200);

		Locale.setDefault(Locale.GERMAN);
		assertEquals("5", desc1.getUniqueString());
		assertEquals("0", desc2.getUniqueString());
		assertEquals("-1200", desc3.getUniqueString());
		assertEquals("13000", desc4.getUniqueString());
		assertEquals("1200", desc5.getUniqueString());

		Locale.setDefault(Locale.FRANCE);
		assertEquals("5", desc1.getUniqueString());
		assertEquals("0", desc2.getUniqueString());
		assertEquals("-1200", desc3.getUniqueString());
		assertEquals("13000", desc4.getUniqueString());
		assertEquals("1200", desc5.getUniqueString());

		Locale.setDefault(Locale.US);
		assertEquals("5", desc1.getUniqueString());
		assertEquals("0", desc2.getUniqueString());
		assertEquals("-1200", desc3.getUniqueString());
		assertEquals("13000", desc4.getUniqueString());
		assertEquals("1200", desc5.getUniqueString());

		Locale.setDefault(def);
	}
}
