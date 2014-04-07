package net.meisen.dissertation.impl.descriptors;

import static org.junit.Assert.assertEquals;

import java.util.Locale;
import java.util.UUID;

import net.meisen.dissertation.impl.descriptors.LongDescriptor;

import org.junit.Test;

/**
 * Tests the implementation of the {@code LongDescriptor}.
 * 
 * @author pmeisen
 * 
 */
public class TestLongDescriptor {

	/**
	 * Tests the usage of an id based on a {@code UUID}.
	 */
	@Test
	public void testWithUUID() {
		final UUID id = UUID.randomUUID();
		LongDescriptor<UUID> descriptor = new LongDescriptor<UUID>(null, id);

		assertEquals(id, descriptor.getId());
		assertEquals(new Long(0), descriptor.getValue());
	}

	/**
	 * Tests the usage of an id based on a {@code Integer}.
	 */
	@Test
	public void testWithInteger() {
		final int id = 5;
		LongDescriptor<Integer> descriptor = new LongDescriptor<Integer>(null,
				id);

		assertEquals(new Integer(id), descriptor.getId());
		assertEquals(new Long(0), descriptor.getValue());
	}

	/**
	 * Tests the formatting of a long as unique string.
	 */
	@Test
	public void testUniqueString() {
		final Locale def = Locale.getDefault();

		final LongDescriptor<Integer> desc1, desc2, desc3, desc4, desc5;
		desc1 = new LongDescriptor<Integer>(null, 1, 5l);
		desc2 = new LongDescriptor<Integer>(null, 2, 0l);
		desc3 = new LongDescriptor<Integer>(null, 3, -11232123200l);
		desc4 = new LongDescriptor<Integer>(null, 4, 13000l);
		desc5 = new LongDescriptor<Integer>(null, 5, 1200l);

		Locale.setDefault(Locale.GERMAN);
		assertEquals("5", desc1.getUniqueString());
		assertEquals("0", desc2.getUniqueString());
		assertEquals("-11232123200", desc3.getUniqueString());
		assertEquals("13000", desc4.getUniqueString());
		assertEquals("1200", desc5.getUniqueString());

		Locale.setDefault(Locale.FRANCE);
		assertEquals("5", desc1.getUniqueString());
		assertEquals("0", desc2.getUniqueString());
		assertEquals("-11232123200", desc3.getUniqueString());
		assertEquals("13000", desc4.getUniqueString());
		assertEquals("1200", desc5.getUniqueString());

		Locale.setDefault(Locale.US);
		assertEquals("5", desc1.getUniqueString());
		assertEquals("0", desc2.getUniqueString());
		assertEquals("-11232123200", desc3.getUniqueString());
		assertEquals("13000", desc4.getUniqueString());
		assertEquals("1200", desc5.getUniqueString());

		Locale.setDefault(def);
	}
}
