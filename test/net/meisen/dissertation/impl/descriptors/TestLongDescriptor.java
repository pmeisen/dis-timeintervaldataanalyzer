package net.meisen.dissertation.impl.descriptors;

import static org.junit.Assert.assertEquals;

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
}
