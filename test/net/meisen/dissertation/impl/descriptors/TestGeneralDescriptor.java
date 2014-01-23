package net.meisen.dissertation.impl.descriptors;

import static org.junit.Assert.assertEquals;

import java.util.UUID;


import net.meisen.dissertation.impl.descriptors.GeneralDescriptor;

import org.junit.Test;

/**
 * Tests the implementation of the {@code GeneralDescriptor}.
 * 
 * @author pmeisen
 * 
 */
public class TestGeneralDescriptor {

	/**
	 * Tests the usage of an id based on a {@code UUID}.
	 */
	@Test
	public void testWithUUID() {
		final UUID id = UUID.randomUUID();
		GeneralDescriptor<UUID> descriptor = new GeneralDescriptor<UUID>(null,
				id, null);

		assertEquals(id, descriptor.getId());
		assertEquals(null, descriptor.getValue());
	}

	/**
	 * Tests the usage of an id based on a {@code Integer}.
	 */
	@Test
	public void testWithInteger() {
		final int id = 5;
		GeneralDescriptor<Integer> descriptor = new GeneralDescriptor<Integer>(
				null, id, null);

		assertEquals(new Integer(id), descriptor.getId());
		assertEquals(null, descriptor.getValue());
	}
}
