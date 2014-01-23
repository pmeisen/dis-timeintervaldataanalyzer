package net.meisen.dissertation.data.impl.descriptors;

import static org.junit.Assert.assertEquals;

import java.util.UUID;


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
}
