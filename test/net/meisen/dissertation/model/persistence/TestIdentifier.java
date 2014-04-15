package net.meisen.dissertation.model.persistence;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Tests the implementation of a {@code Identifier}.
 * 
 * @author pmeisen
 * 
 */
public class TestIdentifier {

	/**
	 * Tests the usage of a single value as {@code Identifier}
	 */
	@Test
	public void testSingleId() {
		final Identifier id = new Identifier("TEST");

		assertEquals("TEST", id.toString());
		assertTrue(id.equals(id));
		assertTrue(id.equals(new Identifier("TEST")));
		assertFalse(id.equals(new Identifier("TEST", "ALAL")));
		assertFalse(id.equals(null));
	}

	/**
	 * Tests the usage of a id and a group to define a {@code Identifier}.
	 */
	@Test
	public void testGroupWithId() {
		final Identifier id = new Identifier("TEST", "my", "group");

		assertEquals("my.group.TEST", id.toString("."));
		assertTrue(id.equals(id));
		assertTrue(id.equals(new Identifier("TEST", "my", "group")));
		assertFalse(id.equals(new Identifier("TEST", "my", "SelectGroup")));
		assertFalse(id.equals(null));
	}

	/**
	 * Tests the creation of an {@code Identifier} based on a string.
	 */
	@Test
	public void testCreationByString() {
		Identifier cmpId, id;

		cmpId = new Identifier(null);
		id = Identifier.createFromString(null);
		assertEquals(cmpId, id);

		cmpId = new Identifier("hallo");
		id = Identifier.createFromString("hallo", ".");
		assertEquals(cmpId, id);

		cmpId = new Identifier("hallo", "this", "is", "my");
		id = Identifier.createFromString("this.is.my.hallo", ".");
		assertEquals(cmpId, id);
	}
}
