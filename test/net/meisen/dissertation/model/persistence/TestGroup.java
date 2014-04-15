package net.meisen.dissertation.model.persistence;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * Tests the implementation of {@code SelectGroup}.
 * 
 * @author pmeisen
 * 
 */
public class TestGroup {

	/**
	 * Tests an empty group.
	 */
	@Test
	public void testEmptyGroup() {
		final Group group = new Group();

		assertEquals("", group.toString("/"));
		assertTrue(group.equals(group));
		assertTrue(group.equals(new Group()));
		assertFalse(group.equals(new Group("value", "deeper")));
		assertFalse(group.equals(null));
		assertFalse(group.equals(new Group("")));

		assertFalse(group.hasItems());
	}

	/**
	 * Tests a single element to make a group.
	 */
	@Test
	public void testSingleGroup() {
		Group group;

		group = new Group("single");
		assertEquals(group.toString("/"), "single");
		assertTrue(group.equals(group));
		assertTrue(group.equals(new Group("single")));
		assertFalse(group.equals(new Group()));
		assertFalse(group.equals(new Group("")));
		assertFalse(group.equals(new Group("SINGLE")));
		assertFalse(group.equals(new Group("Single")));
		assertFalse(group.equals(new Group("other")));
		assertFalse(group.equals(new Group("value", "deeper")));
		assertFalse(group.equals(null));

		assertTrue(group.hasItems());

		group = new Group("");
		assertEquals("", group.toString("/"));
		assertTrue(group.hasItems());
	}

	/**
	 * Tests the usage of a group with multiple elements.
	 */
	@Test
	public void testMultipleGroup() {
		Group group;

		group = new Group("first", "second");

		assertEquals("first/second", group.toString("/"));
		assertTrue(group.equals(group));
		assertTrue(group.equals(new Group("first", "second")));
		assertFalse(group.equals(new Group("second", "first")));
		assertFalse(group.equals(new Group("")));
		assertTrue(group.hasItems());

		group = new Group("first", "");
		assertEquals("first/", group.toString("/"));
		assertTrue(group.hasItems());
	}

	/**
	 * Tests the creation of a {@code SelectGroup} based on a string.
	 */
	@Test
	public void testCreationFromString() {
		Group cmpGroup, group;

		cmpGroup = new Group();
		group = Group.createFromString(null, ".");
		assertEquals(cmpGroup, group);

		cmpGroup = new Group("");
		group = Group.createFromString("", ".");
		assertEquals(cmpGroup, group);

		cmpGroup = new Group("the", "package", "is", "great");
		group = Group.createFromString("the.package.is.great", ".");
		assertEquals(cmpGroup, group);
	}
}
