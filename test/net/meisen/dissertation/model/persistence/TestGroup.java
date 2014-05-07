package net.meisen.dissertation.model.persistence;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

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
		assertNull(group.getParentGroup());
		assertNull(group.removePrefix(new Group()));
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
		assertEquals("single", group.getPart(0));
		assertEquals(new Group(), group.getParentGroup());
		assertEquals(group, group.removePrefix(new Group()));
		assertEquals(new Group(), group.removePrefix(new Group("single")));

		group = new Group("");
		assertEquals("", group.toString("/"));
		assertTrue(group.hasItems());
		assertEquals(1, group.size());
		assertEquals("", group.getPart(0));
		assertEquals(new Group(), group.getParentGroup());
		assertEquals(group, group.removePrefix(new Group()));
		assertEquals(new Group(), group.removePrefix(new Group("")));
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
		assertEquals(new Group("first"), group.getParentGroup());
		assertEquals(2, group.size());
		assertEquals("first", group.getPart(0));
		assertEquals("second", group.getPart(1));
		assertEquals(group, group.removePrefix(new Group()));
		assertEquals(new Group("second"),
				group.removePrefix(new Group("first")));
		assertEquals(new Group(),
				group.removePrefix(new Group("first", "second")));
		assertNull(group.removePrefix(new Group("second")));

		group = new Group("first", "");
		assertEquals("first/", group.toString("/"));
		assertTrue(group.hasItems());
		assertEquals(new Group("first"), group.getParentGroup());
		assertEquals(2, group.size());
		assertEquals("first", group.getPart(0));
	}

	/**
	 * Tests the iteration over a group.
	 */
	@Test
	public void testIteration() {
		Group group;
		Iterator<Group> it;

		group = new Group("first", "second");
		it = group.iterator();
		assertTrue(it.hasNext());
		assertTrue(it.hasNext());
		assertEquals(new Group("first"), it.next());
		assertTrue(it.hasNext());
		assertEquals(new Group("first", "second"), it.next());
		assertFalse(it.hasNext());

		group = new Group("");
		it = group.iterator();
		assertTrue(it.hasNext());
		assertEquals(new Group(""), it.next());
		assertFalse(it.hasNext());

		group = new Group();
		it = group.iterator();
		assertFalse(it.hasNext());
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
