package net.meisen.dissertation.impl.indexes;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.UUID;

import net.meisen.dissertation.impl.indexes.MapIndexedCollection;
import net.meisen.dissertation.impl.indexes.MultipleIndexedCollection;
import net.meisen.dissertation.impl.indexes.TroveIntIndexedCollection;
import net.meisen.dissertation.impl.indexes.mock.PersonMock;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.IndexedCollectionDefinition;

import org.junit.Rule;
import org.junit.Test;
import org.junit.matchers.JUnitMatchers;
import org.junit.rules.ExpectedException;

/**
 * Tests the implementation of the {@code MultipleIndexedCollection}.
 * 
 * @author pmeisen
 * 
 */
public class TestMultipleIndexedCollection {

	/**
	 * Rule to evaluate exceptions
	 */
	@Rule
	public ExpectedException thrown = ExpectedException.none();

	/**
	 * Tests the usage of the index
	 */
	@Test
	public void testUsage() {

		final IndexKeyDefinition key1 = new IndexKeyDefinition(
				PersonMock.class, "getAge");
		final IndexKeyDefinition key2 = new IndexKeyDefinition(
				PersonMock.class, "getName", "getFirstName");

		final IndexedCollectionDefinition def1 = new IndexedCollectionDefinition(
				TroveIntIndexedCollection.class);
		final IndexedCollectionDefinition def2 = new IndexedCollectionDefinition(
				MapIndexedCollection.class, HashMap.class);

		final MultipleIndexedCollection idx = new MultipleIndexedCollection(
				new IndexKeyDefinition[] { key1, key2 },
				new IndexedCollectionDefinition[] { def1, def2 });

		// add some objects
		final PersonMock philipp = new PersonMock("Meisen", "Philipp", 33);
		final PersonMock deborah = new PersonMock("Meisen", "Deborah", 32);
		final PersonMock andrea = new PersonMock("Meisen", "Andrea", 35);
		final PersonMock holger = new PersonMock("Meisen", "Holger", 39);

		// add some persons
		assertTrue(idx.addObject(philipp));
		assertTrue(idx.addObject(deborah));
		assertTrue(idx.addObject(andrea));
		assertTrue(idx.addObject(holger));

		// get some persons
		assertEquals(philipp, idx.getObject(33));
		assertEquals(holger, idx.getObject(39));
		assertEquals(andrea, idx.getObject("Meisen", "Andrea"));
		assertEquals(deborah, idx.getObject("Meisen", "Deborah"));
		assertNull(idx.getObject("Meisen", "Unknown"));
		assertNull(idx.getObject(40));

		// check contains
		assertTrue(idx.containsObject(philipp));
		assertTrue(idx.containsObject(deborah));
		assertTrue(idx.containsObject(andrea));
		assertTrue(idx.containsObject(holger));
		assertFalse(idx
				.containsObject(new PersonMock("Meisen", "Unknown", 100)));

		// remove some objects
		assertTrue(idx.containsObject(philipp));
		idx.removeObject(philipp);
		assertFalse(idx.containsObject(philipp));
		assertTrue(idx.containsObject(deborah));
		idx.removeObject(deborah);
		assertFalse(idx.containsObject(deborah));
		assertTrue(idx.containsObject(andrea));
		idx.removeObject(andrea);
		assertFalse(idx.containsObject(andrea));
	}

	/**
	 * Tests that the sizes of the definitions (i.e. of
	 * {@code IndexKeyDefinition} and {@code IndexedCollectionDefinition} must
	 * be equal.
	 */
	@Test
	public void testInvalidAmountOfDefinitions() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("size of the definitions must be equal"));

		final IndexKeyDefinition key1 = new IndexKeyDefinition(UUID.class,
				"toString");
		final IndexKeyDefinition key2 = new IndexKeyDefinition(UUID.class,
				"getClass");

		new MultipleIndexedCollection(new IndexKeyDefinition[] { key1, key2 },
				new IndexedCollectionDefinition[] {});
	}

	/**
	 * Tests the exception to be thrown when the objectClasses of the keys are
	 * not equal.
	 */
	@Test
	public void testExceptionWhenDifferentObjectClasses() {
		thrown.expect(IllegalArgumentException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("objectClasses of all keys must be equal"));

		final IndexKeyDefinition key1 = new IndexKeyDefinition(UUID.class,
				"toString");
		final IndexKeyDefinition key2 = new IndexKeyDefinition(String.class,
				"toString");
		final IndexedCollectionDefinition def = new IndexedCollectionDefinition(
				MapIndexedCollection.class, HashMap.class);
		new MultipleIndexedCollection(new IndexKeyDefinition[] { key1, key2 },
				new IndexedCollectionDefinition[] { def, def });
	}
}
