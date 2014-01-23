package net.meisen.dissertation.impl.indexes;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.LinkedHashMap;

import net.meisen.dissertation.impl.indexes.MapIndexedCollection;
import net.meisen.dissertation.impl.indexes.NestedIndexedCollection;
import net.meisen.dissertation.impl.indexes.TroveIntIndexedCollection;
import net.meisen.dissertation.impl.indexes.mock.PersonMock;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.IndexedCollectionDefinition;

import org.junit.Before;
import org.junit.Test;

/**
 * Tests the implementation of a {@code NestedIndexedCollection}.
 * 
 * @author pmeisen
 * 
 */
public class TestNestedIndexedCollection {

	private NestedIndexedCollection subject;
	private PersonMock[] mocks;

	/**
	 * Initializes the test {@code subjec}.
	 */
	@Before
	public void init() {

		// define the IndexKeyDefinition
		final IndexKeyDefinition defKey = new IndexKeyDefinition(
				PersonMock.class, "getName", "getAge", "getFirstName");

		// define the IndexedCollectionDefinition
		final IndexedCollectionDefinition defName = new IndexedCollectionDefinition(
				MapIndexedCollection.class,
				IndexedCollectionDefinition.INDEXKEYDEFINITION_PLACEHOLDER,
				HashMap.class);
		final IndexedCollectionDefinition defAge = new IndexedCollectionDefinition(
				TroveIntIndexedCollection.class);
		final IndexedCollectionDefinition defFirstName = new IndexedCollectionDefinition(
				MapIndexedCollection.class,
				IndexedCollectionDefinition.INDEXKEYDEFINITION_PLACEHOLDER,
				LinkedHashMap.class);

		// create the nestedCollection
		subject = new NestedIndexedCollection(defKey, defName, defAge,
				defFirstName);
	}

	private void addSome() {

		mocks = new PersonMock[] { new PersonMock("Meisen", "Philipp", 32),
				new PersonMock("Meisen", "Tobias", 32),
				new PersonMock("Meisen", "Andrea", 35),
				new PersonMock("Meisen", "Holger", 39),
				new PersonMock("Meyer", "Jörg", 40),
				new PersonMock("Lockhausen", "Bodo", 34),
				new PersonMock("Pehle", "Jann", 30),
				new PersonMock("Meisen", "Deborah", 32),
				new PersonMock("Meisen", "Edison", 1) };

		// add some objects
		for (final PersonMock mock : mocks) {
			assertTrue(subject.addObject(mock));
		}
		for (final PersonMock mock : mocks) {
			assertFalse(subject.addObject(mock));
		}
	}

	/**
	 * Tests a {@code NestedIndexedCollection} with only one
	 * {@code IndexedCollectionDefinition}, i.e. with a single key.
	 */
	@Test
	public void testSingleNestedCollection() {
		final IndexKeyDefinition defKey = new IndexKeyDefinition(
				PersonMock.class, "getAge");
		final IndexedCollectionDefinition defAge = new IndexedCollectionDefinition(
				TroveIntIndexedCollection.class);

		subject = new NestedIndexedCollection(defKey, defAge);
		assertTrue(subject.addObject(new PersonMock("Meisen", "Philipp", 32)));
		assertTrue(subject.addObject(new PersonMock("Meyer", "Jörg", 40)));
		assertFalse(subject.addObject(new PersonMock("Meisen", "Tobias", 32)));

		assertEquals(new PersonMock("Meisen", "Philipp", 32),
				subject.getObject(32));
	}

	/**
	 * Tests the implementation of
	 * {@link NestedIndexedCollection#addObject(Object)}.
	 */
	@Test
	public void testAddObjects() {
		addSome();
	}

	/**
	 * Tests the implementation of
	 * {@link NestedIndexedCollection#getObject(Object...)}.
	 */
	@Test
	public void testGetObject() {
		addSome();

		assertNotNull(subject.getObject("Meisen", 32, "Philipp"));
		assertNotNull(subject.getObject("Meisen", 32, "Tobias"));
		assertNotNull(subject.getObject("Meyer", 40, "Jörg"));
		assertNotNull(subject.getObject("Pehle", 30, "Jann"));
		assertNull(subject.getObject("Pehle", 32, "Jann"));

		// make sure the stuff is equal
		assertEquals(new PersonMock("Meisen", "Philipp", 32),
				subject.getObject("Meisen", 32, "Philipp"));
	}

	/**
	 * Tests the implementation of
	 * {@link NestedIndexedCollection#getObjects(Object...)}.
	 */
	@Test
	public void testGetObjects() {
		addSome();

		// check getAll and getObjects
		assertEquals(9, subject.getAll().size());
		assertEquals(9, subject.getObjects().size());
		assertEquals(1, subject.getObjects("Meisen", 32, "Philipp").size());
		assertEquals(0, subject.getObjects("Meisen", 400, "Philipp").size());
		assertEquals(3, subject.getObjects("Meisen", 32).size());
		assertEquals(6, subject.getObjects("Meisen").size());

		// check containsObject
		assertTrue(subject.containsObject(new PersonMock("Meisen", "Philipp",
				32)));
		assertFalse(subject.containsObject(new PersonMock("Meisen", "Philipp",
				400)));
	}

	/**
	 * Tests removal of objects
	 */
	@Test
	public void testRemoveObject() {
		addSome();

		// check for the existence
		for (final PersonMock mock : mocks) {
			assertTrue(subject.containsObject(mock));
			subject.removeObject(mock);
			assertFalse(subject.containsObject(mock));
		}
	}
}
