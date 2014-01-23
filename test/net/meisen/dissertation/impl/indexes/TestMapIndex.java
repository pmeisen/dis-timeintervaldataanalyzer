package net.meisen.dissertation.impl.indexes;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.UUID;

import net.meisen.dissertation.impl.indexes.MapIndexedCollection;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;

import org.junit.Test;

/**
 * Tests the implementation of a {@code MapIndexedCollection}.
 * 
 * @author pmeisen
 * 
 */
public class TestMapIndex {

	/**
	 * Tests the {@link MapIndexedCollection#containsObject(Object)} method
	 * using a {@code HashMap} implementation.
	 */
	@Test
	public void testHashMapIndexContainsObject() {
		final IndexKeyDefinition idxDef = new IndexKeyDefinition(Integer.class);
		final MapIndexedCollection idx = new MapIndexedCollection(idxDef,
				HashMap.class);

		// add some objects
		assertEquals(0, idx.addObjects(5, 6, 7, 8, 9, 10).size());

		// check if objects can be found via contains
		assertTrue(idx.containsObject(5));
		assertTrue(idx.containsObject(6));
		assertTrue(idx.containsObject(7));
		assertTrue(idx.containsObject(8));
		assertTrue(idx.containsObject(9));
		assertTrue(idx.containsObject(10));

		// check that some aren't found
		assertFalse(idx.containsObject(-5));
		assertFalse(idx.containsObject(0));
		assertFalse(idx.containsObject(1));
		assertFalse(idx.containsObject(2));
		assertFalse(idx.containsObject(3));
		assertFalse(idx.containsObject(4));
		assertFalse(idx.containsObject(11));
	}

	/**
	 * Tests the {@link MapIndexedCollection#getObject(Object...)} method using
	 * a {@code HashMap} implementation.
	 */
	@Test
	public void testHashMapIndexGet() {
		final IndexKeyDefinition idxDef = new IndexKeyDefinition(UUID.class,
				"toString");
		final MapIndexedCollection idx = new MapIndexedCollection(idxDef,
				HashMap.class);

		// create some objects
		final List<String> ids = new ArrayList<String>();
		for (int i = 0; i < 100; i++) {
			final UUID o = UUID.randomUUID();
			ids.add(o.toString());
			idx.addObject(o);
		}

		// check for the existence
		for (final String id : ids) {
			assertNotNull(idx.getObject(id));
			assertTrue(idx.containsObject(idx.getObject(id)));
		}
	}

	/**
	 * Tests removal of objects
	 */
	@Test
	public void testHashMapIndexRemove() {
		final IndexKeyDefinition idxDef = new IndexKeyDefinition(UUID.class,
				"toString");
		final MapIndexedCollection idx = new MapIndexedCollection(idxDef,
				HashMap.class);

		// create some objects
		final List<String> ids = new ArrayList<String>();
		for (int i = 0; i < 100; i++) {
			final UUID o = UUID.randomUUID();
			ids.add(o.toString());
			idx.addObject(o);
		}

		// check for the existence
		for (final String id : ids) {
			final Object object = idx.getObject(id);
			assertNotNull(object);
			idx.removeObject(object);
			assertFalse(idx.containsObject(object));
		}
	}
}
