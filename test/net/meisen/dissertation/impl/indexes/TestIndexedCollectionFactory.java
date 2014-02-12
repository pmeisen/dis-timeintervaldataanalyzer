package net.meisen.dissertation.impl.indexes;

import static org.junit.Assert.assertEquals;

import java.util.UUID;

import net.meisen.dissertation.model.indexes.IndexKeyDefinition;

import org.junit.Test;

/**
 * Tests the implementation of {@code IndexedCollectionFactory}.
 * 
 * @author pmeisen
 * 
 */
public class TestIndexedCollectionFactory {

	/**
	 * Tests the creation of an index based on a single key-type.
	 */
	@Test
	public void testFromTypeCreation() {
		IndexKeyDefinition keyDef;
		final IndexedCollectionFactory factory = new IndexedCollectionFactory();

		// Test a byte key
		keyDef = new IndexKeyDefinition(byte.class);
		assertEquals(TroveByteIndexedCollection.class, factory.create(keyDef)
				.getClass());
		keyDef = new IndexKeyDefinition(Byte.class);
		assertEquals(TroveByteIndexedCollection.class, factory.create(keyDef)
				.getClass());

		// Test a short key
		keyDef = new IndexKeyDefinition(short.class);
		assertEquals(TroveShortIndexedCollection.class, factory.create(keyDef)
				.getClass());
		keyDef = new IndexKeyDefinition(Short.class);
		assertEquals(TroveShortIndexedCollection.class, factory.create(keyDef)
				.getClass());

		// Test an integer key
		keyDef = new IndexKeyDefinition(int.class);
		assertEquals(TroveIntIndexedCollection.class, factory.create(keyDef)
				.getClass());
		keyDef = new IndexKeyDefinition(Integer.class);
		assertEquals(TroveIntIndexedCollection.class, factory.create(keyDef)
				.getClass());

		// Test a long key
		keyDef = new IndexKeyDefinition(long.class);
		assertEquals(TroveLongIndexedCollection.class, factory.create(keyDef)
				.getClass());
		keyDef = new IndexKeyDefinition(Long.class);
		assertEquals(TroveLongIndexedCollection.class, factory.create(keyDef)
				.getClass());

		// Test a UUID key
		keyDef = new IndexKeyDefinition(UUID.class);
		assertEquals(MapIndexedCollection.class, factory.create(keyDef)
				.getClass());
	}
}
