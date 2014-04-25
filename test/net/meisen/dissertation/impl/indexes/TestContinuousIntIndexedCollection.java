package net.meisen.dissertation.impl.indexes;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;

import org.junit.Test;

/**
 * Tests the implementation of a {@code ContinuousIntIndexedCollection}.
 * 
 * @author pmeisen
 * 
 */
public class TestContinuousIntIndexedCollection {

	/**
	 * Tests the adding of elements to the tail.
	 */
	@Test
	public void testContinuousTailAppending() {

		for (int base = 0; base < 100; base++) {
			final IndexKeyDefinition keyDef = new IndexKeyDefinition(
					Integer.class);
			final ContinuousIntIndexedCollection idx = new ContinuousIntIndexedCollection(
					keyDef);

			for (int i = base; i < 1000; i++) {
				final String out = "base: " + base + ", run: " + i;

				assertTrue(out, idx.addObject(i));
				assertEquals(out, idx.getOffset(), base);
				assertEquals(out, i + 1 - base, idx.size());
				assertEquals(out, i, idx.getObject(i));
			}
		}
	}

	/**
	 * Tests the adding of elements to the head.
	 */
	@Test
	public void testContinuousHeadAppending() {

		for (int base = 0; base < 100; base++) {
			final IndexKeyDefinition keyDef = new IndexKeyDefinition(
					Integer.class);
			final ContinuousIntIndexedCollection idx = new ContinuousIntIndexedCollection(
					keyDef);

			for (int i = 1000; i >= base; i--) {
				final String out = "base: " + base + ", run: " + i;

				assertTrue(out, idx.addObject(i));
				assertEquals(out, idx.getOffset(), i);
				assertEquals(out, 1000 - i + 1, idx.size());
				assertEquals(out, i, idx.getObject(i));
			}
		}
	}

	/**
	 * Tests the removing of elements from the head of the continuous index.
	 */
	@Test
	public void testRemovingFromHead() {
		final IndexKeyDefinition keyDef = new IndexKeyDefinition(Integer.class);
		final ContinuousIntIndexedCollection idx = new ContinuousIntIndexedCollection(
				keyDef);

		// remove some not existing values
		for (int i = 0; i < 100; i++) {
			idx.removeObject(i);
		}

		// add 100 objects
		for (int i = 10; i < 110; i++) {
			idx.addObject(i);
		}

		// remove each object from the head
		for (int i = 10; i < 110; i++) {
			assertEquals(110 - i, idx.size());
			idx.removeObject(i);
			assertEquals(109 - i, idx.size());
		}
	}

	/**
	 * Tests the removing of elements from the tail of the continuous index.
	 */
	@Test
	public void testRemovingFromTail() {
		final IndexKeyDefinition keyDef = new IndexKeyDefinition(Integer.class);
		final ContinuousIntIndexedCollection idx = new ContinuousIntIndexedCollection(
				keyDef);

		// remove some not existing values
		for (int i = 0; i < 100; i++) {
			idx.removeObject(i);
		}

		// add 100 objects
		for (int i = 10; i < 110; i++) {
			idx.addObject(i);
		}

		// remove each object from the tail
		for (int i = 109; i >= 10; i--) {
			assertEquals(i - 9, idx.size());
			idx.removeObject(i);
			assertEquals(i - 10, idx.size());
		}
	}
}
