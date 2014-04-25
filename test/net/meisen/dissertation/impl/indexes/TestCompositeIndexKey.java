package net.meisen.dissertation.impl.indexes;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.model.indexes.keys.CompositeIndexKey;

import org.junit.Test;

/**
 * Tests the implementation of a {@code CompositeIndexKey}.
 * 
 * @author pmeisen
 * 
 */
public class TestCompositeIndexKey {

	/**
	 * Tests the equality implementation
	 */
	@Test
	public void testEquality() {

		// the testSubjects
		CompositeIndexKey key1, key2;

		key1 = new CompositeIndexKey("A", "B", "D");
		key2 = new CompositeIndexKey("A", "B", "D");
		assertTrue(key1.equals(key2));
		assertTrue(key1.hashCode() == key2.hashCode());

		key1 = new CompositeIndexKey("A", null, "D");
		key2 = new CompositeIndexKey("A", null, "D");
		assertTrue(key1.equals(key2));
		assertTrue(key1.hashCode() == key2.hashCode());

		key1 = new CompositeIndexKey(1, 1.0, 5);
		key2 = new CompositeIndexKey(1, 1.0, 5);
		assertTrue(key1.equals(key2));
		assertTrue(key1.hashCode() == key2.hashCode());

		key1 = new CompositeIndexKey(1, 2, 3);
		key2 = new CompositeIndexKey(1, 2, 3.0);
		assertFalse(key1.equals(key2));
	}

	/**
	 * Tests the implemented comparison
	 */
	@Test
	public void testComparison() {

		// the testSubjects
		CompositeIndexKey key1, key2;

		// check equality, equality will find
		key1 = new CompositeIndexKey("A", "B", "D");
		key2 = new CompositeIndexKey("A", "B", "D");
		assertTrue("Got: " + key1.compareTo(key2), key1.compareTo(key2) == 0);
		assertTrue("Got: " + key2.compareTo(key1), key2.compareTo(key1) == 0);

		// check with null
		key1 = new CompositeIndexKey("A", null, "D");
		key2 = new CompositeIndexKey("A", "B", "D");
		assertTrue("Got: " + key1.compareTo(key2), key1.compareTo(key2) < 0);
		assertTrue("Got: " + key2.compareTo(key1), key2.compareTo(key1) > 0);

		key1 = new CompositeIndexKey(1, 5l, "D", null);
		key2 = new CompositeIndexKey(1, 5l, "D", null);
		assertTrue("Got: " + key1.compareTo(key2), key1.compareTo(key2) == 0);
		assertTrue("Got: " + key2.compareTo(key1), key2.compareTo(key1) == 0);

		// simply compare some values changed, compare will decide
		key1 = new CompositeIndexKey("A", "B", "C");
		key2 = new CompositeIndexKey("A", "B", "D");
		assertTrue("Got: " + key1.compareTo(key2), key1.compareTo(key2) < 0);
		assertTrue("Got: " + key2.compareTo(key1), key2.compareTo(key1) > 0);

		// test with different classes but same type, class will decide here
		key1 = new CompositeIndexKey("A", new Integer(5), "D");
		key2 = new CompositeIndexKey("A", "5", "D");
		assertTrue("Got: " + key1.compareTo(key2), key1.compareTo(key2) < 0);
		assertTrue("Got: " + key2.compareTo(key1), key2.compareTo(key1) > 0);

		key1 = new CompositeIndexKey("A", new Integer(5000));
		key2 = new CompositeIndexKey("A", new Long(5000));
		assertTrue("Got: " + key1.compareTo(key2), key1.compareTo(key2) < 0);
		assertTrue("Got: " + key2.compareTo(key1), key2.compareTo(key1) > 0);

		key1 = new CompositeIndexKey("A", new Integer(5000));
		key2 = new CompositeIndexKey("A", new Double(5000));
		assertTrue("Got: " + key1.compareTo(key2), key1.compareTo(key2) > 0);
		assertTrue("Got: " + key2.compareTo(key1), key2.compareTo(key1) < 0);
	}
}
