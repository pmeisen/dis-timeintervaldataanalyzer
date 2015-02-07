package net.meisen.dissertation.impl.datasets;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSetEntry;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSetIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;

import org.junit.Test;

/**
 * Tests the implementation of a {@code SingleStaticDataSet}.
 * 
 * @author pmeisen
 * 
 */
public class TestSingleStaticDataSet {

	/**
	 * Tests an empty {@code SingleStaticDataSet}.
	 */
	@Test
	public void testEmptyCreation() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet();

		assertFalse(dataSet.isValidPosition(0));
		assertFalse(dataSet.isValidPosition(1));
		assertFalse(dataSet.isValidPosition(2));
	}

	/**
	 * Tests the creation of a {@code SingleStaticDataSet}, which has
	 * {@code null} values implicitly defined at positions.
	 */
	@Test
	public void testCreationWithNullValues() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet(
				new SingleStaticDataSetEntry(2, "A VALUE"));

		assertFalse(dataSet.isValidPosition(0));
		assertTrue(dataSet.isValidPosition(1));
		assertTrue(dataSet.isValidPosition(2));
		assertFalse(dataSet.isValidPosition(3));

		assertNull(dataSet.getValue(1));
		assertEquals("A VALUE", dataSet.getValue(2));
	}

	/**
	 * Tests the creation of a {@code SingleStaticDataSet}, which has explicitly
	 * {@code null} values defined.
	 */
	@Test
	public void testCreationNullPlaceHolder() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet(null,
				new SingleStaticDataSetEntry(1, 1), null, null,
				new SingleStaticDataSetEntry(2, 2));

		assertFalse(dataSet.isValidPosition(0));
		assertTrue(dataSet.isValidPosition(1));
		assertTrue(dataSet.isValidPosition(2));
		assertTrue(dataSet.isValidPosition(3));
		assertTrue(dataSet.isValidPosition(4));
		assertTrue(dataSet.isValidPosition(5));
		assertFalse(dataSet.isValidPosition(6));
		
		assertEquals(1, dataSet.getValue(1));
		assertEquals(2, dataSet.getValue(2));
		assertNull(dataSet.getValue(3));
		assertNull(dataSet.getValue(4));
		assertNull(dataSet.getValue(5));
	}

	/**
	 * Tests the creation of a {@code SingleStaticDataSet}, which has undefined
	 * positions and position overwriting.
	 */
	@Test
	public void testCreationWithUndefinedPositions() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet(null,
				new SingleStaticDataSetEntry(1, "myName", 1), null,
				new SingleStaticDataSetEntry(4), new SingleStaticDataSetEntry(
						3, "anotherName", 3));

		assertFalse(dataSet.isValidPosition(0));
		assertTrue(dataSet.isValidPosition(1));
		assertTrue(dataSet.isValidPosition(2));
		assertTrue(dataSet.isValidPosition(3));
		assertTrue(dataSet.isValidPosition(4));
		assertTrue(dataSet.isValidPosition(5));
		assertFalse(dataSet.isValidPosition(6));
		
		assertEquals(1, dataSet.getValue(1));
		assertNull(dataSet.getValue(2));
		assertEquals(3, dataSet.getValue(3));
		assertEquals(4, dataSet.getValue(4));
		assertNull(dataSet.getValue(5));
	}

	/**
	 * Tests the creation of a {@code SingleStaticDataSet} using just the values
	 * (i.e. objects).
	 */
	@Test
	public void testCreationWithObjects() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet("value", 1,
				125l, null, 500.0);

		assertFalse(dataSet.isValidPosition(0));
		assertTrue(dataSet.isValidPosition(1));
		assertTrue(dataSet.isValidPosition(2));
		assertTrue(dataSet.isValidPosition(3));
		assertTrue(dataSet.isValidPosition(4));
		assertTrue(dataSet.isValidPosition(5));
		assertFalse(dataSet.isValidPosition(6));
		
		assertEquals("value", dataSet.getValue(1));
		assertEquals(1, dataSet.getValue(2));
		assertEquals(125l, dataSet.getValue(3));
		assertEquals(null, dataSet.getValue(4));
		assertEquals(500.0, dataSet.getValue(5));
	}

	/**
	 * Tests the implementation of the
	 * {@link SingleStaticDataSet#hasNamedValue(String)} method.
	 */
	@Test
	public void testHasNamedValue() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet(null,
				new SingleStaticDataSetEntry(1, "myName", 1), null,
				new SingleStaticDataSetEntry(4), new SingleStaticDataSetEntry(
						3, "anotherName", 3));

		assertTrue(dataSet.hasNamedValue("anotherName"));
		assertTrue(dataSet.hasNamedValue("myName"));
		assertFalse(dataSet.hasNamedValue(""));
		assertFalse(dataSet.hasNamedValue("noName"));
	}

	/**
	 * Tests the implementation of the {@link SingleStaticDataSet#iterator()}
	 * method.
	 */
	@Test
	public void testIteration() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet(null,
				new SingleStaticDataSetEntry(1, "myName", 1), null,
				new SingleStaticDataSetEntry(4), new SingleStaticDataSetEntry(
						3, "anotherName", 3));

		// get the iterator
		final SingleStaticDataSetIterator it = dataSet.iterator();

		// there should be one value
		for (int i = 0; i < 10; i++) {
			assertTrue(it.hasNext());
		}

		final IDataRecord record = it.next();
		assertTrue(record.hasNamedValue("myName"));
		assertTrue(record.hasNamedValue("anotherName"));
		assertFalse(record.hasNamedValue(null));
		assertFalse(record.hasNamedValue(""));
		assertEquals(1, record.getValue("myName"));
		assertEquals(3, record.getValue("anotherName"));
		assertEquals(1, record.getValue(1));
		assertEquals(null, record.getValue(2));
		assertEquals(3, record.getValue(3));
		assertEquals(4, record.getValue(4));
		assertEquals(null, record.getValue(5));

		// no more
		for (int i = 0; i < 10; i++) {
			assertFalse(it.hasNext());
		}

		// check the exception
		try {
			it.next();
			fail("Exception not thrown");
		} catch (final Exception e) {
			assertNotNull(e);
		}
	}
}
