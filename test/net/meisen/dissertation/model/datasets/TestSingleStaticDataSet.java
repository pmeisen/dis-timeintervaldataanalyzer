package net.meisen.dissertation.model.datasets;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

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

		assertEquals(0, dataSet.getSize());
	}

	@Test
	public void testCreationWithNullValues() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet(
				new SingleStaticDataSetEntry(2, "A VALUE"));

		assertEquals(2, dataSet.getSize());
		assertNull(dataSet.getValue(1));
		assertEquals("A VALUE", dataSet.getValue(2));
	}

	@Test
	public void testCreationNullPlaceHolder() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet(null,
				new SingleStaticDataSetEntry(1, 1), null, null,
				new SingleStaticDataSetEntry(2, 2));

		assertEquals(5, dataSet.getSize());
		assertEquals(1, dataSet.getValue(1));
		assertEquals(2, dataSet.getValue(2));
		assertNull(dataSet.getValue(3));
		assertNull(dataSet.getValue(4));
		assertNull(dataSet.getValue(5));
	}

	@Test
	public void testCreationWithUndefinedPositions() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet(null,
				new SingleStaticDataSetEntry(1, "myName", 1), null,
				new SingleStaticDataSetEntry(4), new SingleStaticDataSetEntry(
						3, "anotherName", 3));

		assertEquals(5, dataSet.getSize());
		assertEquals(1, dataSet.getValue(1));
		assertNull(dataSet.getValue(2));
		assertEquals(3, dataSet.getValue(3));
		assertEquals(4, dataSet.getValue(4));
		assertNull(dataSet.getValue(5));
	}

	@Test
	public void testCreationWithObjects() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet("value", 1,
				125l, null, 500.0);

		assertEquals(5, dataSet.getSize());
		assertEquals("value", dataSet.getValue(1));
		assertEquals(1, dataSet.getValue(2));
		assertEquals(125l, dataSet.getValue(3));
		assertEquals(null, dataSet.getValue(4));
		assertEquals(500.0, dataSet.getValue(5));
	}

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

	@Test
	public void testIteration() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet(null,
				new SingleStaticDataSetEntry(1, "myName", 1), null,
				new SingleStaticDataSetEntry(4), new SingleStaticDataSetEntry(
						3, "anotherName", 3));

		// get the iterator
		final SingleStaticDataSetIterator it = dataSet.iterate();

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
