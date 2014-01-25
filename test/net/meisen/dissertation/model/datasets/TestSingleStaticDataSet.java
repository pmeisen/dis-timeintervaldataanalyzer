package net.meisen.dissertation.model.datasets;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;

public class TestSingleStaticDataSet {

	@Test
	public void testEmptyCreation() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet();

		assertEquals(0, dataSet.entrySize());
	}

	@Test
	public void testCreationWithNullValues() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet(
				new SingleStaticDataSetEntry(2, "A VALUE"));

		assertEquals(2, dataSet.entrySize());
		assertNull(dataSet.getValue(1));
		assertEquals("A VALUE", dataSet.getValue(2));
	}

	@Test
	public void testCreationNullPlaceHolder() {
		final SingleStaticDataSet dataSet = new SingleStaticDataSet(null,
				new SingleStaticDataSetEntry(1, 1), null, null,
				new SingleStaticDataSetEntry(2, 2));

		assertEquals(5, dataSet.entrySize());
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

		assertEquals(5, dataSet.entrySize());
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

		assertEquals(5, dataSet.entrySize());
		assertEquals("value", dataSet.getValue(1));
		assertEquals(1, dataSet.getValue(2));
		assertEquals(125l, dataSet.getValue(3));
		assertEquals(null, dataSet.getValue(4));
		assertEquals(500.0, dataSet.getValue(5));
	}
}
