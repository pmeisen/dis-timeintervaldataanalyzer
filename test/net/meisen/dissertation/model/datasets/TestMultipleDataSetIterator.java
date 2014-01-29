package net.meisen.dissertation.model.datasets;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import org.junit.Test;

/**
 * Tests the iteration across multiple {@code DataSet} instances using a
 * {@code MultipleDataSetIterator}.
 * 
 * @author pmeisen
 * 
 */
public class TestMultipleDataSetIterator {

	/**
	 * Tests the iteration over no {@code DataSet}.
	 */
	@Test
	public void testEmptyIterator() {
		final MultipleDataSetIterator iterator = new MultipleDataSetIterator();

		for (int i = 0; i < 100; i++) {
			assertFalse(iterator.hasNext());
		}

		// make sure the call to next failes
		try {
			iterator.next();
			fail("Exception not thrown");
		} catch (final Exception e) {
			assertNotNull(e);
		}
	}

	/**
	 * Tests the iteration over an empty {@code DataSet}.
	 */
	@Test
	public void testEmptyDataSetIterator() {
		final MultipleDataSetIterator iterator = new MultipleDataSetIterator(
				new SingleStaticDataSet());

		for (int i = 0; i < 100; i++) {
			assertTrue(iterator.hasNext());
		}

		final IDataRecord record = iterator.next();
		assertEquals(0, record.getSize());

		// make sure the call to next failes
		try {
			iterator.next();
			fail("Exception not thrown");
		} catch (final Exception e) {
			assertNotNull(e);
		}
	}

	/**
	 * Test the iteration over a {@code DataSet} with a single
	 * {@code DataRecord}.
	 */
	@Test
	public void testSingleDataSetIterator() {
		final MultipleDataSetIterator iterator = new MultipleDataSetIterator(
				new SingleStaticDataSet("myValue"));

		for (int i = 0; i < 100; i++) {
			assertTrue(iterator.hasNext());
		}

		final IDataRecord record = iterator.next();
		assertEquals(1, record.getSize());
		assertEquals("myValue", record.getValue(1));

		// make sure the call to next failes
		try {
			iterator.next();
			fail("Exception not thrown");
		} catch (final Exception e) {
			assertNotNull(e);
		}
	}

	/**
	 * Tests the iteration over several {@code DataSet} instanances.
	 */
	@Test
	public void testMultipleDataSetIterator() {
		final MultipleDataSetIterator iterator = new MultipleDataSetIterator(
				new SingleStaticDataSet("myValue", "anotherValue", 5),
				new SingleStaticDataSet(new SingleStaticDataSetEntry(1, "#1",
						"THE VALUE IS 1"), new SingleStaticDataSetEntry(2,
						"#2", "THE VALUE IS 2"), null));

		// first record
		for (int i = 0; i < 100; i++) {
			assertTrue(iterator.hasNext());
		}
		final IDataRecord record1 = iterator.next();
		assertEquals(3, record1.getSize());
		assertEquals("myValue", record1.getValue(1));
		assertEquals("anotherValue", record1.getValue(2));
		assertEquals(5, record1.getValue(3));

		// second record
		for (int i = 0; i < 100; i++) {
			assertTrue(iterator.hasNext());
		}
		final IDataRecord record2 = iterator.next();
		assertEquals(3, record2.getSize());
		assertEquals("THE VALUE IS 1", record2.getValue(1));
		assertEquals("THE VALUE IS 1", record2.getValue("#1"));
		assertEquals("THE VALUE IS 2", record2.getValue(2));
		assertEquals("THE VALUE IS 2", record2.getValue("#2"));
		assertEquals(null, record2.getValue(3));

		// make sure the call to next fails
		try {
			iterator.next();
			fail("Exception not thrown");
		} catch (final Exception e) {
			assertNotNull(e);
		}
	}
}
