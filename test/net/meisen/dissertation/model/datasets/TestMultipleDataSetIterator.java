package net.meisen.dissertation.model.datasets;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.UUID;

import net.meisen.dissertation.help.DbBasedTest;
import net.meisen.dissertation.impl.dataretriever.DbConnectionConfig;
import net.meisen.dissertation.impl.dataretriever.DbDataRetriever;
import net.meisen.dissertation.impl.dataretriever.DbQueryConfig;
import net.meisen.dissertation.impl.datasets.DataRetrieverDataSet;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSetEntry;
import net.meisen.dissertation.model.data.OfflineMode;

import org.hsqldb.jdbcDriver;
import org.junit.Test;

/**
 * Tests the iteration across multiple {@code DataSet} instances using a
 * {@code MultipleDataSetIterator}.
 * 
 * @author pmeisen
 * 
 */
public class TestMultipleDataSetIterator extends DbBasedTest {

	/**
	 * Tests the iteration over no {@code DataSet}.
	 */
	@Test
	public void testEmptyIterator() {
		final MultipleDataSetIterator iterator = new MultipleDataSetIterator(
				OfflineMode.FALSE);

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
				OfflineMode.TRUE, new SingleStaticDataSet());

		for (int i = 0; i < 100; i++) {
			assertTrue(iterator.hasNext());
		}

		iterator.next();

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
				OfflineMode.TRUE, new SingleStaticDataSet("myValue"));

		for (int i = 0; i < 100; i++) {
			assertTrue(iterator.hasNext());
		}

		final IDataRecord record = iterator.next();
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
	public void testMultipleStaticDataSetIterator() {
		final MultipleDataSetIterator iterator = new MultipleDataSetIterator(
				OfflineMode.TRUE, new SingleStaticDataSet("myValue",
						"anotherValue", 5),
				new SingleStaticDataSet(new SingleStaticDataSetEntry(1, "#1",
						"THE VALUE IS 1"), new SingleStaticDataSetEntry(2,
						"#2", "THE VALUE IS 2"), null));

		// first record
		for (int i = 0; i < 100; i++) {
			assertTrue(iterator.hasNext());
		}
		final IDataRecord record1 = iterator.next();
		assertEquals("myValue", record1.getValue(1));
		assertEquals("anotherValue", record1.getValue(2));
		assertEquals(5, record1.getValue(3));

		// second record
		for (int i = 0; i < 100; i++) {
			assertTrue(iterator.hasNext());
		}
		final IDataRecord record2 = iterator.next();
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

	/**
	 * Tests the iteration over several {@code DataSet} instanances.
	 * 
	 * @throws IOException
	 */
	@Test
	public void testMultipleMixedDataSetIterator() throws IOException {
		getDb("tidaTestData",
				"/net/meisen/dissertation/impl/hsqldbs/tidaTestData.zip");

		// setup the single static data
		final SingleStaticDataSet singleDataSet = new SingleStaticDataSet(
				"myValue", "anotherValue", 5);

		// setup the database connection
		final DbConnectionConfig config = new DbConnectionConfig();
		config.setDriver(jdbcDriver.class.getName());
		config.setUsername("SA");
		config.setPassword("");
		config.setType("jdbc");
		config.setUrl("jdbc:hsqldb:hsql://localhost:6666/tidaTestData");
		final DbDataRetriever retriever = new DbDataRetriever(UUID.randomUUID()
				.toString(), config);
		final DbQueryConfig query = new DbQueryConfig();
		query.setLanguage("sql");
		query.setQuery("SELECT FIXED, RANDOM, COUNTER FROM TB_TESTDATA");

		// setup the db data
		final DataRetrieverDataSet dbDataSet = new DataRetrieverDataSet(
				retriever, query);

		// check the different modes
		MultipleDataSetIterator it;

		// test the OfflineMode.FALSE
		it = new MultipleDataSetIterator(OfflineMode.FALSE, singleDataSet,
				dbDataSet);
		assertEquals(10001, it.count());
		it.close();

		// test the OfflineMode.AUTO
		it = new MultipleDataSetIterator(OfflineMode.AUTO, singleDataSet,
				dbDataSet);
		assertEquals(10001, it.count());
		it.close();

		// test the OfflineMode.TRUE
		it = new MultipleDataSetIterator(OfflineMode.TRUE, singleDataSet,
				dbDataSet);
		assertEquals(1, it.count());
		it.close();

		// release the retriever
		retriever.release();
	}
}
