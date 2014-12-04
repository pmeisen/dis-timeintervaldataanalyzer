package net.meisen.dissertation.impl.dataretriever;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.util.Collection;
import java.util.Locale;
import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.exceptions.DataRetrieverException;
import net.meisen.dissertation.help.Db;
import net.meisen.dissertation.impl.dataretriever.DbConnectionConfig;
import net.meisen.dissertation.impl.dataretriever.DbDataCollection;
import net.meisen.dissertation.impl.dataretriever.DbDataRetriever;
import net.meisen.dissertation.impl.dataretriever.DbDataRetrieverException;
import net.meisen.dissertation.impl.dataretriever.DbQueryConfig;
import net.meisen.dissertation.model.dataretriever.DataRecord;
import net.meisen.dissertation.model.dataretriever.IQueryConfiguration;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.hsqldb.jdbcDriver;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.matchers.JUnitMatchers;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of a {@code DbDataRetriever}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "net/meisen/dissertation/config/tida-bean-exceptions.xml")
public class TestDbDataRetriever {

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private IConfiguration c;

	private static final Db db = new Db();

	/**
	 * Helper class for the tests.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class Util {

		/**
		 * Creates a {@code DbConnectionConfig} for the specified {@code dbName}
		 * .
		 * 
		 * @param dbName
		 *            the name of the database to create the configuration for
		 * 
		 * @return the created {code DbConnectionConfig}
		 */
		public static DbConnectionConfig getConnectionConfig(final String dbName) {

			final DbConnectionConfig config = new DbConnectionConfig();
			config.setDriver(jdbcDriver.class.getName());
			config.setUsername("SA");
			config.setPassword("");
			config.setType("jdbc");
			config.setUrl("jdbc:hsqldb:hsql://localhost:6666/" + dbName);

			return config;
		}

		/**
		 * Creates a {@code DbDataRetriever} used to retrieve data from the
		 * specified database of name {@code dbName}.
		 * 
		 * @param configuration
		 *            the {@code DbConnectionConfig} to use for the
		 *            {@code DbDataRetriever}
		 * @param dbName
		 *            the name of the database to create the retriever for
		 * 
		 * @return the created instance
		 */
		public static DbDataRetriever create(
				final IConfiguration configuration, final String dbName) {
			// get the DatabaseRetriever to be tested
			final DbDataRetriever dbRetriever = new DbDataRetriever(UUID
					.randomUUID().toString(), Util.getConnectionConfig(dbName));
			configuration.wireInstance(dbRetriever);

			return dbRetriever;
		}
	}

	/**
	 * Rule to evaluate exceptions
	 */
	@Rule
	public ExpectedException thrown = ExpectedException.none();

	private Locale oldDefault;

	/**
	 * Initializes the database used for testing.
	 * 
	 * @throws IOException
	 *             if the database cannot be loaded
	 */
	@BeforeClass
	public static void init() throws IOException {
		// start the test database
		db.addDb("tidaTestData",
				"/net/meisen/dissertation/impl/hsqldbs/tidaTestData.zip");
		db.setUpDb();
	}

	/**
	 * Make sure we have {@code Locale.US} so that comparisons of errors will
	 * fit
	 */
	@Before
	public void setUp() {
		oldDefault = Locale.getDefault();
		Locale.setDefault(Locale.US);
	}

	/**
	 * Tests the definition using a {@code null} configuration.
	 */
	@Test
	public void testInvalidRetrieveWithNullQueryConfiguration() {
		final DbDataRetriever db = Util.create(c, "tidaTestData");

		thrown.expect(DbDataRetrieverException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("specify a queryConfiguration"));

		db.retrieve(null);
	}

	/**
	 * Tests the definition using an invalid configuration.
	 */
	@Test
	public void testInvalidRetrieveWithIncompatibleQueryConfiguration() {
		final DbDataRetriever db = Util.create(c, "tidaTestData");

		thrown.expect(DataRetrieverException.class);
		thrown.expectMessage(JUnitMatchers.containsString("class '"
				+ DbDataRetriever.class.getName()
				+ "' does not support a queryConfiguration of type '"
				+ TestDbDataRetriever.class.getName() + "$1'"));

		db.retrieve(new IQueryConfiguration() {
		});
	}

	/**
	 * Tests the data retrieval.
	 */
	@Test
	public void testRetrieve() {
		final DbDataRetriever db = Util.create(c, "tidaTestData");

		// get a collection of the data based on a query
		final DbQueryConfig queryConfiguration = new DbQueryConfig();
		queryConfiguration
				.setQuery("SELECT * FROM TB_TESTDATA WHERE COUNTER < 10");
		queryConfiguration.setLanguage("sql");
		final DbDataCollection res = db.retrieve(queryConfiguration);

		// check the retrieved column names
		assertEquals(3, res.getNames().size());
		assertTrue(res.getNames().contains("FIXED"));
		assertTrue(res.getNames().contains("RANDOM"));
		assertTrue(res.getNames().contains("COUNTER"));

		// check the data
		final Collection<DataRecord<String>> data = res.get();
		final int size = data.size();
		assertEquals(9, size);

		final Object[] ar = data.toArray();
		for (int i = 0; i < size; i++) {
			@SuppressWarnings("unchecked")
			final DataRecord<String> dr = (DataRecord<String>) ar[i];
			assertEquals(i + 1, dr.getData("COUNTER"));
			assertEquals("FIXED VALUE", dr.getData("FIXED"));
		}

		db.release();
	}

	/**
	 * Tests the {@code DbDataIterator} used by the retriever.
	 */
	@Test
	public void testIteration() {
		final DbDataRetriever db = Util.create(c, "tidaTestData");

		final DbQueryConfig queryConfiguration = new DbQueryConfig();
		queryConfiguration
				.setQuery("SELECT * FROM TB_TESTDATA WHERE COUNTER < 10");
		queryConfiguration.setLanguage("sql");
		final DbDataCollection res = db.retrieve(queryConfiguration);
		final DbDataIterator it = res.iterator();

		// check the iteration, it should not change the cursor
		int counter = 0;
		while (it.hasNext() && counter < 100) {
			counter++;
		}
		assertEquals(100, counter);

		// now iterate every second time
		DataRecord<String> record = null;
		for (counter = 1; counter < 19; counter++) {
			if (counter % 2 == 0) {
				DataRecord<String> oldRecord = record;
				record = it.next();

				final int orgCounter = (counter / 2);
				if (oldRecord != null) {
					assertEquals(orgCounter - 1, oldRecord.getData("COUNTER"));
				}
				assertEquals(orgCounter, record.getData("COUNTER"));
			} else {
				assertTrue(it.hasNext());
			}
		}
		assertFalse(it.hasNext());

		// next call to next has to fail
		try {
			it.next();
			fail("Exception not thrown");
		} catch (final Exception e) {
			assertNotNull(e);
		}

		db.release();
	}

	/**
	 * Tests the retrieval of data using an invalid query.
	 */
	@Test
	public void testRetrieveInvalidQuery() {
		final DbDataRetriever db = Util.create(c, "tidaTestData");

		// get a collection of the data based on a query
		final DbQueryConfig queryConfiguration = new DbQueryConfig();
		queryConfiguration.setQuery("SELECT * FROM TB_IDONTEXIST");
		queryConfiguration.setLanguage("sql");

		boolean error = false;
		try {
			db.retrieve(queryConfiguration);
		} catch (final DbDataRetrieverException e) {
			error = true;
			assertTrue(e instanceof DbDataRetrieverException);
			assertTrue(
					e.getMessage(),
					e.getMessage().contains(
							"Unable to create the preparedStatement of query"));
		} finally {
			db.release();
		}
		assertTrue("Expected exception not thrown", error);
	}

	/**
	 * Reset the {@code Locale}
	 */
	@After
	public void cleanUp() {
		Locale.setDefault(oldDefault);
	}

	/**
	 * CleanUp behind the class
	 */
	@AfterClass
	public static void tearDown() {
		// close the database again
		db.shutDownDb();
	}
}
