package net.meisen.dissertation.data.impl.dataretriever;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.sql.SQLException;
import java.util.Locale;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.exceptions.DataRetrieverException;
import net.meisen.dissertation.help.Db;
import net.meisen.dissertation.models.impl.dataretriever.DataCollection;
import net.meisen.dissertation.models.impl.dataretriever.IQueryConfiguration;
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

@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "net/meisen/dissertation/config/tida-bean-exceptions.xml")
public class TestDbDataRetriever {

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private IConfiguration c;

	private static final Db db = new Db();

	public static class Util {

		public static DbConnectionConfig getConnectionConfig(final String dbName) {

			final DbConnectionConfig config = new DbConnectionConfig();
			config.setDriver(jdbcDriver.class.getName());
			config.setUsername("SA");
			config.setPassword("");
			config.setType("jdbc");
			config.setUrl("jdbc:hsqldb:hsql://localhost:6666/" + dbName);

			return config;
		}

		public static DbDataRetriever create(
				final IConfiguration configuration, final String dbName) {
			// get the DatabaseRetriever to be tested
			final DbDataRetriever dbRetriever = new DbDataRetriever(
					Util.getConnectionConfig(dbName));
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

	@BeforeClass
	public static void init() throws IOException {
		// start the test database
		db.addDb("tidaTestData",
				"/net/meisen/dissertation/data/impl/hsqldbs/tidaTestData.zip");
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

	@Test
	public void testInvalidRetrieveWithNullQueryConfiguration()
			throws SQLException {
		final DbDataRetriever db = Util.create(c, "tidaTestData");

		thrown.expect(DbDataRetrieverException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("specify a queryConfiguration"));

		db.retrieve(null);
	}

	@Test
	public void testInvalidRetrieveWithIncompatibleQueryConfiguration()
			throws SQLException {
		final DbDataRetriever db = Util.create(c, "tidaTestData");

		thrown.expect(DataRetrieverException.class);
		thrown.expectMessage(JUnitMatchers.containsString("class '"
				+ DbDataRetriever.class.getName()
				+ "' does not support a queryConfiguration of type '"
				+ TestDbDataRetriever.class.getName() + "$1'"));

		db.retrieve(new IQueryConfiguration() {
		});
	}

	@Test
	public void test() throws SQLException {
		final DbDataRetriever db = Util.create(c, "tidaTestData");

		final DbQueryConfiguration queryConfiguration = new DbQueryConfiguration();
		queryConfiguration.setSqlQuery("SELECT * FROM TB_TESTDATA WHERE COUNTER < 10");

		final DbDataCollection res = db.retrieve(queryConfiguration);
		
		// check the retrieved column names
		assertEquals(3, res.getNames().size());
		assertTrue(res.getNames().contains("FIXED"));
		assertTrue(res.getNames().contains("RANDOM"));
		assertTrue(res.getNames().contains("COUNTER"));

		db.close();
	}

	/**
	 * Reset the {@code Locale}
	 */
	@After
	public void cleanUp() {
		Locale.setDefault(oldDefault);
	}

	@AfterClass
	public static void tearDown() {
		// close the database again
		db.shutDownDb();
	}
}
