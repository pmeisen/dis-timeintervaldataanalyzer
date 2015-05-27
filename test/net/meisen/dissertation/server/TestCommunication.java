package net.meisen.dissertation.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Random;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

import net.meisen.dissertation.help.DbBasedTest;
import net.meisen.dissertation.help.ThreadForTesting;
import net.meisen.dissertation.jdbc.DriverProperties;
import net.meisen.dissertation.jdbc.TidaConnection;
import net.meisen.dissertation.jdbc.TidaResultSet;
import net.meisen.dissertation.jdbc.TidaStatement;
import net.meisen.dissertation.model.cache.IDataRecordCache;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

import com.eclipsesource.json.JsonArray;
import com.eclipsesource.json.JsonObject;
import com.eclipsesource.json.JsonValue;

/**
 * Test to test some communication between a client and a server via TSQL.
 * 
 * @author pmeisen
 * 
 */
public class TestCommunication {

	/**
	 * A simple test loading models and interacting with those.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class TestSimple extends BaseTestWithServerConnection {

		@Override
		public boolean isTSQL() {
			return true;
		}

		/**
		 * Tests the loading of a model, insertion of data and the retrieval of
		 * time-series.
		 * 
		 * @throws SQLException
		 *             if a problem on client side occurs
		 */
		@Test
		public void testDataInsertionAndTimeSeriesRetrieval()
				throws SQLException {
			Statement stmt;
			ResultSet res;
			stmt = conn.createStatement();

			// load the communicationModel
			stmt.executeUpdate("LOAD FROM 'classpath://net/meisen/dissertation/server/testCommunicationModel.xml'");

			// add some data
			assertEquals(
					1,
					stmt.executeUpdate("INSERT INTO testCommunicationModel ([START], [END], TASK, DEPARTMENT, WORKAREA, MANPOWER) VALUES (1, 10, 'ROOMSERVICE', 'CLEANING', 'FLOOR30', '5')"));
			assertEquals(
					1,
					stmt.executeUpdate("INSERT INTO testCommunicationModel ([START], [END], TASK, DEPARTMENT, WORKAREA, MANPOWER) VALUES (5, 15, 'MAINTENANCE', 'IT', 'FLOOR30', '1')"));
			assertEquals(
					1,
					stmt.executeUpdate("INSERT INTO testCommunicationModel ([START], [END], TASK, DEPARTMENT, WORKAREA, MANPOWER) VALUES (5, 15, 'ROOMSERVICE', 'CATERING', 'FLOOR30', '2')"));

			// select some timeseries
			res = stmt
					.executeQuery("SELECT TIMESERIES OF COUNT(TASK) AS \"COUNT\", MEAN(MANPOWER) AS \"MEAN\" FROM testCommunicationModel IN [4, 5] GROUP BY TASK");
			assertTrue(res.next());
			assertEquals("ROOMSERVICE (COUNT)", res.getString(1));
			assertEquals(1.0, res.getDouble(2), 0.0);
			assertEquals(2.0, res.getDouble(3), 0.0);
			assertTrue(res.next());
			assertEquals("MAINTENANCE (COUNT)", res.getString(1));
			assertEquals(0.0, res.getDouble(2), 0.0);
			assertEquals(1.0, res.getDouble(3), 0.0);
			assertTrue(res.next());
			assertEquals("MAINTENANCE (MEAN)", res.getString(1));
			assertEquals(0.0, res.getDouble(2), 0.0);
			assertEquals(1.0, res.getDouble(3), 0.0);
			assertTrue(res.next());
			assertEquals("ROOMSERVICE (MEAN)", res.getString(1));
			assertEquals(5.0, res.getDouble(2), 0.0);
			assertEquals(3.5, res.getDouble(3), 0.0);
			assertFalse(res.next());
			res.close();

			res = stmt
					.executeQuery("SELECT TIMESERIES OF SUM(MANPOWER) AS \"SUMMP\" FROM testCommunicationModel IN [4, 5] GROUP BY WORKAREA");
			assertTrue(res.next());
			assertEquals("FLOOR30 (SUMMP)", res.getString(1));
			assertEquals(5.0, res.getDouble(2), 0.0);
			assertEquals(8.0, res.getDouble(3), 0.0);
			assertFalse(res.next());
			res.close();

			res = stmt
					.executeQuery("SELECT TIMESERIES OF SUM(MANPOWER) AS \"SUM\", COUNT(MANPOWER) AS \"COUNT\" FROM testCommunicationModel IN [4, 5] GROUP BY TASK, DEPARTMENT exclude {('MAINTENANCE','C*'), ('ROOMSERVICE','IT')}");
			assertTrue(res.next());
			assertEquals("ROOMSERVICE, CLEANING (COUNT)", res.getString(1));
			assertEquals(1.0, res.getDouble(2), 0.0);
			assertEquals(1.0, res.getDouble(3), 0.0);
			assertTrue(res.next());
			assertEquals("MAINTENANCE, IT (COUNT)", res.getString(1));
			assertEquals(0.0, res.getDouble(2), 0.0);
			assertEquals(1.0, res.getDouble(3), 0.0);
			assertTrue(res.next());
			assertEquals("ROOMSERVICE, CATERING (SUM)", res.getString(1));
			assertEquals(0.0, res.getDouble(2), 0.0);
			assertEquals(2.0, res.getDouble(3), 0.0);
			assertTrue(res.next());
			assertEquals("MAINTENANCE, IT (SUM)", res.getString(1));
			assertEquals(0.0, res.getDouble(2), 0.0);
			assertEquals(1.0, res.getDouble(3), 0.0);
			assertTrue(res.next());
			assertEquals("ROOMSERVICE, CLEANING (SUM)", res.getString(1));
			assertEquals(5.0, res.getDouble(2), 0.0);
			assertEquals(5.0, res.getDouble(3), 0.0);
			assertTrue(res.next());
			assertEquals("ROOMSERVICE, CATERING (COUNT)", res.getString(1));
			assertEquals(0.0, res.getDouble(2), 0.0);
			assertEquals(1.0, res.getDouble(3), 0.0);
			assertFalse(res.next());
			res.close();

			stmt.close();
		}

		/**
		 * Tests the adding of an advanced descriptor.
		 * 
		 * @throws SQLException
		 *             if the query cannot be fired
		 */
		@Test
		public void testAdvancedDescriptors() throws SQLException {
			Statement stmt;

			stmt = conn.createStatement();

			stmt.executeUpdate("LOAD FROM 'classpath://net/meisen/dissertation/server/testAdvancedDescriptors'");
			for (int i = 0; i < 1000; i++) {
				stmt.executeUpdate("INSERT INTO testAdvancedDescriptors ([START], [END], LONG, INT, STRING, LIST) VALUES (01.01.2015 08:00:00, 01.01.2015 08:07:00, '5', '7', 'DIFF', 'Kuchen, Kaffee')");
			}
			stmt.close();
		}

		/**
		 * Test used for multi-threading. The tests fires several queries over
		 * several threads for different models using
		 * {@link #runMultiThreadUsage(String, int, int, int, int)}.
		 * 
		 * @throws SQLException
		 *             if the test fails
		 */
		@Test
		public void testMultiThreadedUsage() throws SQLException {
			runMultiThreadUsage("testCommunicationModel", 100, 100, 100, 100);
			runMultiThreadUsage("testCommunicationModelWithFileCache", 100, 50,
					50, 10);
			runMultiThreadUsage("testCommunicationModelWithMapDbCache", 70, 50,
					30, 10);
		}

		/**
		 * The multi-threading test, performed for the specified {@code model}.
		 * 
		 * @param model
		 *            the identifier of the model, which must be equal - in this
		 *            specific case - with the filename
		 * @param stmtInsertCount
		 *            the amount of insert statements per thread
		 * @param stmtDeleteCount
		 *            the amount of delete statements per thread
		 * @param stmtSelectCount
		 *            the amount of select statements per thread
		 * @param threadCountPerJob
		 *            the amount of threads to run per statement-type
		 * @throws SQLException
		 */
		protected void runMultiThreadUsage(final String model,
				final int stmtInsertCount, final int stmtDeleteCount,
				final int stmtSelectCount, final int threadCountPerJob)
				throws SQLException {

			// set the properties used within the connections of the threads
			final Properties properties = new Properties();
			properties.setProperty(DriverProperties.PROPERTY_TIMEOUT, "0");
			properties.setProperty(DriverProperties.PROPERTY_DISABLELINGER,
					"true");

			// load the communicationModel
			final Statement stmt = conn.createStatement();
			stmt.executeUpdate("LOAD FROM 'classpath://net/meisen/dissertation/server/"
					+ model + ".xml'");

			// generate a list to keep track of values added
			final List<String> taskDescriptor = Collections
					.synchronizedList(new ArrayList<String>());
			final List<String> depDescriptor = Collections
					.synchronizedList(new ArrayList<String>());
			final List<String> waDescriptor = Collections
					.synchronizedList(new ArrayList<String>());
			final List<Integer> mpDescriptor = Collections
					.synchronizedList(new ArrayList<Integer>());
			final List<Integer> addedIds = Collections
					.synchronizedList(new ArrayList<Integer>());
			final Set<Integer> deletedIds = Collections
					.synchronizedSet(new HashSet<Integer>());

			// create some threads, each with an own connection
			final List<ThreadForTesting> threads = new ArrayList<ThreadForTesting>();
			for (int i = 0; i < threadCountPerJob; i++) {
				threads.add(new ThreadForTesting("insert_" + i) {
					private final Random rnd = new Random();
					private final TidaConnection conn = (TidaConnection) DriverManager
							.getConnection(getJdbc(), properties);

					public String addValue(final int coll, final int nr) {
						final String name = UUID.randomUUID().toString();

						switch (coll) {
						case 0:
							taskDescriptor.add(name);
							break;
						case 1:
							depDescriptor.add(name);
							break;
						case 2:
							waDescriptor.add(name);
							break;
						case 3:
							synchronized (mpDescriptor) {
								if (!mpDescriptor.contains(nr)) {
									mpDescriptor.add(nr);
								}
							}
							break;
						}

						return name;
					}

					@Override
					public void _run() throws Throwable {
						TidaStatement threadStmt = conn.createStatement();
						addValue(0, -1);
						addValue(1, -1);
						addValue(2, -1);
						addValue(3, -1);

						for (int i = 0; i < stmtInsertCount; i++) {
							/*
							 * generate a random integer telling what should be
							 * added
							 */
							final int task = rnd.nextInt(20);

							// task 0: reconnect
							if (task == 0) {
								threadStmt.close();
								threadStmt = conn.createStatement();
							}

							// task 1-7: create a new value for a descriptor
							if (task > 0 && task < 8) {
								final int coll = rnd.nextInt(4);
								addValue(coll, i);
							}

							// create the query
							final int v1 = rnd.nextInt(102);
							final int v2 = rnd.nextInt(102);

							String tSql = "INSERT INTO \""
									+ model
									+ "\" ([START], [END], TASK, DEPARTMENT, WORKAREA, MANPOWER) VALUES ("
									+ Math.min(v1, v2)
									+ ", "
									+ Math.max(v1, v2)
									+ ", '"
									+ taskDescriptor.get(rnd
											.nextInt(taskDescriptor.size()))
									+ "', '"
									+ depDescriptor.get(rnd
											.nextInt(depDescriptor.size()))
									+ "', '"
									+ waDescriptor.get(rnd.nextInt(waDescriptor
											.size()))
									+ "', '"
									+ mpDescriptor.get(rnd.nextInt(mpDescriptor
											.size())) + "')";

							// task 8: invalid value, i.e. delayed exception
							if (task == 8) {
								tSql = tSql.replaceFirst("'\\-?\\d+'\\)",
										"'NOINT')");
								try {
									executeUpdate(threadStmt, tSql);
								} catch (final Exception e) {
									assertEquals(SQLException.class,
											e.getClass());
									assertTrue(e
											.getMessage()
											.contains(
													"string 'NOINT' does not represent any valid descriptor value"));
								}
							} else {
								execute(threadStmt, tSql,
										Statement.RETURN_GENERATED_KEYS);

								final ResultSet idRes = threadStmt
										.getGeneratedKeys();
								if (idRes.next()) {
									addedIds.add(idRes.getInt(1));
								}
								idRes.close();
							}
						}

						threadStmt.close();
					}

					@Override
					protected void cleanUp() throws Throwable {
						conn.close();
					}
				});
				threads.add(new ThreadForTesting("delete_" + i) {
					private final Random rnd = new Random();
					private final TidaConnection conn = (TidaConnection) DriverManager
							.getConnection(getJdbc(), properties);

					@Override
					public void _run() throws Throwable {
						final TidaStatement threadStmt = conn.createStatement();

						for (int i = 0; i < stmtDeleteCount; i++) {
							final Integer id;
							synchronized (addedIds) {
								final int size = addedIds.size();
								id = size > 0 ? addedIds.get(rnd.nextInt(size))
										: null;
							}

							// remove id from the database
							if (id == null) {
								Thread.sleep(500);
							} else {
								executeUpdate(threadStmt, "DELETE " + id
										+ " FROM \"" + model + "\"");
								deletedIds.add(id);
							}
						}

						threadStmt.close();
					}

					@Override
					protected void cleanUp() throws Throwable {
						conn.close();
					}
				});
				threads.add(new ThreadForTesting("select_" + i) {
					private final Random rnd = new Random();
					private final TidaConnection conn = (TidaConnection) DriverManager
							.getConnection(getJdbc(), properties);

					@Override
					public void _run() throws Throwable {
						final TidaStatement threadStmt = conn.createStatement();

						for (int i = 0; i < stmtSelectCount; i++) {

							final int v1 = rnd.nextInt(102);
							final int v2 = rnd.nextInt(102);

							final TidaResultSet res = executeQuery(threadStmt,
									"SELECT RECORDS FROM \"" + model
											+ "\" WITHIN [" + Math.min(v1, v2)
											+ ", " + Math.max(v1, v2) + "]");

							// every 2nd we read some results
							if (rnd.nextInt(100) < 10) {
								while (res.next()) {
									// get the identifier
									res.getInt(1);
								}
							}

							res.close();
						}
						threadStmt.close();
					}

					@Override
					protected void cleanUp() throws Throwable {
						conn.close();
					}
				});
			}

			// start the threads
			for (final ThreadForTesting t : threads) {
				t.start();
			}

			// wait for all threads
			for (final ThreadForTesting t : threads) {
				try {
					t.join();
				} catch (final InterruptedException e) {
					fail(t.getName() + " (" + e.getMessage() + ")");
				}
			}

			// validate each thread
			int foundExceptions = 0;
			for (final ThreadForTesting t : threads) {
				try {
					t.validate();
				} catch (final Throwable e) {
					System.out.println("Exception found in '" + t.getName()
							+ "' with '" + e.getMessage() + "'.");

					if (!e.getMessage().contains(
							"Unable to establish a connection")) {
						e.printStackTrace();
					}
					foundExceptions++;
				}
			}

			// check if we found any exceptions
			if (foundExceptions > 0) {
				fail("Found " + foundExceptions + " exceptions.");
			}

			// get the model
			final TidaModel m = server.getModel(model);

			// check the descriptors
			final MetaDataModel mMeta = m.getMetaDataModel();
			assertDescriptorModel(mMeta.getDescriptorModel("TASK"),
					taskDescriptor);
			assertDescriptorModel(mMeta.getDescriptorModel("DEPARTMENT"),
					depDescriptor);
			assertDescriptorModel(mMeta.getDescriptorModel("WORKAREA"),
					waDescriptor);
			assertDescriptorModel(mMeta.getDescriptorModel("MANPOWER"),
					mpDescriptor);

			// check the added and deleted ids
			assertRecordIds(m, addedIds, deletedIds);

			// close the connection
			try {
				stmt.execute("DROP MODEL testCommunicationModel");
			} catch (final Exception e) {
				// nothing that isn't of importance
			}
			stmt.close();
		}

		/**
		 * Validates the addedIds and deletedIds according to the data in the
		 * model.
		 * 
		 * @param model
		 *            the model
		 * @param addedIds
		 *            the added identifiers as collected in the test
		 * @param deletedIds
		 *            the deleted identifiers as collected in the test
		 */
		protected void assertRecordIds(final TidaModel model,
				List<Integer> addedIds, Set<Integer> deletedIds) {
			final IDataRecordCache cache = model.getDataRecordCache();
			final Set<Integer> currentIds = new HashSet<Integer>();
			currentIds.addAll(addedIds);
			currentIds.removeAll(deletedIds);

			final Iterator<Integer> it = model.getValidRecords().iterator();
			final Set<Integer> modelIds = new HashSet<Integer>();
			while (it.hasNext()) {
				final int id = it.next();
				assertTrue(modelIds.add(id));
				assertNotNull(cache.get(id));
			}

			assertEquals(model.getAmountOfRecords(), modelIds.size());
			assertEquals(model.getAmountOfRecords(), currentIds.size());

			for (final Integer id : currentIds) {
				final Object[] rec = cache.get(id);
				assertNotNull(rec);
				assertEquals(id, rec[0]);
			}

		}

		/**
		 * Assert to validate the specified {@code DescriptorModel}.
		 * 
		 * @param model
		 *            the {@code DescriptorModel} to be validated
		 * @param descriptors
		 *            the created descriptors of the specified type
		 */
		protected void assertDescriptorModel(final DescriptorModel<?> model,
				final List<?> descriptors) {
			assertTrue(descriptors.size() >= model.getAllDescriptors().size());
			for (final Descriptor<?, ?, ?> desc : model.getAllDescriptors()) {
				assertTrue(desc.getValue().toString(),
						descriptors.contains(desc.getValue()));
			}
		}

		/**
		 * Method used to handle exceptions during test.
		 * 
		 * @param e
		 *            the exception thrown
		 * @param counter
		 *            the actual counter, i.e. how often the exception was
		 *            handled so far
		 * @return {@code true} if the exception should be ignored,
		 *         {@code false} if the amount of retries is exceeded
		 * 
		 * @throws SQLException
		 *             if the exception should have been handled
		 */
		protected static boolean handleException(final SQLException e,
				final AtomicInteger counter) throws SQLException {
			if (e.getMessage().contains("Unable to establish a connection")) {
				if (counter.incrementAndGet() > 5) {
					fail("Did not get a new connection after ten retries ("
							+ counter.get() + ".");
				}
				return false;
			} else {
				throw e;
			}
		}

		/**
		 * Executes the specified query.
		 * 
		 * @param stmt
		 *            the statement used for execution
		 * @param query
		 *            the query the query to be fired
		 * @param returnGeneratedKeys
		 *            defines if the flag for return the generated keys should
		 *            be set
		 * 
		 * @throws SQLException
		 *             if an unexpected problem occures
		 */
		protected static void execute(final TidaStatement stmt,
				final String query, final int returnGeneratedKeys)
				throws SQLException {
			final AtomicInteger counter = new AtomicInteger(0);
			boolean noException;
			do {
				noException = true;
				try {
					stmt.execute(query, returnGeneratedKeys);
				} catch (final SQLException e) {
					noException = handleException(e, counter);
				}
			} while (!noException);
		}

		/**
		 * Executes the specified update query.
		 * 
		 * @param stmt
		 *            the statement used for execution
		 * @param query
		 *            the query the query to be fired
		 * 
		 * @throws SQLException
		 *             if an unexpected problem occures
		 */
		protected static void executeUpdate(final TidaStatement stmt,
				final String query) throws SQLException {
			final AtomicInteger counter = new AtomicInteger(0);
			boolean noException;
			do {
				noException = true;
				try {
					stmt.executeUpdate(query);
				} catch (final SQLException e) {
					noException = handleException(e, counter);
				}
			} while (!noException);
		}

		/**
		 * Executes the specified select query.
		 * 
		 * @param stmt
		 *            the statement used for execution
		 * @param query
		 *            the query the query to be fired
		 * @return the created {@code TidaResultSet}
		 * 
		 * @throws SQLException
		 *             if an unexpected problem occures
		 */
		protected static TidaResultSet executeQuery(final TidaStatement stmt,
				final String query) throws SQLException {
			final AtomicInteger counter = new AtomicInteger(0);
			TidaResultSet res = null;

			boolean noException;
			do {
				noException = true;
				try {
					res = stmt.executeQuery(query);
				} catch (final SQLException e) {
					noException = handleException(e, counter);
				}
			} while (!noException);

			return res;
		}

		/**
		 * Tests the enabling and disabling of bulk-loading.
		 * 
		 * @throws SQLException
		 *             if an unexpected exception is thrown
		 * @throws InterruptedException
		 *             if an unexpected exception is thrown
		 */
		@Test
		public void testBulkLoad() throws SQLException, InterruptedException {
			final Statement stmt = conn.createStatement();
			stmt.executeUpdate("LOAD FROM 'classpath://net/meisen/dissertation/server/testCommunicationModel.xml'");

			// get the model and make sure there is no bulk-load
			final TidaModel m = server.getModel("testCommunicationModel");
			assertFalse(m.isBulkload());

			// set the bulk-load
			stmt.execute("MODIFY MODEL testCommunicationModel SET BULKLOAD=true");
			stmt.close();
			assertTrue(m.isBulkload());

			// try to load again, should fail
			assertSettingBulkLoadTwice(conn);
			assertTrue(m.isBulkload());

			final ThreadForTesting setBulkLoad = new ThreadForTesting(
					"setBulkLoad") {
				private final TidaConnection innerConn = (TidaConnection) DriverManager
						.getConnection(getJdbc());

				@Override
				public void _run() throws Throwable {
					assertSettingBulkLoadTwice(innerConn);
				}

				@Override
				protected void cleanUp() throws Throwable {
					innerConn.close();
				}
			};
			setBulkLoad.run();
			setBulkLoad.join();

			final ThreadForTesting unsetBulkLoad = new ThreadForTesting(
					"setBulkLoad") {
				private final TidaConnection innerConn = (TidaConnection) DriverManager
						.getConnection(getJdbc());

				@Override
				public void _run() throws Throwable {
					final TidaStatement innerStmt = innerConn.createStatement();
					innerStmt
							.executeUpdate("MODIFY MODEL testCommunicationModel SET BULKLOAD=false");
					innerStmt.close();

					assertFalse(m.isBulkload());
				}

				@Override
				protected void cleanUp() throws Throwable {
					innerConn.close();
				}
			};
			unsetBulkLoad.run();
			unsetBulkLoad.join();
		}

		/**
		 * Helper method to validate the exception to be thrown if a bulk-load
		 * is set twice.
		 * 
		 * @param conn
		 *            the connection used to fire the query on
		 * 
		 * @throws SQLException
		 *             if an unexpected exception is thrown
		 */
		protected void assertSettingBulkLoadTwice(final TidaConnection conn)
				throws SQLException {
			boolean exception;

			final TidaStatement stmt = conn.createStatement();

			// test setting twice
			try {
				exception = false;
				stmt.executeUpdate("MODIFY MODEL testCommunicationModel SET BULKLOAD=true");
			} catch (final SQLException e) {
				assertTrue(e.getMessage().contains("[TidaModelException]"));
				assertTrue(e.getMessage().contains(
						"Bulk-loading is already active"));

				exception = true;
			} finally {
				stmt.close();
			}
			assertTrue(exception);
		}
	}

	/**
	 * Tests dealing with communication involving a database.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class TestWithDatabase extends BaseTestWithServerConnection {
		private DbBasedTest dbTest = null;

		/**
		 * Setup the database.
		 * 
		 * @throws IOException
		 *             if the defined database file cannot be accessed.
		 */
		@Before
		public void setup() throws IOException {

			// load a database to be used
			dbTest = new DbBasedTest();
			dbTest.initLocale();
			dbTest.getDb("tidaPioneerData",
					"/net/meisen/dissertation/impl/hsqldbs/tidaPioneerData.zip");
		}

		@Override
		public int getHttpPort() {
			return 6668;
		}

		@Override
		public int getTsqlPort() {
			return 6669;
		};

		@Override
		public boolean isTSQL() {
			return true;
		}

		@Override
		public boolean isHttp() {
			return true;
		}

		/**
		 * Test the reloading of the descriptors of a model.
		 * 
		 * @throws IOException
		 *             if a file cannot be accessed
		 * @throws SQLException
		 *             if the query cannot be parsed or loaded
		 */
		@Test
		public void testDescriptorModelReload() throws IOException,
				SQLException {
			signIn();

			final TidaStatement stmt = this.conn.createStatement();
			stmt.executeUpdate("LOAD FROM 'classpath://net/meisen/dissertation/server/testCommunicationModel.xml'");
			stmt.executeUpdate("LOAD FROM 'classpath://net/meisen/dissertation/server/testPioneerModel.xml'");
			stmt.executeUpdate("UNLOAD testPioneerModel");
			stmt.executeUpdate("LOAD FROM 'classpath://net/meisen/dissertation/server/testPersistedPioneerModel.xml'");

			// load the data
			final Map<String, String> params = new HashMap<String, String>();
			params.put("object", "adddbrecords");
			params.put("model", "testPersistedPioneerModel");
			params.put(
					"connection",
					// @formatter:off
					new JsonObject()
						.add("driver", "org.hsqldb.jdbcDriver")
						.add("url", "jdbc:hsqldb:hsql://localhost:6666/tidaPioneerData")
						.add("username", "SA")
						.add("password", "")
						.toString());
					// @formatter:on
			params.put(
					"structure",
					// @formatter:off
					new JsonArray()
						.add(new JsonObject()
								.add("descriptor", "DESC_SYMBOL")
								.add("dbname", "SYMBOL"))
						.add(new JsonObject()
								.add("interval", "START")
								.add("dbname", "START_TIME"))
						.add(new JsonObject()
								.add("interval", "END")
								.add("dbname", "END_TIME"))
						.toString());
					// @formatter:on
			params.put("query",
					"SELECT start_time, end_time, symbol FROM TB_INTERVALS");

			// fire the system-query
			final byte[] response = this.getResponse("/query/system", params);
			final JsonValue result = JsonValue.readFrom(new String(response,
					"UTF8"));
			assertTrue(result.isBoolean());
			assertTrue(result.asBoolean());

			// check the model
			TidaModel model;
			model = server.getModel("testPersistedPioneerModel");

			final int loadedRecords = model.getAmountOfRecords();
			final int loadedDescs = model.getMetaDataModel().getDescriptors()
					.size();
			final int loadedDescModels = model.getMetaDataModel()
					.getDescriptorModels().size();

			// unload the model
			stmt.executeUpdate("UNLOAD testPersistedPioneerModel");
			assertNull(server.getModel("testPersistedPioneerModel"));

			// reload it and validate again
			stmt.executeUpdate("LOAD testPersistedPioneerModel");
			model = server.getModel("testPersistedPioneerModel");

			// make sure everything is loaded correctly
			assertEquals(loadedRecords, model.getAmountOfRecords());
			assertEquals(loadedDescs, model.getMetaDataModel().getDescriptors()
					.size());
			assertEquals(loadedDescModels, model.getMetaDataModel()
					.getDescriptorModels().size());

			stmt.close();
		}

		/**
		 * Cleans up after the test.
		 */
		@After
		public void cleanUp() {
			if (dbTest != null) {
				dbTest.cleanUpDb();
				dbTest.cleanUpLocale();
			}
		}
	}

	/**
	 * A test using authentication and communication.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class TestWithAuthentication extends
			BaseTestWithServerConnection {

		@Override
		public String getConfig() {
			return "net/meisen/dissertation/server/testShiroAuthConfig.xml";
		}

		@Override
		public boolean isTSQL() {
			return true;
		}

		/**
		 * Test the adding of users and roles.
		 * 
		 * @throws SQLException
		 *             if an unexpected error occurred
		 */
		@Test
		public void testAddUsersAndRoles() throws SQLException {
			ResultSet res;
			Statement stmt;

			// add some users and roles
			stmt = conn.createStatement();
			stmt.executeUpdate("ADD USER 'eddie'   WITH PASSWORD 'password'");
			stmt.executeUpdate("ADD USER 'philipp' WITH PASSWORD 'password' WITH ROLES 'connect', 'superuser' WITH PERMISSIONS 'MODEL.testNumberModel.modify'");
			stmt.executeUpdate("ADD USER 'tobias'  WITH PASSWORD 'password' WITH ROLES 'readOnlyNumberModel', 'connect'");

			stmt.executeUpdate("ADD ROLE 'readOnlyNumberModel' WITH PERMISSIONS 'MODEL.testNumberModel.query'");
			stmt.executeUpdate("ADD ROLE 'connect'             WITH PERMISSIONS 'GLOBAL.connectTSQL'");
			stmt.executeUpdate("ADD ROLE 'superuser'           WITH PERMISSIONS 'GLOBAL.load', 'GLOBAL.get', 'GLOBAL.queryAll', 'GLOBAL.modifyAll'");
			stmt.close();

			// check the users
			stmt = conn.createStatement();
			res = stmt.executeQuery("GET USERS");
			assertTrue(res.next());
			assertEquals("admin", res.getString(1));
			assertTrue(res.next());
			assertEquals("eddie", res.getString(1));
			assertTrue(res.next());
			assertEquals("philipp", res.getString(1));
			assertTrue(res.next());
			assertEquals("tobias", res.getString(1));

			res.close();
			stmt.close();
		}
	}

	/**
	 * Tests some functionality with load and unload over communication.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class TestLoadAndUnload extends BaseTestWithServerConnection {

		@Override
		public boolean isTSQL() {
			return true;
		}

		/**
		 * Checks if the loading of an unloaded model leads to an exception when
		 * the model was changed.
		 * 
		 * @throws SQLException
		 *             if some unexpected error occurs
		 */
		@Test
		public void testInvalidReload() throws SQLException {
			Statement stmt;

			stmt = conn.createStatement();

			// load the communicationModel
			stmt.executeUpdate("LOAD FROM 'classpath://net/meisen/dissertation/server/testLoadAndUnload.xml'");

			// @formatter:off
			stmt.executeUpdate("INSERT INTO testLoadAndUnload ([START], [END], PERSON, TASKTYPE, WORKAREA) VALUES (01.01.2008 01:01:00, 01.01.2008 01:01:00, 'Philipp', 'Dev', 'Home')");

			// unload and load again
			stmt.executeUpdate("UNLOAD testLoadAndUnload");
			
			// we expect the exception now
			boolean error = false;
			try {
				stmt.executeUpdate("LOAD FROM 'classpath://net/meisen/dissertation/server/testLoadAndUnload2.xml'");
			} catch (final SQLException e) {
				assertTrue(e.getMessage().contains("Unable to retrieve a result"));
				error = true;
			}
			assertTrue(error);
			// @formatter:on

			stmt.close();
		}

		/**
		 * Tests the reloading of an unloaded model.
		 * 
		 * @throws SQLException
		 *             if an unexpected exception occurs
		 */
		@Test
		public void testReloadViaXslt() throws SQLException {
			Statement stmt;
			ResultSet res;

			stmt = conn.createStatement();

			// unload and load again
			for (int i = 0; i < 5; i++) {
				stmt.executeUpdate("UNLOAD testLoadAndUnload");
				stmt.executeUpdate("LOAD FROM 'classpath://net/meisen/dissertation/server/testLoadAndUnload.xml'");

				// validate the values
				stmt.executeUpdate("INSERT INTO testLoadAndUnload ([START], [END], PERSON, TASKTYPE, WORKAREA) VALUES (01.01.2008 01:01:00, 01.01.2008 01:01:00, 'Philipp', 'Dev', 'Home')");
				res = stmt
						.executeQuery("select TRANSPOSE(timeSeries) OF COUNT(TASKTYPE) AS \"COUNT\" from testLoadAndUnload in [01.01.2008 01:00:00,01.01.2008 01:02:00]");
				while (res.next()) {
					if (res.getString(3).equals("01.01.2008 01:01:00,000")) {
						assertEquals(res.getDouble(2), i + 1, 0.0);
					} else {
						assertEquals(res.getDouble(2), 0.0, 0.0);
					}
				}
				res.close();
			}
			// @formatter:on

			stmt.close();
		}

		/**
		 * Tests the reloading of a model using the id of it.
		 * 
		 * @throws SQLException
		 *             if an unexpected exception occurs
		 */
		@Test
		public void testReloadViaId() throws SQLException {
			Statement stmt;
			ResultSet res;

			stmt = conn.createStatement();
			stmt.executeUpdate("LOAD FROM 'classpath://net/meisen/dissertation/server/testLoadAndUnload.xml'");

			// @formatter:off
			for (int i = 0; i < 5; i++) {
				
				// unload and load again via id
				stmt.executeUpdate("UNLOAD testLoadAndUnload");
				stmt.executeUpdate("LOAD testLoadAndUnload");

				// validate the values
				stmt.executeUpdate("INSERT INTO testLoadAndUnload ([START], [END], PERSON, TASKTYPE, WORKAREA) VALUES (01.01.2008 01:01:00, 01.01.2008 01:01:00, 'Philipp', 'Dev', 'Home')");
				res = stmt.executeQuery("select TRANSPOSE(timeSeries) OF COUNT(TASKTYPE) AS \"COUNT\" from testLoadAndUnload in [01.01.2008 01:00:00,01.01.2008 01:02:00]");
				while (res.next()) {
					if (res.getString(3).equals("01.01.2008 01:01:00,000")) {
						assertEquals(res.getDouble(2), i + 1, 0.0);
					} else {
						assertEquals(res.getDouble(2), 0.0, 0.0);
					}
				}
				res.close();
			}
			// @formatter:on

			stmt.close();
		}
	}

	/**
	 * Suite for different communication tests.
	 * 
	 * @author pmeisen
	 * 
	 */
	@RunWith(Suite.class)
	@Suite.SuiteClasses({ TestSimple.class, TestWithDatabase.class,
			TestWithAuthentication.class, TestLoadAndUnload.class })
	public static class TestCommunicationSuite {
		// all tests are defined within the suite's annotation
	}
}
