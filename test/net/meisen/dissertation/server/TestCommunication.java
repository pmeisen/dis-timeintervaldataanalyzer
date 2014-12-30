package net.meisen.dissertation.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.UUID;

import net.meisen.dissertation.help.ThreadForTesting;
import net.meisen.dissertation.jdbc.TidaConnection;
import net.meisen.dissertation.jdbc.TidaStatement;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

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

			// select a timeseries
			res = stmt
					.executeQuery("SELECT TIMESERIES OF COUNT(TASK) AS \"COUNT\" FROM testCommunicationModel IN [1, 2] GROUP BY WORKAREA");
			assertTrue(res.next());
			assertEquals("COUNT", res.getString(1));
			assertEquals(0.0, res.getDouble(2), 0.0);
			assertFalse(res.next());

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
			assertEquals(Double.NaN, res.getDouble(2), 0.0);
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
					.executeQuery("SELECT TIMESERIES OF SUM(MANPOWER) AS \"SUM\", COUNT(MANPOWER) AS \"COUNT\" FROM testCommunicationModel IN [4, 5] GROUP BY TASK, DEPARTMENT IGNORE {('MAINTENANCE','C*'), ('ROOMSERVICE','IT')}");
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
			assertEquals(Double.NaN, res.getDouble(2), 0.0);
			assertEquals(2.0, res.getDouble(3), 0.0);
			assertTrue(res.next());
			assertEquals("MAINTENANCE, IT (SUM)", res.getString(1));
			assertEquals(Double.NaN, res.getDouble(2), 0.0);
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

		@Test
		public void testMultiThreadedUsage() throws SQLException {
			runMultiThreadUsage("testCommunicationModel", 100, 100);
//			runMultiThreadUsage("testCommunicationModelWithFileCache", 10, 100);
//			runMultiThreadUsage("testCommunicationModelWithMapDbCache", 10, 100);
		}

		protected void runMultiThreadUsage(final String model,
				final int stmtInsertCount, final int threadCountPerJob)
				throws SQLException {

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

			// create some threads, each with an own connection
			final List<ThreadForTesting> threads = new ArrayList<ThreadForTesting>();
			for (int i = 0; i < threadCountPerJob; i++) {
				threads.add(new ThreadForTesting("insert_" + i) {
					private final Random rnd = new Random();
					private final TidaConnection conn = (TidaConnection) DriverManager
							.getConnection(getJdbc());

					public void addValue(final int coll, final int nr) {
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
							final int task = rnd.nextInt(10);

							// task 0: reconnect
							if (task == 0) {
								threadStmt.close();
								threadStmt = conn.createStatement();
							}

							// task 1 & 2: create a new value for a descriptor
							if (task == 1 || task == 2 || task == 3
									|| task == 4) {
								final int coll = rnd.nextInt(4);
								addValue(coll, i);
							}

							// create the query
							final int v1 = rnd.nextInt(102);
							final int v2 = rnd.nextInt(102);

							final String tSql = "INSERT INTO \""
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

							// task 5: invalid value, i.e. delayed exception
							if (task == 5) {
								try {
									threadStmt.executeUpdate(tSql.replaceFirst(
											"'\\d+'\\)", "'NOINT')"));
								} catch (final Exception e) {
									assertEquals(SQLException.class,
											e.getClass());
									assertTrue(e.getMessage().contains(
											"Unable to create a descriptor"));
								}
							} else {
								threadStmt.execute(tSql,
										Statement.RETURN_GENERATED_KEYS);
								final ResultSet idRes = threadStmt.getGeneratedKeys();
								if (idRes.next()) {
									System.out.println(idRes.getInt(1));
								} else {
									System.out.println("NO");
								}
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
					private final static int stmtCount = 500;
					private final Random rnd = new Random();
					private final TidaConnection conn = (TidaConnection) DriverManager
							.getConnection(getJdbc());

					@Override
					public void _run() throws Throwable {
						final TidaStatement threadStmt = conn.createStatement();

						threadStmt.close();
					}

					@Override
					protected void cleanUp() throws Throwable {
						conn.close();
					}
				});
				threads.add(new ThreadForTesting("select_" + i) {
					private final static int stmtCount = 100;
					private final Random rnd = new Random();
					private final TidaConnection conn = (TidaConnection) DriverManager
							.getConnection(getJdbc());

					@Override
					public void _run() throws Throwable {
						final TidaStatement threadStmt = conn.createStatement();

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
					t.validate();
				} catch (final InterruptedException e) {
					fail(t.getName() + " (" + e.getMessage() + ")");
				}
			}

			// close the connection
			stmt.close();

			System.out.println(waDescriptor.size());
			System.out.println(taskDescriptor.size());
			System.out.println(depDescriptor.size());
			System.out.println(mpDescriptor.size());
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
			ResultSet res;

			stmt = conn.createStatement();

			// load the communicationModel
			stmt.executeUpdate("LOAD FROM 'classpath://net/meisen/dissertation/server/testLoadAndUnload.xml'");

			// @formatter:off
			res = stmt.executeQuery("INSERT INTO testLoadAndUnload ([START], [END], PERSON, TASKTYPE, WORKAREA) VALUES (01.01.2008 01:01:00, 01.01.2008 01:01:00, 'Philipp', 'Dev', 'Home')");

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

			res.close();
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
				res = stmt
						.executeQuery("INSERT INTO testLoadAndUnload ([START], [END], PERSON, TASKTYPE, WORKAREA) VALUES (01.01.2008 01:01:00, 01.01.2008 01:01:00, 'Philipp', 'Dev', 'Home')");
				res.close();
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
				res = stmt.executeQuery("INSERT INTO testLoadAndUnload ([START], [END], PERSON, TASKTYPE, WORKAREA) VALUES (01.01.2008 01:01:00, 01.01.2008 01:01:00, 'Philipp', 'Dev', 'Home')");
				res.close();
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
	@Suite.SuiteClasses({ TestSimple.class, TestWithAuthentication.class,
			TestLoadAndUnload.class })
	public static class TestCommunicationSuite {
		// all tests are defined within the suite's annotation
	}
}
