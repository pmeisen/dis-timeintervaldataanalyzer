package net.meisen.dissertation.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

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
		public void testSimpleReload() throws SQLException {
			Statement stmt;
			ResultSet res;

			stmt = conn.createStatement();

			// load the communicationModel
			stmt.executeUpdate("LOAD FROM 'classpath://net/meisen/dissertation/server/testLoadAndUnload.xml'");

			// @formatter:off
			res = stmt.executeQuery("INSERT INTO testLoadAndUnload ([START], [END], PERSON, TASKTYPE, WORKAREA) VALUES (01.01.2008 01:01:00, 01.01.2008 01:01:00, 'Philipp', 'Dev', 'Home')");
			
			// just for fun let's check the results
			res = stmt.executeQuery("select TRANSPOSE(timeSeries) OF COUNT(TASKTYPE) AS \"COUNT\" from testLoadAndUnload in [01.01.2008 01:00:00,01.01.2008 01:02:00]");
			while (res.next()) {
				if (res.getString(3).equals("01.01.2008 01:01:00,000")) {
					assertEquals(res.getDouble(2), 1.0, 0.0);
				} else {
					assertEquals(res.getDouble(2), 0.0, 0.0);
				}
			}
			
			// unload and load again
			stmt.executeUpdate("UNLOAD testLoadAndUnload");
			stmt.executeUpdate("LOAD FROM 'classpath://net/meisen/dissertation/server/testLoadAndUnload.xml'");
			
			// validate the values
			res = stmt.executeQuery("INSERT INTO testLoadAndUnload ([START], [END], PERSON, TASKTYPE, WORKAREA) VALUES (01.01.2008 01:01:00, 01.01.2008 01:01:00, 'Philipp', 'Dev', 'Home')");
			res = stmt.executeQuery("select TRANSPOSE(timeSeries) OF COUNT(TASKTYPE) AS \"COUNT\" from testLoadAndUnload in [01.01.2008 01:00:00,01.01.2008 01:02:00]");
			while (res.next()) {
				if (res.getString(3).equals("01.01.2008 01:01:00,000")) {
					assertEquals(res.getDouble(2), 2.0, 0.0);
				} else {
					assertEquals(res.getDouble(2), 0.0, 0.0);
				}
			}
			// @formatter:on

			res.close();
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
