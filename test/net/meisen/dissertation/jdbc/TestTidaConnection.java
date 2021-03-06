package net.meisen.dissertation.jdbc;

import com.jolbox.bonecp.BoneCPDataSource;
import com.jolbox.bonecp.ConnectionHandle;
import org.junit.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

/**
 * Tests the {@code TidaConnection} implementation.
 * 
 * @author pmeisen
 * 
 */
public class TestTidaConnection extends TestBaseForConnections {

	/**
	 * Tests the implementation of {@code TidaConnection#isValid(int)}.
	 * 
	 * @throws SQLException
	 *             if a problem occurred
	 */
	@Test
	public void testConnectionValidityWithOpenClose() throws SQLException {
		Connection conn;

		// just create several connections and close those
		final Properties properties = new Properties();
		properties.setProperty(DriverProperties.PROPERTY_TIMEOUT, "1000");
		properties.setProperty(DriverProperties.PROPERTY_DISABLELINGER, "true");
		for (int i = 0; i < 1000; i++) {
			conn = DriverManager.getConnection(getJdbc(), properties);

			final TidaConnection tc = (TidaConnection) conn;
			assertEquals(1000, tc.getDriverProperties().getTimeout());

			// check the validity, must be true
			assertTrue("Failed in Run " + i, conn.isValid(500));
			assertTrue("Failed in Run " + i, conn.isValid(0));

			// check the type and special attributes
			assertEquals(TidaConnection.class, conn.getClass());
			assertEquals(1, tc.getManager().sizeOfProtocols(tc));
			assertEquals(1, tc.getManager().sizeOfScopes());
			assertEquals(1, tc.getManager().sizeOfOwners());

			// close the connection, the answer must be false
			conn.close();
			assertFalse("Failed in Run " + i, conn.isValid(0));
			assertTrue("Failed in Run " + i, conn.isClosed());
			assertTrue(((TidaConnection) conn).getManager().isClosed());
			assertEquals(0, tc.getManager().sizeOfProtocols(tc));
			assertEquals(0, tc.getManager().sizeOfScopes());
			assertEquals(0, tc.getManager().sizeOfOwners());
		}

		// create a new connection
		conn = DriverManager.getConnection(getJdbc());

		// shutdown the server and check the validity
		server.shutdown();
		assertFalse(conn.isValid(1000));
		assertFalse(conn.isValid(1));
		conn.close();
	}

	/**
	 * Tests the closing of a connection after several of those are opened.
	 * 
	 * @throws Exception
	 *             if a problem occurred
	 */
	@Test
	public void testConnectionValidityWithDelayedClose() throws Exception {
		final List<Connection> conns = new ArrayList<Connection>();

		// just create several connections and close those
		final Properties properties = new Properties();
		properties.setProperty(DriverProperties.PROPERTY_TIMEOUT, "0");
		properties.setProperty(DriverProperties.PROPERTY_DISABLELINGER, "true");
		for (int i = 0; i < 1000; i++) {
			final Connection conn = DriverManager.getConnection(getJdbc(),
					properties);

			// check the validity, must be true
			assertTrue("Failed in Run " + i, conn.isValid(500));
			assertTrue("Failed in Run " + i, conn.isValid(0));

			// check the type and special attributes
			final TidaConnection tc = (TidaConnection) conn;
			assertEquals(0, tc.getDriverProperties().getTimeout());
			assertEquals(1, tc.getManager().sizeOfProtocols(tc));
			assertEquals(1, tc.getManager().sizeOfScopes());
			assertEquals(1, tc.getManager().sizeOfOwners());

			// close the connection, the answer must be false
			conn.close();
			assertFalse("Failed in Run " + i, conn.isValid(0));

			// add the connection
			conns.add(conn);
		}

		for (final Connection conn : conns) {
			conn.close();
			assertFalse(conn.isValid(0));
			assertTrue(conn.isClosed());
			assertTrue(((TidaConnection) conn).getManager().isClosed());
		}
	}

	/**
	 * Tests the usage of the {@code BoneConnectionPool} to retrieve a
	 * connection.
	 * 
	 * @throws SQLException
	 *             if a problem occurs
	 */
	@Test
	public void testUsageOfBoneCp() throws SQLException {

		// create the connectionPool
		final BoneCPDataSource ds = new BoneCPDataSource();
		ds.setJdbcUrl(getJdbc());
		ds.setUsername("any");
		ds.setPassword("nopw");

		// get a connection
		final Connection conn = ds.getConnection();
		assertEquals(ConnectionHandle.class, conn.getClass());
		assertEquals(TidaConnection.class,
				(((ConnectionHandle) conn).getInternalConnection()).getClass());

		// cleanUp
		conn.close();
		ds.close();
	}
}
