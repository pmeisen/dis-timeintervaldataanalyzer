package net.meisen.dissertation.help;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import net.meisen.general.genmisc.types.Files;

import org.junit.After;
import org.junit.Test;

/**
 * Tests the implementation of a sample database
 * 
 * @author pmeisen
 * 
 */
public class TestDb {
	private final Db db = new Db();

	/**
	 * Tests the creation of an empty database
	 * 
	 * @throws IOException
	 *             if the database cannot be created
	 */
	@Test
	public void testCreateDb() throws IOException {
		final String tmpDir = System.getProperty("java.io.tmpdir");
		final File tmpDbDir = new File(tmpDir, UUID.randomUUID().toString());

		// create the Database
		db.openNewDb("tidaNewTestData", tmpDbDir, false);

		// check if the directory is created
		assertTrue(tmpDbDir.exists());
		assertTrue(tmpDbDir.isDirectory());

		// close the connection again and cleanUp
		db.shutDownDb();
		assertTrue(Files.deleteDir(tmpDbDir));

		// next wait for a thread to close the server
		final Thread t = new Thread() {
			@Override
			public void run() {
				try {
					Thread.sleep(500);
				} catch (InterruptedException e) {
					// nothing to do
				}

				db.shutDownDb();
			}
		};
		t.start();

		// open a new database and let's wait for the Thread to end it
		db.openNewDb("tidaNewTestData", tmpDbDir, true);
		assertTrue(tmpDbDir.exists());
		assertTrue(new File(tmpDbDir, "tidaNewTestData.zip").exists());
		assertTrue(Files.deleteDir(tmpDbDir));
	}

	/**
	 * Tests the query implementation
	 * 
	 * @throws SQLException
	 *             if the SQL is invalid
	 * @throws IOException
	 *             if the database cannot be opened
	 */
	@Test
	public void testSqlQuery() throws SQLException, IOException {
		db.addDb("tidaTestData",
				"/net/meisen/dissertation/impl/hsqldbs/tidaTestData.zip");
		db.setUpDb();

		final List<Map<String, Object>> data = db.query("tidaTestData",
				"SELECT * FROM TB_TESTDATA");
		assertEquals(10000, data.size());
	}

	/**
	 * Make sure the database is cleanUp
	 */
	@After
	public void cleanUp() {

		// stop the server
		db.shutDownDb();
	}
}
