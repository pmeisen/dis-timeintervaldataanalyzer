package net.meisen.dissertation.help;

import java.io.IOException;

import org.junit.After;

/**
 * A test which needs to retrieve a single database instance within a test. Use
 * {@link #getDb(String)} to retrieve a {@code Db} instance.
 * 
 * @author pmeisen
 * 
 */
public class DbBasedTest extends ExceptionBasedTest {
	private Db db;

	/**
	 * Helper method to load the specified database.
	 * 
	 * @param classpathDb
	 *            the path to the database
	 * 
	 * @return the created {@code DB}.
	 * 
	 * @throws IOException
	 *             if the path leads to an exception
	 */
	public Db getDb(final String classpathDb) throws IOException {
		return getDb(null, classpathDb);
	}

	/**
	 * Helper method to load the specified database. The database is returned
	 * and {@link Db#setUpDb()} is already called.
	 * 
	 * @param name
	 *            the name of the db, by default, i.e. {@code null} is passed,
	 *            {@code tidaTestData} is used
	 * @param classpathDb
	 *            the path to the database
	 * 
	 * @return the created {@code DB}.
	 * 
	 * @throws IOException
	 *             if the path leads to an exception
	 */
	public Db getDb(final String name, final String classpathDb)
			throws IOException {
		if (db != null) {
			db.shutDownDb();
		}

		db = new Db();
		db.addDb(name == null ? "tidaTestData" : name, classpathDb);
		db.setUpDb();

		return db;
	}

	/**
	 * CleansUp by shutting down the {@code Db}.
	 */
	@After
	public void cleanUpDb() {
		if (db != null) {
			db.shutDownDb();
			db = null;
		}
	}
}
