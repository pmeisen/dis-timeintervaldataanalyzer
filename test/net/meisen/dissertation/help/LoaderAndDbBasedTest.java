package net.meisen.dissertation.help;

import java.io.IOException;

import org.junit.After;

/**
 * Tests which are loader and db based.
 * 
 * @author pmeisen
 * 
 */
public class LoaderAndDbBasedTest extends LoaderBasedTest {
	private final DbBasedTest dbBasedTest = new DbBasedTest();

	/**
	 * Helper method to load the specified database.
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
	protected Db getDb(final String name, final String classpathDb)
			throws IOException {
		return dbBasedTest.getDb(name, classpathDb);
	}

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
	protected Db getDb(final String classpathDb) throws IOException {
		return dbBasedTest.getDb(classpathDb);
	}

	/**
	 * CleansUp by shutting down the {@code Db}.
	 */
	@After
	public void cleanUpModulesAndDb() {
		super.unload();
		dbBasedTest.cleanUpDb();
	}

	@Override
	public void unload() {
		// do nothing
	}
}
