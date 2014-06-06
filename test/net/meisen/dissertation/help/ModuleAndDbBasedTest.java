package net.meisen.dissertation.help;

import java.io.IOException;

import net.meisen.general.sbconfigurator.api.IModuleHolder;

import org.junit.After;

/**
 * A test which supports the usage of a {@code ModuleHolder} and a {@code Db}.
 * 
 * @author pmeisen
 * 
 * @see IModuleHolder
 * @see Db
 * @see DbBasedTest
 * @see ModuleBasedTest
 */
public class ModuleAndDbBasedTest extends ModuleBasedTest {
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
		super.cleanUpModules();
		dbBasedTest.cleanUpDb();
	}

	@Override
	public void cleanUpModules() {
		// do nothing
	}
}
