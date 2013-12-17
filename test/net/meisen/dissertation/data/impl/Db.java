package net.meisen.dissertation.data.impl;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.hsqldb.Database;
import org.hsqldb.Server;

import static org.junit.Assert.assertTrue;

import net.meisen.general.genmisc.types.Files;

/**
 * Helper class to work with databases during tests
 * 
 * @author pmeisen
 * 
 */
public class Db {

	private final File tmpFolder;
	private final Server hSqlDb;
	private final Map<String, File> dbs;

	/**
	 * Constructor to store the database in a temporary directory.
	 */
	public Db() {
		this(generateTmpDir());
	}

	/**
	 * Constructor to specify the directory of the database.
	 * 
	 * @param dbDir
	 *            the directory of the database
	 */
	public Db(final String dbDir) {
		this(new File(dbDir));
	}

	/**
	 * Constructor to specify the directory of the database.
	 * 
	 * @param dbDir
	 *            the directory of the database
	 */
	public Db(final File dbDir) {
		tmpFolder = dbDir;
		hSqlDb = new Server();
		dbs = new HashMap<String, File>();
	}

	/**
	 * Generates a temporary directory.
	 * 
	 * @return the created directory
	 */
	protected static File generateTmpDir() {
		final String tmpDir = System.getProperty("java.io.tmpdir");
		final String uuid = UUID.randomUUID().toString();

		final File tmpDbDir = new File(tmpDir, uuid);
		assertTrue(tmpDbDir.mkdirs());

		return tmpDbDir;
	}

	/***
	 * Unzips the specified file into a directory
	 * 
	 * @param dbName
	 *            the name of the database-file to be used
	 * @param classPath
	 *            the file on the classpath
	 * 
	 * @return the created directory with the files
	 * 
	 * @throws IOException
	 *             if the file cannot be accessed
	 */
	protected File unzip(final String dbName, final String classPath)
			throws IOException {
		final InputStream zipDb = TestDataRetrieval.class
				.getResourceAsStream(classPath);

		// get the zip-archive, the path to unzip to and a buffer
		final ZipInputStream zis = new ZipInputStream(zipDb);
		final byte[] buffer = new byte[1024];
		final File dbFile = new File(tmpFolder, dbName);

		try {
			ZipEntry ze = zis.getNextEntry();
			while (ze != null) {

				// get the output to the file
				final File newFile = new File(dbFile, ze.getName());
				newFile.getParentFile().mkdirs();
				final FileOutputStream fos = new FileOutputStream(newFile);

				// write the output
				int len;
				while ((len = zis.read(buffer)) > 0) {
					fos.write(buffer, 0, len);
				}

				// flush and close the file
				fos.flush();
				fos.close();

				ze = zis.getNextEntry();
			}
		} catch (final IOException e) {
			throw e;
		} finally {
			zis.close();
		}

		return dbFile;
	}

	/**
	 * Add a specific file to the database
	 * 
	 * @param dbName
	 *            the name of the database-file to be used
	 * @param classPath
	 *            the file on the classpath
	 * 
	 * @throws IOException
	 *             if the file cannot be accessed
	 */
	public void addDb(final String dbName, final String classPath)
			throws IOException {
		if (dbs.containsKey(dbName)) {

			// delete everything
			shutDownDb();

			// throw the exception
			throw new IllegalArgumentException("Database with name '" + dbName
					+ "' is already used.");
		}

		final File dbFile = unzip(dbName, classPath);
		dbs.put(dbName, dbFile);
	}

	/**
	 * Start the database
	 */
	public void setUpDb() {

		// set default settings
		hSqlDb.shutdown();

		hSqlDb.setAddress("localhost");
		hSqlDb.setPort(6666);

		// disable any logging
		hSqlDb.setTrace(false);
		hSqlDb.setLogWriter(null);

		// add the tidaGhTasks database
		int i = 0;
		for (final Entry<String, File> e : dbs.entrySet()) {
			hSqlDb.setDatabaseName(i, "tidaGhTasks");
			hSqlDb.setDatabasePath(i,
					"file:" + Files.getCanonicalPath(e.getValue()) + e.getKey());
			i++;
		}

		// start the server
		hSqlDb.start();
	}

	/**
	 * Shutdown the database.
	 */
	public void shutDownDb() {
		hSqlDb.shutdownWithCatalogs(Database.CLOSEMODE_IMMEDIATELY);
		assertTrue(Files.deleteDir(tmpFolder));
	}
}
