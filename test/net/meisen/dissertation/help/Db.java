package net.meisen.dissertation.help;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Scanner;
import java.util.UUID;
import java.util.regex.Pattern;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import net.meisen.general.genmisc.types.Files;
import net.meisen.general.genmisc.types.Streams;

import org.hsqldb.Database;
import org.hsqldb.Server;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Helper class to work with databases during tests
 * 
 * @author pmeisen
 * 
 */
public class Db {
	private final static Logger LOG = LoggerFactory.getLogger(Db.class);

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

		// disable any logging
		hSqlDb.setLogWriter(null);
		hSqlDb.setErrWriter(null);
		hSqlDb.setTrace(false);
		hSqlDb.setSilent(true);

		// define the address
		hSqlDb.setAddress("localhost");
		hSqlDb.setPort(6666);
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
		final InputStream zipDb = Db.class.getResourceAsStream(classPath);

		// get the zip-archive, the path to unzip to and a buffer
		final ZipInputStream zis = new ZipInputStream(zipDb);
		final byte[] buffer = new byte[1024];
		final File dbFile = new File(getTmpFolder(), dbName);

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
			Streams.closeIO(zis);
			Streams.closeIO(zipDb);
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
	 * Opens an empty database with the specified {@code dbName} at the
	 * specified {@code path}.
	 * 
	 * @param dbName
	 *            the name of the database to be opened
	 * @param path
	 *            the path to create the new empty database add
	 * @param waitAndWrap
	 *            {@code true} if the method should return after the server is
	 *            shutdown and the database should be wrapped in a zip
	 * 
	 * @throws IOException
	 *             if the database cannot be created
	 * @throws IllegalArgumentException
	 *             if the {@code path} already contains files or if the
	 *             specified path points to a file
	 */
	public void openNewDb(final String dbName, final File path,
			final boolean waitAndWrap) throws IOException {
		openNewDbs(new String[] { dbName }, path, waitAndWrap);
	}

	/**
	 * Opens an empty database with the specified {@code dbName} at the
	 * specified {@code path}.
	 * 
	 * @param dbNames
	 *            the names of the databases to be opened
	 * @param path
	 *            the path to create the new empty database add
	 * @param waitAndWrap
	 *            {@code true} if the method should return after the server is
	 *            shutdown and the database should be wrapped in a zip
	 * 
	 * @throws IOException
	 *             if the database cannot be created
	 * @throws IllegalArgumentException
	 *             if the {@code path} already contains files or if the
	 *             specified path points to a file
	 */
	public void openNewDbs(final String[] dbNames, final File path,
			final boolean waitAndWrap) throws IOException {

		// check some requirements
		if (!path.exists() && !path.mkdirs()) {
			throw new IllegalArgumentException("Cannot create the directory '"
					+ path + "'.");
		} else if (!path.isDirectory()) {
			throw new IllegalArgumentException("The path '"
					+ Files.getCanonicalPath(path)
					+ "' doesn't point to a directory.");
		} else if (waitAndWrap) {
			for (final String dbName : dbNames) {
				if (new File(path, dbName + ".zip").exists()) {
					throw new IllegalArgumentException(
							"The file '"
									+ dbName
									+ ".zip' already exists and should be deleted prior to creating a new database with the name.");
				} else if (new File(path, "dbName").exists()) {
					throw new IllegalArgumentException(
							"The directory '"
									+ dbName
									+ "' already exists and should be deleted prior to creating a new database with the name.");
				}
			}
		}

		// now open an empty database
		final Map<String, File> dbs = new HashMap<String, File>();

		for (final String dbName : dbNames) {
			dbs.put(dbName, path);
		}
		setUpDb(dbs, false);

		// if asked for lets wait
		if (waitAndWrap) {
			while (hSqlDb.getServerThread() != null) {
				try {
					Thread.sleep(100);
				} catch (final InterruptedException e) {
					// end it
					break;
				}
			}

			// delete unneeded files
			for (final String dbName : dbNames) {
				final File destDir = new File(path, dbName);
				assertTrue("Cannot create '" + destDir + "'", destDir.mkdirs());

				// copy the needed files
				Files.copyFile(new File(path, dbName + ".properties"),
						new File(destDir, dbName + ".properties"));
				Files.copyFile(new File(path, dbName + ".script"), new File(
						destDir, dbName + ".script"));

				// delete all the unneeded files of the database
				assertTrue(Files.deleteDir(new File(path, dbName + ".tmp")));
				Files.bulkDeleteFiles(Files.getFilelist(path, null,
						Pattern.compile("^" + dbName + "\\..*$")));

				// zip it
				Files.zipDirectory(destDir, new File(path, dbName + ".zip"));

				// delete the database
				assertTrue(Files.deleteDir(destDir));
			}
		}
	}

	/**
	 * Start the database
	 */
	public void setUpDb() {
		setUpDb(false);
	}

	/**
	 * Start the database.
	 * 
	 * @param wait
	 *            decide if the thread should sleep while the database is
	 *            running
	 */
	public void setUpDb(final boolean wait) {
		setUpDb(dbs, wait);
	}

	/**
	 * Starts the database server with the specified databases
	 * 
	 * @param dbs
	 *            the databases to be added
	 * @param wait
	 *            decide if the thread should sleep while the database is
	 *            running
	 */
	protected void setUpDb(final Map<String, File> dbs, final boolean wait) {

		// make sure it's not running
		hSqlDb.shutdown();

		// add the tidaGhTasks database
		int i = 0;
		for (final Entry<String, File> e : dbs.entrySet()) {
			if (LOG.isTraceEnabled()) {
				LOG.trace("Adding database '" + e.getKey() + "'...");
			}

			hSqlDb.setDatabaseName(i, e.getKey());
			hSqlDb.setDatabasePath(i,
					"file:" + Files.getCanonicalPath(e.getValue()) + e.getKey());
			i++;
		}

		// start the server
		if (LOG.isTraceEnabled()) {
			LOG.trace("Starting HSqlDbMs...");
		}
		hSqlDb.start();
		if (LOG.isTraceEnabled()) {
			LOG.trace("HSqlDbMs is started!");
		}

		// if asked for lets wait
		if (wait) {
			while (hSqlDb.getServerThread() != null) {
				try {
					Thread.sleep(100);
				} catch (final InterruptedException e) {
					// end it
					break;
				}
			}
		}
	}

	/**
	 * Shutdown the database.
	 */
	public void shutDownDb() {
		Exception exception = null;

		try {

			// try to force the shutdown
			hSqlDb.shutdownWithCatalogs(Database.CLOSEMODE_IMMEDIATELY);
		} catch (final Exception e1) {
			// ignore any error as long as the files can be removed
			exception = exception == null ? null : e1;
		} finally {
			try {

				// finally try to close it again after the force
				hSqlDb.shutdown();
			} catch (final Exception e2) {
				// silent
				exception = exception == null ? null : e2;
			}
		}

		// enable it again
		assertTrue(exception == null ? null : exception.getMessage(),
				Files.deleteDir(getTmpFolder()));
	}

	/**
	 * Queries the database for some data.
	 * 
	 * @param dbName
	 *            the database to retrieve data from
	 * @param query
	 *            the query to be fired
	 * 
	 * @return the retrieved data
	 * 
	 * @throws SQLException
	 *             if the query could not be fired
	 */
	public List<Map<String, Object>> query(final String dbName,
			final String query) throws SQLException {
		final Connection c = DriverManager.getConnection(
				"jdbc:hsqldb:hsql://localhost:6666/" + dbName, "SA", "");
		final Statement stmnt = c.createStatement();
		final ResultSet rs = stmnt.executeQuery(query);
		final ResultSetMetaData metaData = rs.getMetaData();

		final List<Map<String, Object>> table = new ArrayList<Map<String, Object>>();
		while (rs.next()) {
			final Map<String, Object> row = new LinkedHashMap<String, Object>();

			for (int i = 1; i <= metaData.getColumnCount(); i++) {
				final String key = metaData.getColumnName(i);
				final Object value = rs.getObject(key);

				row.put(key, value);
			}

			table.add(row);
		}

		// close everything
		rs.close();
		stmnt.close();
		c.close();

		return table;
	}

	/**
	 * Main method to create new database instances within the
	 * {@code net/meisen/dissertation/data/impl/hsqldbs} test directory.
	 * 
	 * @param args
	 *            arguments which can be a list of database names
	 * 
	 * @throws IOException
	 *             if a database file cannot be created or zipped
	 */
	public static void main(final String[] args) throws IOException {
		final Db db = new Db();

		final String[] dbs;

		boolean load = false;
		if (args == null || args.length == 0) {
			final Scanner scanner = new Scanner(System.in);
			final List<String> dbList = new ArrayList<String>();

			System.out
					.println("You didn't specify any new databases, please do it now by entering a database (empty line will stop input):");

			while (true) {
				final String eingabe = scanner.nextLine();

				if (eingabe.equals("-load")) {
					System.out
							.println("Switched to loading mode, all databases will be loaded from the classpath.");
					load = true;
				} else if (dbList.contains(eingabe)) {
					System.out.println("Skipped '" + eingabe
							+ "' because it is already added.");
				} else if (eingabe.trim().equals("")) {
					break;
				} else if (!eingabe.matches("^[a-zA-Z0-9\\-_]+")) {
					System.out
							.println("Invalid name for a database, please use only alphanumeric characters and _ or -");
				} else {
					dbList.add(eingabe);
				}
			}

			dbs = dbList.toArray(new String[] {});
		} else if (args != null && args.length == 1 && args[0].equals("-load")) {
			load = true;

			final Scanner scanner = new Scanner(System.in);
			final List<String> dbList = new ArrayList<String>();

			System.out
					.println("Specify any database to be loaded, please do it now by entering a database (empty line will stop input):");

			while (true) {
				final String eingabe = scanner.nextLine();

				if (dbList.contains(eingabe)) {
					System.out.println("Skipped '" + eingabe
							+ "' because it is already added.");
				} else if (eingabe.trim().equals("")) {
					break;
				} else if (!eingabe.matches("^[a-zA-Z0-9\\-_]+")) {
					System.out
							.println("Invalid name for a database, please use only alphanumeric characters and _ or -");
					System.out.println(eingabe);
				} else {
					dbList.add(eingabe);
				}
			}

			dbs = dbList.toArray(new String[] {});
		} else {
			dbs = args;
		}

		final File dbLoc = new File("test/net/meisen/dissertation/impl/hsqldbs");

		System.out.println("Going to start the following databases:");
		for (final String arg : dbs) {
			System.out.println("- " + arg);
		}
		System.out
				.println("The databases will be available until sending 'SHUTDOWN IMMEDIATELY', after that they will be wrapped and packed in separated zip-files, located at:");
		System.out.println("- " + Files.getCanonicalPath(dbLoc));

		// so open them now
		if (load) {
			for (final String dbName : dbs) {
				db.addDb(dbName, "/net/meisen/dissertation/impl/hsqldbs/"
						+ dbName + ".zip");
			}
			db.setUpDb(true);
		} else {
			db.openNewDbs(dbs, dbLoc, true);
		}

		// just call it for cleanUp
		db.shutDownDb();
	}

	/**
	 * Gets the created tmp-folder of the database if non was specified.
	 * 
	 * @return the created tmp-folder of the database if non was specified
	 */
	public File getTmpFolder() {
		return tmpFolder;
	}

}
