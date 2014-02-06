package net.meisen.dissertation.help;

import java.io.IOException;

/**
 * A runner to start the passed databases.
 * 
 * @author pmeisen
 * 
 */
public class DbRunner {

	private final Db db;

	/**
	 * Wrapper to run several databases.
	 * 
	 * @param dbs
	 *            the databases to run
	 */
	public DbRunner(final String[] dbs) {
		this.db = new Db();

		// get the databases
		for (final String db : dbs) {
			final String classpathDb = "/net/meisen/dissertation/impl/hsqldbs/"
					+ db + ".zip";

			// check availability
			if (DbRunner.class.getResource(classpathDb) == null) {
				throw new IllegalArgumentException("Cannot find '" + db
						+ "' under '" + classpathDb + "'.");
			} else {
				try {
					this.db.addDb(db, classpathDb);
				} catch (final IOException e) {
					throw new IllegalArgumentException("Cannot open '" + db
							+ "' under '" + classpathDb + "'.", e);
				}
			}
		}
	}

	/**
	 * Start databases.
	 */
	public void setup() {
		System.out.println("The database is up and running now...");
		System.out.println("-> send 'SHUTDOWN IMMEDIATELY' to close it");
		db.setUpDb(true);
	}

	/**
	 * Shutdown databases.
	 */
	public void shutdown() {
		db.shutDownDb();
	}

	/**
	 * Main method to start the runner.
	 * 
	 * @param args
	 *            the list of databases to run.
	 */
	public static void main(final String[] args) {
		int exitCode = 0;

		try {
			final DbRunner runner = new DbRunner(args);

			// register the hook
			Runtime.getRuntime().addShutdownHook(new Thread() {

				@Override
				public void run() {
					if (runner != null) {
						runner.shutdown();
					}
				}
			});

			try {
				runner.setup();
			} catch (final Exception e) {
				System.err.println(e.getMessage());
				exitCode = 1;
			} finally {
				runner.shutdown();
			}
		} catch (final Exception e) {
			System.err.println(e.getMessage());
			exitCode = 1;
		}

		System.exit(exitCode);
	}
}
