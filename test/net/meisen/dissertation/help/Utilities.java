package net.meisen.dissertation.help;

import java.io.File;

/**
 * Helper methods for testing.
 * 
 * @author pmeisen
 * 
 */
public class Utilities {

	/**
	 * Deletes all the directories matching the specified {@code pattern} within
	 * the {@code dir}.
	 * 
	 * @param pattern
	 *            a regular expression
	 * @param dir
	 *            the directory to look for directories matching the pattern
	 */
	public static void deleteDir(final String pattern, final File dir) {

		final String[] children = dir.list();
		for (final String child : children) {
			final File sub = new File(dir, child);

			if (sub.isDirectory() && sub.getName().matches(pattern)) {
				deleteDir(sub);
			}
		}
	}

	/**
	 * Deletes the directory (and all it's content). If not deleteable it is
	 * marked to be deleted on shutdown.
	 * 
	 * @param dir
	 *            the directory to be deleted
	 */
	public static void deleteDir(final File dir) {

		if (!dir.exists()) {
			return;
		} else if (dir.isDirectory()) {
			final String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				deleteDir(new File(dir, children[i]));
			}
		}

		if (!dir.delete()) {
			dir.deleteOnExit();
		}
	}
}
