package net.meisen.dissertation.impl.persistence;

import java.io.File;

import net.meisen.dissertation.model.persistence.IFileLocation;
import net.meisen.general.genmisc.types.Files;

/**
 * A location handling a file.
 * 
 * @author pmeisen
 * 
 */
public class FileLocation implements IFileLocation {
	private final File file;

	/**
	 * Default creator for file locations.
	 * 
	 * @param file
	 *            the file to be addressed by {@code this}
	 */
	public FileLocation(final File file) {
		this.file = file;
	}

	/**
	 * Get the file defined by this {@code Location}.
	 * 
	 * @return the file represented by the location
	 */
	public File getFile() {
		return file;
	}

	public String toString() {
		if (file == null) {
			return null;
		} else if (file.exists()) {
			return Files.getCanonicalPath(file);
		} else {
			return file.toString();
		}
	}
}
