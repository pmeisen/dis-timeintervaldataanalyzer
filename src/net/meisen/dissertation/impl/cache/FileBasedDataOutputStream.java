package net.meisen.dissertation.impl.cache;

import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;

/**
 * DataOutputStream which writes to a file. If the append mode is enabled, the
 * implementation provides the current-position of the pointer in bytes, i.e.
 * the next position the system will write to.
 * 
 * @author pmeisen
 * 
 */
public class FileBasedDataOutputStream extends DataOutputStream {

	private long offset;
	private boolean append;

	/**
	 * Creates a {@code DataOuputStream} for the specified file and the
	 * specified {@code append}-mode.
	 * 
	 * @param dataFile
	 *            the file to write to
	 * @param append
	 *            {@code true} if append should be enabled, otherwise
	 *            {@code false}
	 * 
	 * @throws FileNotFoundException
	 *             if the file could not be found
	 */
	public FileBasedDataOutputStream(final File dataFile, final boolean append)
			throws FileNotFoundException {
		super(new BufferedOutputStream(new FileOutputStream(dataFile, append)));

		this.append = append;
		this.offset = dataFile.length();
	}

	/**
	 * Gets the current position of the file, if and only if {@code append} is
	 * set to {@code true}.
	 * 
	 * @return the current position within the file the next write will occur at
	 *         (in bytes), returns {@code Integer.MAX_VALUE} if the position is
	 *         to large and {@code -1} if append is set to {@code false}
	 */
	public int getCurrentPosition() {
		if (append) {
			final long curPos = this.offset + this.size();
			return curPos > Integer.MAX_VALUE ? Integer.MAX_VALUE
					: (int) curPos;
		} else {
			return -1;
		}
	}
}
