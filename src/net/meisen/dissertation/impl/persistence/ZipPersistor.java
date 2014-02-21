package net.meisen.dissertation.impl.persistence;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Enumeration;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;
import java.util.zip.ZipOutputStream;

import net.meisen.dissertation.exceptions.ZipPersistorException;
import net.meisen.dissertation.model.IPersistable;
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.genmisc.types.Streams;

/**
 * A {@code persistor} which persists the data into a zip-file.
 * 
 * @author pmeisen
 * 
 */
public class ZipPersistor extends BasePersistor {

	private ZipOutputStream zipOutputStream = null;

	public void save(final String location) {
		if (zipOutputStream != null) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1005);
		} else if (location == null) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1011);
		}

		final File file = resolveLocation(location);
		if (file.exists() && !file.delete()) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1002,
					Files.getCanonicalPath(file));
		}

		// create the directories for the parent
		final File folder = file.getParentFile();
		if (folder == null || (!folder.exists() && !folder.mkdirs())) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1003,
					folder == null ? location : Files.getCanonicalPath(folder));
		}

		// create the FileOutputStream to write to the file
		FileOutputStream fos = null;
		try {
			fos = new FileOutputStream(file);
		} catch (final Exception e) {
			final String path = Files.getCanonicalPath(file);
			exceptionRegistry.throwException(ZipPersistorException.class, 1004,
					e, path == null ? location : path);
		}

		// create the ZipOutputStream
		this.zipOutputStream = new ZipOutputStream(fos);
		for (final IPersistable persistable : getPersistables().values()) {
			try {
				persistable.save(this);
			} catch (final ForwardedRuntimeException e) {
				exceptionRegistry.throwRuntimeException(e);
			}
		}

		// close the handler
		try {
			this.zipOutputStream.flush();
		} catch (final IOException e) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1014,
					e);
		}
		Streams.closeIO(this.zipOutputStream);
		Streams.closeIO(fos);

		// make sure we are done with the current one
		this.zipOutputStream = null;
	}

	@Override
	public OutputStream openForWrite(final Identifier identifier) {
		if (this.zipOutputStream == null) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1006);
		} else if (getPersistable(identifier.getGroup()) == null) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1008,
					identifier.getGroup());
		}

		// create the entry and return the OutputStream to write to
		final ZipEntry zipEntry = new ZipEntry(identifier.toString("/"));

		// set the comment if one is defined
		if (identifier.getComment() != null) {
			zipEntry.setComment(identifier.getComment());
		}

		// add the entry to the archive
		try {
			this.zipOutputStream.putNextEntry(zipEntry);
		} catch (final IOException exception) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1007,
					identifier);
		}

		return this.zipOutputStream;
	}

	@Override
	public void close(final Identifier identifier) {
		if (this.zipOutputStream == null) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1006);
		}

		// close the current entry
		try {
			this.zipOutputStream.closeEntry();
		} catch (final IOException exception) {

		}
	}

	@Override
	public void load(final String location) {
		final File file = resolveLocation(location);
		if (!file.exists()) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1001,
					Files.getCanonicalPath(file));
		}

		ZipFile zipFile = null;
		try {
			zipFile = new ZipFile(file);
		} catch (final ZipException e) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1009,
					e, Files.getCanonicalPath(file));
		} catch (final IOException e) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1010,
					e, Files.getCanonicalPath(file));
		}

		final Enumeration<? extends ZipEntry> entries = zipFile.entries();
		while (entries.hasMoreElements()) {
			final ZipEntry entry = entries.nextElement();

			InputStream inputStream = null;
			try {
				inputStream = zipFile.getInputStream(entry);
			} catch (final IOException e) {
				exceptionRegistry.throwException(ZipPersistorException.class,
						1013, e, Files.getCanonicalPath(file), entry.getName());
			}

			// call the persistable to handle the entry
			final Identifier identifier = Identifier.createFromString(
					entry.getName(), "/");
			final IPersistable persistable = getPersistable(identifier
					.getGroup());
			if (persistable == null) {
				exceptionRegistry.throwException(ZipPersistorException.class,
						1015, identifier.getGroup());
			}
			persistable.load(this, identifier, inputStream);

			try {
				inputStream.close();
			} catch (final IOException e) {
				exceptionRegistry.throwException(ZipPersistorException.class,
						1013, e, Files.getCanonicalPath(file), entry.getName());
			}
		}

		try {
			zipFile.close();
		} catch (final IOException e) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1012,
					e, Files.getCanonicalPath(file));
		}
	}

	/**
	 * Resolves a location generally, i.e. checks if it's not a directory.
	 * 
	 * @param location
	 *            the location to resolve
	 * 
	 * @return the resolved location
	 */
	protected File resolveLocation(final String location) {
		final File resLocation = new File(location);
		if (resLocation.isDirectory()) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1000,
					Files.getCanonicalPath(location));
		}

		return resLocation;
	}
}
