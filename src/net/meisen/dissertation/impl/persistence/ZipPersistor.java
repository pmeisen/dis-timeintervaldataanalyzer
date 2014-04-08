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
import net.meisen.dissertation.model.persistence.BasePersistor;
import net.meisen.dissertation.model.persistence.IFileLocation;
import net.meisen.dissertation.model.persistence.ILocation;
import net.meisen.dissertation.model.persistence.Identifier;
import net.meisen.dissertation.model.persistence.MetaData;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
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

	/**
	 * Constructor specifying the {@code exceptionRegistry} to be used.
	 * 
	 * @param exceptionRegistry
	 *            the {@code exceptionRegistry} to be used
	 */
	public ZipPersistor(final IExceptionRegistry exceptionRegistry) {
		super(exceptionRegistry);
	}

	@Override
	public void save(final ILocation location, final MetaData... additionalData) {
		if (zipOutputStream != null) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1005);
		} else if (location == null) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1011);
		}

		// get the file to save to
		final File file = resolveSaveLocation(location);

		// create the FileOutputStream to write to the file
		final FileOutputStream fos;
		try {
			fos = new FileOutputStream(file);
		} catch (final Exception e) {
			final String path = Files.getCanonicalPath(file);
			exceptionRegistry.throwException(ZipPersistorException.class, 1004,
					e, path == null ? location : path);

			return;
		}

		// create the ZipOutputStream
		this.zipOutputStream = new ZipOutputStream(fos);

		// write the MetaData
		this.writeMetaData(additionalData);

		// write the persistables
		try {
			writePersistables();
		} catch (final ForwardedRuntimeException e) {

			// cleanup without any further exceptions
			Streams.closeIO(this.zipOutputStream);
			Streams.closeIO(fos);

			// throw the forwarded exception
			exceptionRegistry.throwRuntimeException(e);
		} catch (final RuntimeException e) {

			// cleanup without any further exceptions
			Streams.closeIO(this.zipOutputStream);
			Streams.closeIO(fos);

			// throw the exception
			throw e;
		}

		// try to close the handler fine and handle the exception
		try {
			if (getPersistables().size() + additionalData.length > 0) {
				this.zipOutputStream.flush();
				this.zipOutputStream.close();
			}
			fos.close();
		} catch (final IOException e) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1014,
					e);
		}

		// make sure we are done with the current one
		this.zipOutputStream = null;
	}

	@Override
	protected OutputStream _openForWrite(final Identifier identifier) {
		if (this.zipOutputStream == null) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1006);
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
	public void load(final ILocation location, final MetaData... additionalData) {
		final File file = resolveLoadLocation(location);

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

			try {

				// call the persistable to handle the entry
				final Identifier identifier = Identifier.createFromString(
						entry.getName(), "/");
				read(identifier, inputStream, additionalData);
			} catch (final ForwardedRuntimeException e) {

				// close the stream ignore any exception thrown there
				Streams.closeIO(inputStream);

				// throw the exception
				exceptionRegistry.throwRuntimeException(e);
			} catch (final RuntimeException e) {

				// close the stream ignore any exception thrown there
				Streams.closeIO(inputStream);

				// throw the real exception again
				throw e;
			}

			// if we came so far close the stream with an exception handling
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
	 * Resolves a location to be a save spot, i.e. checks if it's not a
	 * directory, if it can be written to and creates needed folder structure.
	 * 
	 * @param location
	 *            the location to resolve
	 * 
	 * @return the resolved location
	 */
	protected File resolveSaveLocation(final ILocation location) {
		if (location instanceof IFileLocation) {

		}

		final File file = ((IFileLocation) location).getFile();
		if (file.isDirectory()) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1000,
					location);
		} else if (file.exists() && !file.delete()) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1002,
					Files.getCanonicalPath(file));
		}

		// create the directories for the parent
		final File folder = file.getParentFile();
		if (folder == null || (!folder.exists() && !folder.mkdirs())) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1003,
					folder == null ? location : Files.getCanonicalPath(folder));
		}

		return file;
	}

	/**
	 * Resolves a {@code Location} to be used as loading location, e.g. if it
	 * exists, if it's readable.
	 * 
	 * @param location
	 *            the location to be resolved to a file
	 * @return the file to load from
	 */
	protected File resolveLoadLocation(final ILocation location) {
		if (location instanceof IFileLocation == false) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1008,
					location);
		}

		final File file = ((IFileLocation) location).getFile();
		if (file.isDirectory()) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1000,
					location);
		} else if (!file.exists()) {
			exceptionRegistry.throwException(ZipPersistorException.class, 1001,
					Files.getCanonicalPath(file));
		}

		return file;
	}
}
