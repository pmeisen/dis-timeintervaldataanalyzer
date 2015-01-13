package net.meisen.dissertation.impl.cache;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;

import net.meisen.dissertation.model.cache.IIdentifierCacheConfig;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.genmisc.types.Streams;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * A {@code FileIdentifierCache} is used to cache information about the
 * identifiers used to identify the different data internally.
 * 
 * @author pmeisen
 * 
 */
public class FileIdentifierCache extends BaseIdentifierCache {
	private final static Logger LOG = LoggerFactory
			.getLogger(FileIdentifierCache.class);

	/**
	 * The name of the file used to store the meta-data
	 */
	protected final static String identifierFileName = "id.data";

	private File location;
	private File modelLocation;
	private File identifierFile;
	private RandomAccessFile identifierFileWriter;

	@Override
	protected void cacheIdentifier(final int lastUsedIdentifier) {
		try {
			identifierFileWriter.seek(0);
			identifierFileWriter.write(Streams.intToByte(lastUsedIdentifier));
		} catch (final IOException e) {
			exceptionRegistry.throwException(
					FileIdentifierCacheException.class, 1002, e,
					lastUsedIdentifier, identifierFile);
		}
	}

	@Override
	protected void cacheBitmap(final Bitmap newBitmap) {
		try {
			identifierFileWriter.seek(Streams.SIZEOF_INT);
			newBitmap.serialize(identifierFileWriter);
		} catch (final IOException e) {
			exceptionRegistry.throwException(
					FileIdentifierCacheException.class, 1003, e, newBitmap,
					identifierFile);
		}
	}

	public void release() {

		if (!isInitialized()) {
			return;
		} else {

			// now reset the initialization
			super.release();

			try {
				identifierFileWriter.close();
			} catch (final IOException e) {
				exceptionRegistry.throwException(
						FileIdentifierCacheException.class, 1004,
						identifierFile);
			}
		}
	}

	@Override
	public void setConfig(final IIdentifierCacheConfig config) {
		if (isInitialized()) {
			exceptionRegistry.throwException(
					FileIdentifierCacheException.class, 1000);
		} else if (config == null) {
			this.location = null;
		} else if (config instanceof FileIdentifierCacheConfig) {
			final FileIdentifierCacheConfig fcc = (FileIdentifierCacheConfig) config;

			final File cLoc = fcc.getLocation();
			this.location = cLoc == null ? null : fcc.getLocation();
		} else {
			exceptionRegistry.throwException(
					FileIdentifierCacheException.class, 1001, config.getClass()
							.getName());
		}
	}

	@Override
	public void initialize(final TidaModel model) {

		// if already initialized we are done
		if (isInitialized()) {
			return;
		}

		// read the values needed
		final String modelId = model.getId();
		final File modelLocation = model.getLocation();

		// determine the location of the model
		if (location != null) {
			this.modelLocation = new File(location, modelId);
		} else if (modelLocation != null) {
			this.modelLocation = modelLocation;
			this.location = modelLocation.getParentFile();
		} else {
			this.location = getDefaultLocation();
			this.modelLocation = new File(getDefaultLocation(), modelId);
		}

		// create the location of the identifierFileName
		if (!this.modelLocation.exists() && !this.modelLocation.mkdirs()) {
			exceptionRegistry
					.throwException(FileIdentifierCacheException.class, 100,
							this.modelLocation);
		}

		// get the init-values
		final int lastUsedIdentifier;
		final Bitmap bitmap;

		// create the file if needed
		identifierFile = new File(this.modelLocation, identifierFileName);
		if (!identifierFile.exists()) {
			try {
				identifierFile.createNewFile();
			} catch (final IOException e) {
				exceptionRegistry.throwException(
						FileIdentifierCacheException.class, 1009, e,
						identifierFile);
			}
		}

		// get a handler for the file
		try {
			identifierFileWriter = new RandomAccessFile(identifierFile, "rws");
		} catch (final FileNotFoundException e) {
			exceptionRegistry
					.throwException(FileIdentifierCacheException.class, 1005,
							e, identifierFile);
		}

		if (identifierFile.length() > 0) {
			if (identifierFile.isFile()) {
				if (LOG.isDebugEnabled()) {
					LOG.debug("Loading the FileIdentifierCache from '"
							+ identifierFile + "'.");
				}

				// read the lastUsedIdentifier
				final byte[] intBytes = new byte[4];
				try {
					identifierFileWriter.read(intBytes);
				} catch (final IOException e) {
					exceptionRegistry.throwException(
							FileIdentifierCacheException.class, 1006, e,
							identifierFile);
				}
				lastUsedIdentifier = Streams.byteToInt(intBytes);

				// read the bitmap
				try {
					bitmap = Bitmap.createFromInput(indexFactory,
							identifierFileWriter);
				} catch (final IOException e) {
					exceptionRegistry.throwException(
							FileIdentifierCacheException.class, 1007, e,
							identifierFile);
					return;
				}
			} else {
				exceptionRegistry.throwException(
						FileIdentifierCacheException.class, 1008,
						identifierFile);
				return;
			}
		}
		// or create it an use a default value
		else {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Initializing the FileIdentifierCache at '"
						+ identifierFile + "'.");
			}

			// initialize the default values
			lastUsedIdentifier = -1;
			bitmap = indexFactory.createBitmap();
		}

		// initialize the base-class and finalize initialization
		initialize(lastUsedIdentifier, bitmap);
		markAsInitialized();
	}

	/**
	 * Gets the default-location used by the {@code FileIdentifierCache} if no
	 * other is specified.
	 * 
	 * @return the default-location used by the {@code FileIdentifierCache}
	 */
	protected File getDefaultLocation() {
		return new File(".");
	}

	/**
	 * Gets the root-location of the caches. The {@code FileIdentifierCache}
	 * generates a sub-folder within this {@code location}.
	 * 
	 * @return the root-location of the caches
	 */
	public File getLocation() {
		return this.location;
	}

	/**
	 * Gets the location where the data of the cache is stored. This location is
	 * a sub-folder within the location specified by {@link #getLocation()}.
	 * 
	 * @return the location the cache stores data
	 */
	public File getModelLocation() {
		return this.modelLocation;
	}

	@Override
	public void remove() {
		if (isInitialized()) {
			exceptionRegistry.throwException(
					FileIdentifierCacheException.class, 1011);
		}

		if (getModelLocation() == null) {
			// nothing to do
		} else if (!Files.deleteOnExitDir(getModelLocation())
				&& LOG.isErrorEnabled()) {
			LOG.error("Unable to delete the files created for the cache '"
					+ getClass().getSimpleName() + "' at '"
					+ Files.getCanonicalPath(getModelLocation()) + "'");
		}
	}
}
