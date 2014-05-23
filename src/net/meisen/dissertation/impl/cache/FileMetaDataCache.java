package net.meisen.dissertation.impl.cache;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Map.Entry;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.impl.data.metadata.LoadedMetaData;
import net.meisen.dissertation.model.cache.IMetaDataCache;
import net.meisen.dissertation.model.cache.IMetaDataCacheConfig;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.data.metadata.IIdentifiedMetaData;
import net.meisen.dissertation.model.data.metadata.IMetaData;
import net.meisen.dissertation.model.data.metadata.MetaDataCollection;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.resources.FileByteBufferReader;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.genmisc.types.Streams;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * A {@code FileMetaDataCache} is used to cache meta-data accross several
 * restarts.
 * 
 * @author pmeisen
 * 
 */
public class FileMetaDataCache implements IMetaDataCache {
	private final static Logger LOG = LoggerFactory
			.getLogger(FileMetaDataCache.class);

	/**
	 * The name of the file used to store the meta-data
	 */
	protected final static String metaDataFileName = "meta.data";
	/**
	 * The extension appended to a file-name if it's a backup
	 */
	protected final static String backupFileExtension = ".bak";

	@Autowired
	@Qualifier(DefaultValues.METADATACOLLECTION_ID)
	private MetaDataCollection metaDataCollection;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private boolean init = false;

	private File location;
	private File modelLocation;
	private File metaDataFile;

	@Override
	public void cacheMetaDataModel(final MetaDataModel model) {
		if (!init) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1003);
		}

		// get the collection and write the file
		final MetaDataCollection collection = UtilMetaDataCache
				.createCollectionForModel(model);
		write(metaDataFile, collection);

		// keep the collection as the currently cached one
		this.metaDataCollection = collection;
	}

	@Override
	public MetaDataCollection createMetaDataCollection() {
		if (!init) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1003);
		}

		if (metaDataFile.exists()) {
			if (!metaDataFile.isFile()) {
				exceptionRegistry.throwException(
						FileMetaDataCacheException.class, 1004, metaDataFile);
			} else if (LOG.isDebugEnabled()) {
				LOG.debug("Reloading meta-data from '" + metaDataFile + "'.");
			}

			// load the collection from the file
			return read(metaDataFile);
		} else {
			if (LOG.isDebugEnabled()) {
				LOG.debug("Using configured meta-data.");
			}

			// return the configuration, thats the best we have
			return metaDataCollection;
		}
	}

	/**
	 * Reads the {@code MetaDataCollection} from the specified
	 * {@code metaDataFile}.
	 * 
	 * @param metaDataFile
	 *            the file to read the data from
	 * 
	 * @return the read {@code MetaDataCollection}
	 * 
	 * @throws FileMetaDataCacheException
	 *             if the {@code metaDataFile} does not exist or isn't a file,
	 *             if the {@code metaDataFile} cannot be read, or if the
	 *             {@code metaDataFile} contains corrupted data
	 */
	protected MetaDataCollection read(final File metaDataFile)
			throws FileMetaDataCacheException {
		final MetaDataCollection collection = new MetaDataCollection();

		// make sure we have a file
		if (!metaDataFile.exists() || !metaDataFile.isFile()) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1004, metaDataFile);
		}

		// read the data
		FileByteBufferReader fbbr = null;
		try {
			fbbr = new FileByteBufferReader(metaDataFile);

			// make sure those values are valid
			while (fbbr.hasRemaining()) {

				// first we need the amount and the descId
				final Object oSize = Streams.readNextObject(fbbr);
				final Object oDescId = Streams.readNextObject(fbbr);

				// make sure we found valid size and descId
				if (oSize instanceof Integer && oDescId instanceof String) {
					final int size = ((Integer) oSize).intValue();
					final String descId = oDescId.toString();

					final LoadedMetaData metaData = new LoadedMetaData(descId);
					for (int i = 0; i < size; i++) {
						final Object key = Streams.readNextObject(fbbr);
						final Object val = Streams.readNextObject(fbbr);

						metaData.addValue(key, val);
					}

					collection.addMetaData(metaData);
				} else {
					exceptionRegistry.throwException(
							FileMetaDataCacheException.class, 1012,
							metaDataFile, oSize, oDescId);
				}
			}
		} catch (final IOException e) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1013, e, metaDataFile);
		} catch (final IllegalArgumentException e) {

			// check if a backup exists and try to load it
			final File backup = getBackUpFile(metaDataFile);
			if (backup.exists() && backup.isFile()) {
				if (LOG.isInfoEnabled()) {
					LOG.info("Loading backup file '" + backup + "', because '"
							+ metaDataFile + "' seems to be corrupted.");
				}

				return read(backup);
			} else {
				exceptionRegistry
						.throwException(FileMetaDataCacheException.class, 1014,
								e, metaDataFile);
			}
		} catch (final Exception e) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1015, e, metaDataFile);
		} finally {
			Streams.closeIO(fbbr);
		}

		return collection;
	}

	/**
	 * Writes the {@code collection} to the specified {@code metaDataFile}.
	 * 
	 * @param metaDataFile
	 *            the file to write to
	 * @param collection
	 *            the {@code MetaDataCollection} to be written
	 * 
	 * @throws FileMetaDataCacheException
	 *             if the defined {@code metaDataFile} location exists and isn't
	 *             a directory, if an existing backup cannot be deleted, if a
	 *             backup cannot be created, if the {@code metaDataFile} cannot
	 *             be created, or if the data cannot be written
	 * 
	 * @see MetaDataCollection
	 */
	protected void write(final File metaDataFile,
			final MetaDataCollection collection)
			throws FileMetaDataCacheException {
		final File backup;

		if (metaDataFile.exists()) {
			if (!metaDataFile.isFile()) {
				exceptionRegistry.throwException(
						FileMetaDataCacheException.class, 1004, metaDataFile);
			}

			backup = getBackUpFile(metaDataFile);

			// remove any existing backup
			if (backup.exists() && !backup.delete()) {
				if (LOG.isTraceEnabled()) {
					LOG.trace("Removing old backup-file at '" + backup + "'.");
				}

				exceptionRegistry.throwException(
						FileMetaDataCacheException.class, 1016, backup,
						metaDataFile);
			}

			// create a backup of the current file
			if (LOG.isTraceEnabled()) {
				LOG.trace("Creating a backup-file at '" + backup + "'.");
			}
			try {
				Files.moveFile(metaDataFile, backup);
			} catch (final IOException e) {
				exceptionRegistry.throwException(
						FileMetaDataCacheException.class, 1009, e, backup,
						metaDataFile);
			}
		} else {
			backup = null;
		}

		// create the new cache file and add the data
		try {
			metaDataFile.createNewFile();
		} catch (final IOException e) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1006, metaDataFile, e);
		}

		// create the outputStream
		final OutputStream out;
		try {
			out = new BufferedOutputStream(new FileOutputStream(metaDataFile,
					true));
		} catch (final FileNotFoundException e) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1004, e, metaDataFile);
			return;
		}

		boolean success = true;
		try {
			for (final IMetaData data : collection) {
				if (data instanceof IIdentifiedMetaData) {
					final IIdentifiedMetaData idData = (IIdentifiedMetaData) data;
					out.write(Streams.objectToByte(idData.size()));
					out.write(Streams.objectToByte(idData
							.getDescriptorModelId()));

					// write the data of the model
					for (final Entry<Object, Object> entry : idData
							.getIdentifiedValues().entrySet()) {
						out.write(Streams.objectToByte(entry.getKey()));
						out.write(Streams.objectToByte(entry.getValue()));
					}
				} else {
					success = false;
					exceptionRegistry.throwException(
							FileMetaDataCacheException.class, 1007, data);
				}
			}

			// make sure the data is written
			out.flush();
		} catch (final IOException e) {
			success = false;
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1008, metaDataFile);
		} finally {
			Streams.closeIO(out);

			// if the writing wasn't successful get the backup, delete the file
			if (!success) {
				if (!metaDataFile.delete()) {
					exceptionRegistry.throwException(
							FileMetaDataCacheException.class, 1010,
							metaDataFile, backup);
				}
			} else {
				if (backup != null) {
					if (LOG.isTraceEnabled()) {
						LOG.trace("Removing backup-file '" + backup + "'.");
					}

					if (!backup.delete()) {
						exceptionRegistry.throwException(
								FileMetaDataCacheException.class, 1011, backup);
					}
				}
			}

		}
	}

	/**
	 * Gets the name of the backup-file for the {@code metaDataFile}.
	 * 
	 * @param metaDataFile
	 *            the file to get the backup-file for
	 * 
	 * @return the backup-file
	 */
	protected File getBackUpFile(final File metaDataFile) {
		final File parent = metaDataFile.getParentFile();
		if (parent == null) {
			return new File(metaDataFile.getName() + backupFileExtension);
		} else {
			return new File(parent, metaDataFile.getName()
					+ backupFileExtension);
		}
	}

	@Override
	public void release() {
		if (metaDataCollection != null) {
			metaDataCollection.clear();
			metaDataCollection = null;
		}

		init = false;
	}

	@Override
	public void setConfig(final IMetaDataCacheConfig config) {
		if (init) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1000);
		} else if (config == null) {
			this.location = null;
		} else if (config instanceof FileMetaDataCacheConfig) {
			final FileMetaDataCacheConfig fcc = (FileMetaDataCacheConfig) config;

			final File cLoc = fcc.getLocation();
			this.location = cLoc == null ? null : fcc.getLocation();
		} else {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1001, config.getClass().getName());
		}
	}

	@Override
	public void initialize(final TidaModel model) {

		// if already initialized we are done
		if (this.init) {
			return;
		}

		// read the values needed
		final String modelId = model.getId();
		final File modelLocation = model.getLocation();

		// determine the location of the model
		if (metaDataCollection == null) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1002);
		} else if (this.location != null) {
			this.modelLocation = new File(this.location, modelId);
		} else if (modelLocation != null) {
			this.modelLocation = modelLocation;
			this.location = modelLocation.getParentFile();
		} else {
			this.location = getDefaultLocation();
			this.modelLocation = new File(getDefaultLocation(), modelId);
		}

		// set the location of the metaDataFile
		this.metaDataFile = new File(this.modelLocation, metaDataFileName);

		init = true;
	}

	/**
	 * Gets the default-location used by the {@code FileMetaDataCache} if no
	 * other is specified.
	 * 
	 * @return the default-location used by the {@code FileMetaDataCache}
	 */
	protected File getDefaultLocation() {
		return new File(".");
	}

	/**
	 * Gets the root-location of the caches. The {@code FileMetaDataCache}
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
}
