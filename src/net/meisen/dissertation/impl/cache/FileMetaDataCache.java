package net.meisen.dissertation.impl.cache;

import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.Map.Entry;
import java.util.concurrent.locks.ReentrantLock;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.impl.data.metadata.DescriptorMetaDataCollection;
import net.meisen.dissertation.impl.data.metadata.LoadedMetaData;
import net.meisen.dissertation.impl.data.metadata.ReadOnlyMetaDataCollection;
import net.meisen.dissertation.model.cache.IMetaDataCache;
import net.meisen.dissertation.model.cache.IMetaDataCacheConfig;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.data.metadata.IIdentifiedMetaData;
import net.meisen.dissertation.model.data.metadata.IMetaData;
import net.meisen.dissertation.model.data.metadata.IMetaDataCollection;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.resources.IByteBufferReader;
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

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	private DescriptorMetaDataCollection metaDataColl;

	private final DescriptorMetaDataCollection unpersistedMetaDataColl = new DescriptorMetaDataCollection();
	private final ReentrantLock cacheLock = new ReentrantLock();

	private boolean init = false;
	private boolean persistency = true;

	private File location;
	private File modelLocation;
	private File metaDataFile;
	private FileBasedDataOutputStream writer;

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
		if (this.location != null) {
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

		// create the directories
		final File dir = metaDataFile.getParentFile();
		if (!dir.exists() && !dir.mkdirs()) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1007, Files.getCanonicalPath(dir));
		}

		try {
			metaDataFile.createNewFile();
			writer = new FileBasedDataOutputStream(metaDataFile, true);
		} catch (final IOException e) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1005, Files.getCanonicalPath(metaDataFile));
		}

		// just create a new in memory collection and add the configured data
		readCachedMetaDataCollection();

		init = true;
	}

	@Override
	public void cacheDescriptor(final Descriptor<?, ?, ?> desc) {
		if (!init) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1003);
		}

		// modify the file if the information isn't available yet
		cacheLock.lock();
		try {
			if (!metaDataColl.contains(desc)) {
				if (persistency) {
					metaDataColl.addDescriptor(desc);
					writeDescriptor(desc);
				} else {
					unpersistedMetaDataColl.addDescriptor(desc);
				}
			}
		} finally {
			cacheLock.unlock();
		}
	}

	/**
	 * Writes the so far unpersisted values to the cache.
	 */
	protected void writeUnpersisted() {
		cacheLock.lock();

		try {
			final Iterator<IMetaData> it = unpersistedMetaDataColl.iterator();

			while (it.hasNext()) {
				final IMetaData md = it.next();
				if (md instanceof IIdentifiedMetaData) {
					final IIdentifiedMetaData imd = (IIdentifiedMetaData) md;
					final String modelId = imd.getDescriptorModelId();
					for (final Entry<Object, Object> e : imd
							.getIdentifiedValues().entrySet()) {
						metaDataColl.addMetaData(md);
						write(modelId, e.getKey(), e.getValue());
					}
				}
			}
			unpersistedMetaDataColl.clear();
		} finally {
			cacheLock.unlock();
		}
	}

	/**
	 * Writes the specified {@code Descriptor} to the cache.
	 * 
	 * @param desc
	 *            the {@code Descriptor} to be written
	 */
	protected void writeDescriptor(final Descriptor<?, ?, ?> desc) {
		write(desc.getModelId(), desc.getId(), desc.getValue());
	}

	/**
	 * Writes the specified meta-data to the cache.
	 * 
	 * @param modelId
	 *            the identifier of the {@code DescriptorModel}
	 * @param id
	 *            the identifier of the meta-data
	 * @param value
	 *            the value of the meta-data
	 */
	protected void write(final String modelId, final Object id,
			final Object value) {
		try {
			writer.write(Streams.writeAllObjects(modelId, id, value));
			writer.flush();
		} catch (final IOException e) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1006, e, modelId, id, value);
		}
	}

	/**
	 * Reads the cached meta-data.
	 */
	protected void readCachedMetaDataCollection() {

		// cleanup the meta-collection
		if (metaDataColl == null) {
			metaDataColl = new DescriptorMetaDataCollection();
		} else {
			metaDataColl.clear();
		}

		// open a reader
		final IByteBufferReader reader = Streams
				.createByteBufferReader(this.metaDataFile);

		int counter = 0;
		String modelId = null;
		Object id = null;
		Object value = null;
		while (reader.hasRemaining()) {
			final Object object = Streams.readNextObject(reader);

			// we have the modelId
			final int pos = counter % 3;
			if (pos == 0) {
				modelId = object.toString();
			}
			// the identifier
			else if (pos == 1) {
				id = object;
			}
			// the value
			else if (pos == 2) {
				value = object;

				final LoadedMetaData metaData = new LoadedMetaData(modelId);
				metaData.addValue(id, value);
				metaDataColl.addMetaData(metaData);

				// reset everything
				modelId = null;
				id = null;
				value = null;
			}

			counter++;
		}

		// release the reader again
		reader.close();

		// make sure we read valid values
		if (counter % 3 != 0) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1002, Files.getCanonicalPath(metaDataFile));
		}
	}

	@Override
	public IMetaDataCollection createMetaDataCollection() {
		if (!init) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1003);
		}

		final DescriptorMetaDataCollection collection = new DescriptorMetaDataCollection();
		cacheLock.lock();
		try {
			collection.add(this.metaDataColl);
			collection.add(this.unpersistedMetaDataColl);
		} finally {
			cacheLock.unlock();
		}

		return new ReadOnlyMetaDataCollection(collection);
	}

	/**
	 * Gets the persisted values of the cache.
	 * 
	 * @return the persisted values
	 */
	protected IMetaDataCollection getPersistedMetaDataCollection() {
		return new ReadOnlyMetaDataCollection(metaDataColl);
	}

	/**
	 * Gets the unpersisted values.
	 * 
	 * @return the unpersisted values
	 */
	protected IMetaDataCollection getUnpersistedMetaDataCollection() {
		return new ReadOnlyMetaDataCollection(unpersistedMetaDataColl);
	}

	@Override
	public void release() {
		// make sure everything is written
		writeUnpersisted();

		// close the writer
		Streams.closeIO(writer);

		// we are done here
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
	 * Gets the root-location of the cache. The {@code FileMetaDataCache}
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
	public boolean setPersistency(final boolean enable) {
		final boolean oldPersistency = this.persistency;
		this.persistency = enable;

		// nothing to do, nothing was changed
		if (oldPersistency == this.persistency) {
			// nothing to do
		}
		// persistency was enabled
		else if (oldPersistency) {
			// nothing to do
		}
		// persistency was disabled, write the not persisted once now
		else {
			writeUnpersisted();
		}

		return oldPersistency;
	}

	@Override
	public void remove() {
		if (init) {
			exceptionRegistry.throwException(FileMetaDataCacheException.class,
					1004);
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
