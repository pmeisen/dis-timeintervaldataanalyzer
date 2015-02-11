package net.meisen.dissertation.impl.cache;

import java.io.ByteArrayOutputStream;
import java.io.DataInput;
import java.io.DataOutput;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.cache.IBitmapIdCache;
import net.meisen.dissertation.model.cache.IBitmapIdCacheConfig;
import net.meisen.dissertation.model.cache.IBitmapIdCacheable;
import net.meisen.dissertation.model.cache.IBitmapIdOwner;
import net.meisen.dissertation.model.cache.IReleaseMechanismCache;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.genmisc.types.Streams;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Implementation of a {@code Cache}, which uses the file-system to persist
 * entities related to {@code BitmapId} instances and loads only a specific
 * amount of those instances in memory. <br/>
 * <br/>
 * <b>Note:</b><br/>
 * The implementation is thread-safe.
 * 
 * @author pmeisen
 * @param <T>
 *            the {@code Cacheable} to be cached
 * 
 * @see IBitmapIdCacheable
 * 
 */
public abstract class BaseFileBitmapIdCache<T extends IBitmapIdCacheable>
		implements IBitmapIdCache<T>, IReleaseMechanismCache<BitmapId<?>, T> {
	private final static Logger LOG = LoggerFactory
			.getLogger(BaseFileBitmapIdCache.class);

	private static final int idxBitmapIdSizeInBytes = BitmapId
			.getMaxBytesLength();
	private static final int idxLineSizeInBytes = idxBitmapIdSizeInBytes
			+ IndexEntry.idxEntrySizeInBytes;

	/**
	 * The {@code ExceptionRegistry}.
	 */
	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	protected IExceptionRegistry exceptionRegistry;

	private final ICachingStrategy strategy;

	private final Map<BitmapId<?>, IBitmapIdOwner> owners;
	private final Map<BitmapId<?>, T> cache;
	private final Map<BitmapId<?>, IndexEntry> idx;

	private final ReentrantReadWriteLock ownersLock;
	private final ReentrantReadWriteLock cacheLock;
	private final ReentrantReadWriteLock idxLock;
	private final ReentrantReadWriteLock fileLock;

	private final List<RandomAccessFile> fileReaders;
	private final List<FileBasedDataOutputStream> fileWriters;
	private final List<File> files;

	private final Set<BitmapId<?>> notPersisted;

	private boolean init;
	private boolean persistency;
	private File location;
	private File modelLocation;
	private int cacheSize;
	private int maxFileSizeInByte;
	private double cacheCleaningFactor;

	private int curFileNumber = -1;

	private RandomAccessFile idxTableWriter;
	private File idxTableFile;

	/**
	 * Default constructor.
	 */
	public BaseFileBitmapIdCache() {
		this.strategy = new RandomCachingStrategy();

		this.notPersisted = new HashSet<BitmapId<?>>();

		this.ownersLock = new ReentrantReadWriteLock();
		this.cacheLock = new ReentrantReadWriteLock();
		this.idxLock = new ReentrantReadWriteLock();
		this.fileLock = new ReentrantReadWriteLock();

		this.location = null;
		this.cacheSize = getDefaultCacheSize();
		this.cacheCleaningFactor = getDefaultCacheCleaningFactor();

		this.owners = new HashMap<BitmapId<?>, IBitmapIdOwner>();
		this.idx = new HashMap<BitmapId<?>, IndexEntry>();
		this.cache = new HashMap<BitmapId<?>, T>(this.cacheSize);

		this.fileReaders = new ArrayList<RandomAccessFile>();
		this.fileWriters = new ArrayList<FileBasedDataOutputStream>();
		this.files = new ArrayList<File>();

		this.persistency = true;
		this.init = false;

		// use the default configuration
		setConfig(null);
	}

	@Override
	public synchronized void initialize(final TidaModel model) {

		// if already initialized we are done
		if (this.init) {
			return;
		}

		// read the values needed
		final String modelId = model.getId();
		final File modelLocation = model.getLocation();

		// determine the location of the model
		if (this.location != null) {
			this.modelLocation = new File(location, modelId);
		} else if (modelLocation != null) {
			this.modelLocation = modelLocation;
			this.location = modelLocation.getParentFile();
		} else {
			this.location = getDefaultLocation();
			this.modelLocation = new File(getDefaultLocation(), modelId);
		}

		// create the index file
		this.idxTableFile = new File(this.modelLocation, getIndexFileName());

		// decide if data has to be loaded or an instance has to be created
		if (this.idxTableFile.exists()) {
			loadInstance();
		} else {
			createInstance();
		}

		// initialization is finished
		this.init = true;
	}

	/**
	 * Creates the needed files for the {@code BitmapIdCache} at the currently
	 * defined {@code modelLocation}.
	 * 
	 * @throws BaseFileBitmapIdCacheException
	 *             if the {@code modelLocation} is {@code null}, if the
	 *             {@code modelLocation} already exists, if the
	 *             {@code modelLocation} could not be created, or if an
	 *             {@code IOException} occurred during initialization
	 * 
	 * @see IOException
	 */
	protected void createInstance() throws BaseFileBitmapIdCacheException {

		// make sure we can create an instance
		if (modelLocation == null) {
			exceptionRegistry.throwException(getExceptionClass(1005), 1005);
		} else if (idxTableFile.exists()) {
			exceptionRegistry.throwException(getExceptionClass(1004), 1004,
					modelLocation);
		} else if (!modelLocation.exists() && !modelLocation.mkdirs()) {
			exceptionRegistry.throwException(getExceptionClass(1002), 1002,
					modelLocation);
		} else if (LOG.isDebugEnabled()) {
			LOG.debug("Creating cache-file at '" + modelLocation + "' ("
					+ getClass().getSimpleName() + ").");
		}

		try {
			// create the index file
			idxTableFile.createNewFile();

			// create the index writer
			idxTableWriter = new RandomAccessFile(idxTableFile, "rws");
		} catch (final IOException e) {
			exceptionRegistry.throwException(getExceptionClass(1006), 1006, e,
					idxTableFile);
		}
	}

	/**
	 * Loads an instance of a {@code Cache} from the hard-drive.
	 */
	protected void loadInstance() {

		// make sure we can load an instance
		if (modelLocation == null) {
			exceptionRegistry.throwException(getExceptionClass(1005), 1005);
		} else if (!modelLocation.exists()) {
			exceptionRegistry.throwException(getExceptionClass(1007), 1007,
					modelLocation);
		} else if (!idxTableFile.exists()) {
			exceptionRegistry.throwException(getExceptionClass(1018), 1008,
					idxTableFile);
		} else if (!idxTableFile.isFile()) {
			exceptionRegistry.throwException(getExceptionClass(1009), 1009,
					idxTableFile);
		} else if (LOG.isDebugEnabled()) {
			LOG.debug("Loading cache-file from '" + modelLocation + "' ("
					+ getClass().getSimpleName() + ").");
		}

		try {

			// create the index writer
			idxTableWriter = new RandomAccessFile(idxTableFile, "rws");

			// read the index into memory
			final FileInputStream f = new FileInputStream(idxTableFile);
			final FileChannel ch = f.getChannel();
			final ByteBuffer bb = ByteBuffer.allocate(idxLineSizeInBytes);

			int nRead;
			while ((nRead = ch.read(bb)) != -1) {
				if (nRead == 0) {
					continue;
				}

				// get the line and check the size
				final byte[] line = bb.array();
				if (line.length != idxLineSizeInBytes) {
					exceptionRegistry.throwException(getExceptionClass(1011),
							1011, line.length, idxLineSizeInBytes);
				}

				// read the BitmapId and IndexEntry from the line
				@SuppressWarnings("rawtypes")
				final BitmapId bitmapId = new BitmapId(Arrays.copyOfRange(line,
						0, idxBitmapIdSizeInBytes));
				final IndexEntry entry = new IndexEntry(Arrays.copyOfRange(
						line, idxBitmapIdSizeInBytes, idxLineSizeInBytes));

				// just add the values to the index, nothing is cached
				this.idx.put(bitmapId, entry);

				// clear the buffer again
				bb.clear();
			}

			f.close();
		} catch (final IOException e) {
			exceptionRegistry.throwException(getExceptionClass(1012), 1012, e,
					idxTableFile);
		}

		// validate the read files
		for (final IndexEntry entry : this.idx.values()) {
			final int fileNr = entry.getFileNumber();
			final File file = createFileName(fileNr);

			if (files.contains(file)) {
				continue;
			} else {

				// add the bitmapFile
				_addDataFile(file, fileNr);
			}
		}

		// set the number of the file
		curFileNumber = files.size() - 1;
	}

	/**
	 * Adds the specified {@code dataFile} to the files of the cache using the
	 * specified {@code expectedNumber}.
	 * 
	 * @param dataFile
	 *            the file to add
	 * @param expectedNumber
	 *            the number to add the file under
	 * 
	 * @throws BaseFileBitmapIdCacheException
	 *             if the specified {@code dataFile} doesn't exist, isn't a file
	 *             or if the {@code expectedNumber} is used by another
	 *             {@code file}
	 */
	protected void _addDataFile(final File dataFile, final int expectedNumber)
			throws BaseFileBitmapIdCacheException {

		if (!dataFile.exists() || !dataFile.isFile()) {
			exceptionRegistry.throwException(getExceptionClass(1013), 1013,
					dataFile);
		}

		// bitmap reader and writer
		final RandomAccessFile reader;
		final FileBasedDataOutputStream writer;
		try {
			reader = new RandomAccessFile(dataFile, "r");
			writer = new FileBasedDataOutputStream(dataFile, true);
		} catch (final IOException e) {
			exceptionRegistry.throwException(getExceptionClass(1012), 1012, e,
					dataFile);
			return;
		}

		// add everything
		if (expectedNumber < files.size()) {
			if (files.set(expectedNumber, dataFile) != null
					|| fileReaders.set(expectedNumber, reader) != null
					|| fileWriters.set(expectedNumber, writer) != null) {

				// close the reader and writer cause they will not be used
				Streams.closeIO(reader);
				Streams.closeIO(writer);

				exceptionRegistry.throwException(getExceptionClass(1014), 1014,
						dataFile, expectedNumber);
			}
		} else {

			// add some empty placeholders
			if (expectedNumber > files.size()) {
				for (int i = files.size(); i < expectedNumber; i++) {
					files.add(null);
					fileReaders.add(null);
					fileWriters.add(null);
				}
			}

			// add the dataFile and it's reader and writer
			files.add(dataFile);
			fileReaders.add(reader);
			fileWriters.add(writer);
		}
	}

	/**
	 * Creates a new data-file and adds it to the cache using
	 * {@link #_addDataFile(File, int)}.
	 * 
	 * @throws BaseFileBitmapIdCacheException
	 *             if {@link #_addDataFile(File, int)} threw an exception or if
	 *             the file could not be created
	 */
	protected void _createNewDataFile() throws BaseFileBitmapIdCacheException {

		// the current file has to be increased by one
		curFileNumber++;

		// create the file
		final File file = createFileName(curFileNumber);

		try {
			file.createNewFile();
		} catch (final IOException e) {
			exceptionRegistry.throwException(getExceptionClass(1006), 1006, e,
					file);
		}

		// bitmap reader and writer
		_addDataFile(file, curFileNumber);

		if (LOG.isTraceEnabled()) {
			LOG.trace("Created new cache-file with fileNumber '"
					+ curFileNumber + "' at '" + file + "' ("
					+ getClass().getSimpleName() + ").");
		}
	}

	/**
	 * Creates the name of the bitmap-file to be or already in use with the
	 * specified {@code nr}.
	 * 
	 * @param nr
	 *            the number of the file to create the name for
	 * 
	 * @return the file to be used
	 */
	protected File createFileName(final int nr) {
		final String fileName = getFileName(nr);
		return new File(getModelLocation(), fileName);
	}

	/**
	 * Writes the specified {@code bitmap} to the file and indexes it within the
	 * index-file.
	 * 
	 * @param cacheable
	 *            the {@code Cacheable} instance to be persisted
	 * 
	 * @return the entry the bitmap can be found under within the index
	 * 
	 * @throws BaseFileBitmapIdCacheException
	 *             if the bitmap's serialization is larger than the allowed
	 *             maximal file-size, or if the writing to the hard-drive fails
	 * 
	 * @see IBitmapIdCacheable
	 */
	protected IndexEntry write(final T cacheable)
			throws BaseFileBitmapIdCacheException {

		// get the byte representation
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final DataOutputStream w = new DataOutputStream(baos);
		try {
			writeToOutput(cacheable, w);
			w.flush();
		} catch (final IOException e) {
			exceptionRegistry.throwException(getExceptionClass(1015), 1015, e,
					cacheable);
			return null;
		} finally {
			Streams.closeIO(w);
			Streams.closeIO(baos);
		}
		final byte[] result = baos.toByteArray();
		final int resSize = result.length;

		// check if the result can fit into a file
		if (resSize > getMaxFileSizeInByte()) {
			exceptionRegistry.throwException(getExceptionClass(1016), 1016,
					resSize, cacheable, getMaxFileSizeInByte());
		}

		int priorSize = -1;
		int fileNr = -1;
		boolean written = false;

		// write the byte-array to the writer
		if (curFileNumber > -1) {
			fileLock.readLock().lock();
			try {
				final FileBasedDataOutputStream writer = fileWriters
						.get(curFileNumber);

				synchronized (writer) {
					priorSize = writer.getCurrentPosition();
					fileNr = curFileNumber;

					if (priorSize + resSize > getMaxFileSizeInByte()) {
						written = false;
					} else {
						writer.write(result);
						writer.flush();

						written = true;
					}
				}
			} catch (final IOException e) {
				exceptionRegistry.throwException(getExceptionClass(1015), 1015,
						e, cacheable);
				return null;
			} finally {
				fileLock.readLock().unlock();
			}
		}

		if (written) {

			// create the entry the bitmap is stored at
			return new IndexEntry(resSize, priorSize, fileNr);
		} else {

			// create a new file
			fileLock.writeLock().lock();
			try {

				// check if no other thread created a new file
				if (fileNr == curFileNumber) {
					_createNewDataFile();
				}
			} finally {
				fileLock.writeLock().unlock();
			}

			// try again to write the file
			return write(cacheable);
		}
	}

	/**
	 * Writes the {@code cacheable} to the specified {@code out}.
	 * 
	 * @param cacheable
	 *            the {@code Cacheable} to be written
	 * @param out
	 *            the instance to write the {@code Cacheable} to
	 * 
	 * @throws IOException
	 *             if the instance cannot be written
	 */
	protected abstract void writeToOutput(final T cacheable,
			final DataOutput out) throws IOException;

	/**
	 * Reads the {@code Cacheable} for the specified {@code IndexEntry} from the
	 * hard-drive.
	 * 
	 * @param e
	 *            the entry to read the {@code Cacheable} for
	 * 
	 * @return the read {@code Cacheable}
	 * 
	 * @throws BaseFileBitmapIdCacheException
	 *             if the {@code Bitmap} cannot be read from the hard-drive
	 * 
	 * @see IBitmapIdCacheable
	 */
	protected T read(final IndexEntry e) throws BaseFileBitmapIdCacheException {
		final T cacheable;

		fileLock.readLock().lock();
		try {
			final RandomAccessFile reader = fileReaders.get(e.getFileNumber());

			synchronized (reader) {
				reader.seek(e.getFilePosition());
				cacheable = createFromInput(reader);
			}
		} catch (final IOException ex) {
			exceptionRegistry.throwException(getExceptionClass(1017), 1017, ex,
					e);
			return null;
		} finally {
			fileLock.readLock().unlock();
		}

		return cacheable;
	}

	/**
	 * Creates a new instance of the {@code IBitmapIdCacheable} from the
	 * specified {@code in}.
	 * 
	 * @param in
	 *            the {@code DataInput} to read from
	 * @return the created instance
	 * 
	 * @throws IOException
	 *             if the data cannot be read or the instance cannot be created
	 */
	protected abstract T createFromInput(final DataInput in) throws IOException;

	/**
	 * Creates an empty, new instance of the {@code IBitmapIdCacheable}.
	 * 
	 * @return an empty, new instance of the {@code IBitmapIdCacheable}
	 */
	protected abstract T createNewInstance();

	/**
	 * Get the name of the index-file.
	 * 
	 * @return the name of the index-file
	 */
	protected abstract String getIndexFileName();

	/**
	 * Gets the file-name used for the file with the specified {@code nr}.
	 * 
	 * @param nr
	 *            the number of the file
	 * 
	 * @return the file-name used for the data
	 */
	protected abstract String getFileName(final int nr);

	/**
	 * Gets the {@code Cacheable} with the specified {@code bitmapId} from the
	 * cache. This method never returns {@code null} instead a new
	 * {@code Cacheable} is created using {@link #createNewInstance()} if no
	 * {@code Cacheable} is available with the specified {@code BitmapId}.
	 * 
	 * @param bitmapId
	 *            the {@code BitmapId} of the {@code Cacheable} to be retrieved
	 * 
	 * @return the {@code Cacheable} for the specified {@code BitmapId}
	 * 
	 * @see IBitmapIdCacheable
	 */
	public T getCacheable(final BitmapId<?> bitmapId) {
		if (!init) {
			exceptionRegistry.throwException(getExceptionClass(1010), 1010);
		}

		boolean doCache = false;
		T cacheable;
		try {
			this.cacheLock.readLock().lock();
			this.idxLock.readLock().lock();

			// check the cache
			cacheable = _getFromCache(bitmapId);

			// if we couldn't find it get it from the hard-drive
			if (cacheable == null) {
				cacheable = _getFromIndex(idx.get(bitmapId));
				doCache = true;
			} else {
				if (LOG.isTraceEnabled()) {
					LOG.trace("Get " + bitmapId + " from cache.");
				}
			}

			// if we still don't have a bitmap create an empty one
			if (cacheable == null) {
				cacheable = createNewInstance();
				doCache = true;

				if (LOG.isTraceEnabled()) {
					LOG.trace("Created new instance for " + bitmapId + ".");
				}
			} else {
				if (LOG.isTraceEnabled()) {
					LOG.trace("Loaded instance for " + bitmapId + " from disk.");
				}
			}
		} finally {
			this.idxLock.readLock().unlock();
			this.cacheLock.readLock().unlock();
		}

		/*
		 * Cache the retrieved bitmap if needed. The cache might contain a newer
		 * version by now (the release of the read and the acquiring of the
		 * write gave other threads the chance to modify the cache). Therefore
		 * we have to check again.
		 */
		if (doCache) {
			this.cacheLock.writeLock().lock();
			try {

				// the cache might have been updated
				if (!cache.containsKey(bitmapId)) {
					if (_cache(bitmapId, cacheable)) {

						if (isPersistencyEnabled()) {
							_organizeCache();
						} else {
							this.idxLock.writeLock().lock();
							try {
								_organizeCache();
							} finally {
								this.idxLock.writeLock().unlock();
							}
						}
					}
				}
			} finally {
				this.cacheLock.writeLock().unlock();
			}

		}

		// mark the bitmap as used
		getStrategy().usedBitmap(bitmapId);

		return cacheable;
	}

	/**
	 * The type of the exception to be thrown via the {@link #exceptionRegistry}
	 * for the specified {@code errorNr}.
	 * 
	 * @param errorNr
	 *            the number to determine the {@code Class} for
	 * 
	 * @return the class of the exception to be thrown
	 */
	protected Class<? extends BaseFileBitmapIdCacheException> getExceptionClass(
			final int errorNr) {
		return BaseFileBitmapIdCacheException.class;
	}

	/**
	 * Caches the {@code Cacheable} with the specified {@code bitmapId}.
	 * 
	 * @param bitmapId
	 *            the identifier of the {@code Cacheable} to be cached
	 * @param cacheable
	 *            the {@code Cacheable} to be cached
	 * 
	 * @return {@code true} if the cache has to be organized, otherwise
	 *         {@code false}
	 * 
	 * @see IBitmapIdCacheable
	 */
	protected boolean _cache(final BitmapId<?> bitmapId, final T cacheable) {
		boolean needReorganization = false;

		// add the value
		if (cache.put(bitmapId, cacheable) == null) {

			/*
			 * check if the cache size is exceeded and if needed, trigger a
			 * re-organization of the cache
			 */
			if (cache.size() > cacheSize) {
				needReorganization = true;
			}

			// register the bitmap because it wasn't registered yet
			getStrategy().registerBitmap(bitmapId);
		}

		return needReorganization;
	}

	/**
	 * Reorganizes the cache, by removing not used or 'old' entries.
	 */
	protected void _organizeCache() {

		// determine the cleaning size, i.e. how many elements to be removed
		final int cacheCleaningSize = Math.min(cacheSize,
				Math.max(1, (int) (cacheSize * getCacheCleaningFactor())));

		// determine which bitmaps to be released
		final List<BitmapId<?>> removedIds = getStrategy()
				.determineRemovables(cache.keySet(), cacheCleaningSize, true);

		// log what is removed
		if (LOG.isTraceEnabled()) {
			LOG.trace("Organizing cache (size: " + cache.size()
					+ ") by removing '" + removedIds.size() + "' ("
					+ getClass().getSimpleName() + ").");
		}

		// persist the once that will be removed
		if (!isPersistencyEnabled()) {
			_bulkWrite(removedIds);
		}

		// remove the bitmaps from the cache and the owner
		this.ownersLock.readLock().lock();
		try {
			for (final BitmapId<?> removedId : removedIds) {

				// remove the identifier from the cache
				final T cacheable = cache.remove(removedId);

				// release the bitmap from the owner
				final IBitmapIdOwner owner = this.owners.get(removedId);
				if (owner != null) {
					owner.release(cacheable);
				}
			}
		} finally {
			this.ownersLock.readLock().unlock();
		}

		if (LOG.isTraceEnabled()) {
			LOG.trace("Finalized organizing of cache (new size: "
					+ cache.size() + ", " + getClass().getSimpleName() + ").");
		}
	}

	/**
	 * Checks if the {@code expectedNewEntry} is newer than the {@code curEntry}
	 * . The method returns {@code true} if {@code expectedNewEntry} is newer,
	 * otherwise {@code false} is returned.
	 * 
	 * @param curEntry
	 *            the entry to compare with
	 * @param expectedNewEntry
	 *            the entry to be expected newer
	 * 
	 * @return {@code true} if {@code expectedNewEntry} is newer, otherwise
	 *         {@code false}
	 */
	protected boolean isNewerEntry(final IndexEntry curEntry,
			final IndexEntry expectedNewEntry) {

		if (curEntry == null) {
			return true;
		} else if (curEntry.getFileNumber() < expectedNewEntry.getFileNumber()) {
			return true;
		} else if (curEntry.getFileNumber() > expectedNewEntry.getFileNumber()) {
			return false;
		} else {
			return curEntry.getFilePosition() < expectedNewEntry
					.getFilePosition();
		}
	}

	/**
	 * Indexes the specified {@code entry} for the specified {@code bitmapId}.
	 * 
	 * @param bitmapId
	 *            the identifier of the bitmap to be indexed
	 * @param entry
	 *            the entry the bitmap should be indexed under
	 * 
	 * @return {@code true} if the bitmap refreshed (i.e. was newer than the
	 *         internally cached one), otherwise {@code false}
	 * 
	 * @throws BaseFileBitmapIdCacheException
	 *             if the {@code entry} is already indexed (i.e.
	 *             {@link IndexEntry#getIndexFileNumber()} returns a value
	 *             unequal to {@code -1}, if an io-problem occurred, or if the
	 *             entry does not have any {@code indexFileNumber} specified
	 */
	protected boolean _index(final BitmapId<?> bitmapId, final IndexEntry entry)
			throws BaseFileBitmapIdCacheException {

		if (entry.getIndexFileNumber() != -1) {
			exceptionRegistry.throwException(getExceptionClass(1018), 1018,
					entry);
		}

		/*
		 * check if we already have a newer entry cached, this might have
		 * happened because of asynchronous events
		 */
		final IndexEntry curEntry = this.idx.get(bitmapId);
		if (!isNewerEntry(curEntry, entry)) {
			return false;
		} else {
			final byte[] bytes;
			final int idxFilePos;

			// write the bitmap with the
			if (curEntry == null) {
				entry.setIndexFileNumber(this.idx.size());

				// determine the position
				idxFilePos = entry.getIndexFileNumber() * idxLineSizeInBytes;
				bytes = generateIndexLine(bitmapId, entry);
			} else {
				entry.setIndexFileNumber(curEntry.getIndexFileNumber());

				// determine the position
				idxFilePos = entry.getIndexFileNumber() * idxLineSizeInBytes
						+ idxBitmapIdSizeInBytes;
				bytes = entry.bytes();
			}

			// persist it to the file
			synchronized (idxTableWriter) {
				try {
					idxTableWriter.seek(idxFilePos);
					idxTableWriter.write(bytes);
				} catch (final IOException e) {
					exceptionRegistry.throwException(getExceptionClass(1018),
							1018, e, entry, idxTableFile);
				}
			}

			// index the value in memory
			this.idx.put(bitmapId, entry);

			return true;
		}
	}

	/**
	 * Generates the index-line (i.e. the entry within the index-file) for the
	 * specified {@code entry} and the specified {@code bitmapId}.
	 * 
	 * @param bitmapId
	 *            the identifier of the bitmap
	 * @param entry
	 *            the entry the {@code bitmapId} belongs to
	 * 
	 * @return the generated index-line as bytes
	 * 
	 * @throws BaseFileBitmapIdCacheException
	 *             if the length of the bitmapId representation is invalid or if
	 *             the length of the entry is invalid
	 */
	protected byte[] generateIndexLine(final BitmapId<?> bitmapId,
			final IndexEntry entry) throws BaseFileBitmapIdCacheException {
		final byte[] bytesId = bitmapId.bytes();
		final byte[] bytesEntry = entry.bytes();

		if (bytesId.length > idxBitmapIdSizeInBytes) {
			exceptionRegistry.throwException(getExceptionClass(1020), 1020,
					bitmapId, bytesId.length, idxBitmapIdSizeInBytes);
			return null;
		} else if (bytesEntry.length != IndexEntry.idxEntrySizeInBytes) {
			exceptionRegistry.throwException(getExceptionClass(1021), 1021,
					entry, bytesEntry.length, IndexEntry.idxEntrySizeInBytes);
			return null;
		} else {
			final byte[] filledBytesId = new byte[idxBitmapIdSizeInBytes];
			System.arraycopy(bytesId, 0, filledBytesId, 0, bytesId.length);

			return Streams.combineBytes(filledBytesId, bytesEntry);
		}
	}

	/**
	 * Gets the {@code Bitmap} for the specified {@code bitmapId} from the
	 * cache.
	 * 
	 * @param bitmapId
	 *            the identifier of the {@code Bitmap} to be retrieved
	 * 
	 * @return the cached {@code Bitmap} instance, or {@code null} if no
	 *         instance is currently cached
	 */
	protected T _getFromCache(final BitmapId<?> bitmapId) {
		return cache.get(bitmapId);
	}

	/**
	 * Gets the {@code Bitmap} for the specified {@code entry} from the index.
	 * 
	 * @param entry
	 *            the entry of the {@code Bitmap} to be retrieved
	 * 
	 * @return the indexed {@code Bitmap} instance, or {@code null} if no such
	 *         instance is indexed
	 */
	protected T _getFromIndex(final IndexEntry entry) {
		if (entry == null) {
			return null;
		} else {
			return read(entry);
		}
	}

	@Override
	public void cache(final BitmapId<?> bitmapId, final T cacheable) {
		if (!init) {
			exceptionRegistry.throwException(getExceptionClass(1010), 1010);
		}

		// check if the data should be persisted
		if (isPersistencyEnabled()) {
			final IndexEntry entry = write(cacheable);

			this.cacheLock.writeLock().lock();
			this.idxLock.writeLock().lock();
			try {
				if (_index(bitmapId, entry)) {
					if (_cache(bitmapId, cacheable)) {
						_organizeCache();
					}
				}
			} finally {
				this.idxLock.writeLock().unlock();
				this.cacheLock.writeLock().unlock();
			}
		} else {
			this.cacheLock.writeLock().lock();
			try {
				notPersisted.add(bitmapId);

				// update the cache
				if (_cache(bitmapId, cacheable)) {

					// if needed reorganize it
					this.idxLock.writeLock().lock();
					try {
						_organizeCache();
					} finally {
						this.idxLock.writeLock().unlock();
					}
				}
			} finally {
				this.cacheLock.writeLock().unlock();
			}
		}
	}

	@Override
	public void registerOwner(final IBitmapIdOwner owner) {
		if (!init) {
			exceptionRegistry.throwException(getExceptionClass(1010), 1010);
		}

		this.ownersLock.writeLock().lock();

		try {
			if (owners.put(owner.getBitmapId(), owner) != null) {
				if (LOG.isWarnEnabled()) {
					LOG.warn("The owner of '" + owner.getBitmapId()
							+ "' was changed (" + getClass().getSimpleName()
							+ ").");
				}
			}
		} finally {
			this.ownersLock.writeLock().unlock();
		}
	}

	/**
	 * Gets the root-location of the caches. The {@code FileBitmapCache}
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
		if (init) {
			exceptionRegistry.throwException(getExceptionClass(1000), 1022);
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

	@Override
	public void setConfig(final IBitmapIdCacheConfig config) {
		if (init) {
			exceptionRegistry.throwException(getExceptionClass(1000), 1000);
		} else if (config == null) {
			this.location = null;
			this.cacheSize = getDefaultCacheSize();
			this.maxFileSizeInByte = getDefaultMaxFileSizeInByte();
		} else if (config instanceof FileBitmapIdCacheConfig == false) {
			exceptionRegistry.throwException(getExceptionClass(1001), 1001,
					config.getClass().getName());
		} else {
			final FileBitmapIdCacheConfig cc = (FileBitmapIdCacheConfig) config;

			final File cLoc = cc.getLocation();
			this.location = cLoc == null ? null : cc.getLocation();

			final Integer cSize = cc.getCacheSize();
			this.cacheSize = cSize == null ? getDefaultCacheSize() : cSize;

			final Double cFactor = cc.getCacheCleaningFactor();
			this.cacheCleaningFactor = cFactor == null ? getDefaultCacheCleaningFactor()
					: cFactor;

			final Integer cMaxFileSizeInByte = cc.getMaxFileSizeInByte();
			this.maxFileSizeInByte = cMaxFileSizeInByte == null ? getDefaultMaxFileSizeInByte()
					: cMaxFileSizeInByte;
		}
	}

	/**
	 * Gets the {@code CachingStrategy} to be used.
	 * 
	 * @return the {@code CachingStrategy} to be used.
	 */
	protected ICachingStrategy getStrategy() {
		return strategy;
	}

	/**
	 * Gets the default-location used by the {@code FileBitmapCache} if no other
	 * is specified.
	 * 
	 * @return the default-location used by the {@code FileBitmapCache}
	 */
	protected File getDefaultLocation() {
		return new File(".");
	}

	/**
	 * Gets the default max size of the cache.
	 * 
	 * @return the default max size of the cache
	 */
	protected int getDefaultCacheSize() {
		return 100000;
	}

	/**
	 * Gets the default value used as cleaning-factor (default is 20%).
	 * 
	 * @return the default cleaning-factor
	 */
	protected double getDefaultCacheCleaningFactor() {
		return 0.2;
	}

	/**
	 * Gets the default value used as maximal file-size (default
	 * {@code Integer.MAX_VALUE}).
	 * 
	 * @return the default value used as maximal file-size
	 */
	protected int getDefaultMaxFileSizeInByte() {
		return Integer.MAX_VALUE;
	}

	@Override
	public synchronized void release() {
		if (!this.init) {
			return;
		} else if (LOG.isDebugEnabled()) {
			LOG.debug("Releasing '" + getClass().getSimpleName() + "' at '"
					+ getModelLocation() + "' (" + getClass().getSimpleName()
					+ ").");
		}

		/*
		 * set the initialization to be false, every call to the public methods
		 * is not permitted now, initialization is blocked because this method
		 * is synchronized
		 */
		this.init = false;

		// write the not persisted data
		if (!isPersistencyEnabled()) {
			this.cacheLock.writeLock().lock();
			this.idxLock.writeLock().lock();
			try {
				_bulkWrite(this.cache.keySet());
			} finally {
				this.idxLock.writeLock().unlock();
				this.cacheLock.writeLock().unlock();
			}
		}

		// release the index writer
		synchronized (idxTableWriter) {

			// release the indexTableWriter
			if (idxTableWriter != null) {
				try {
					idxTableWriter.close();
				} catch (final IOException e) {
					exceptionRegistry.throwException(getExceptionClass(1003),
							1003, e, idxTableFile);
				}
			}
		}

		// make sure we are the only once using the reader and writer
		fileLock.writeLock().lock();
		try {

			// release the reader
			int fileCounter = 0;
			for (final RandomAccessFile raf : fileReaders) {
				try {
					raf.close();
				} catch (final IOException e) {
					exceptionRegistry.throwException(getExceptionClass(1003),
							1003, e, files.get(fileCounter));
				}

				fileCounter++;
			}

			// release the writer
			fileCounter = 0;
			for (final DataOutputStream dos : fileWriters) {
				try {
					dos.close();
				} catch (final IOException e) {
					exceptionRegistry.throwException(getExceptionClass(1003),
							1003, e, files.get(fileCounter));
				}

				fileCounter++;
			}

			// reset the files
			fileReaders.clear();
			fileWriters.clear();
			files.clear();

			// reset the number
			curFileNumber = -1;
		} finally {
			fileLock.writeLock().unlock();
		}

		// empty the arrays
		this.cacheLock.writeLock().lock();
		this.ownersLock.writeLock().lock();
		this.idxLock.writeLock().lock();

		this.cache.clear();
		this.idx.clear();
		this.owners.clear();

		this.idxLock.writeLock().unlock();
		this.ownersLock.writeLock().unlock();
		this.cacheLock.writeLock().unlock();
	}

	/**
	 * Checks if an instance with the specified {@code id} is cached.
	 * 
	 * @param id
	 *            the identifier of the bitmap to be checked
	 * 
	 * @return {@code true} if the bitmap is cached, otherwise {@code false}
	 */
	public boolean isCached(final BitmapId<?> id) {
		this.cacheLock.readLock().lock();
		try {
			return this.cache.containsKey(id);
		} finally {
			this.cacheLock.readLock().unlock();
		}
	}

	/**
	 * Gets the size of the cached instances.
	 * 
	 * @return the size of the cached instances
	 */
	public int getCacheSize() {
		final int size;

		this.cacheLock.readLock().lock();
		try {
			size = this.cache.size();
		} finally {
			this.cacheLock.readLock().unlock();
		}

		return size;
	}

	/**
	 * Gets the maximal amount of instances to be cached.
	 * 
	 * @return the maximal amount of instances to be cached
	 */
	public int getMaxCacheSize() {
		return cacheSize;
	}

	/**
	 * Clears the cache completely.
	 */
	public void clearCache() {
		this.cacheLock.writeLock().lock();
		try {

			// the data might not be persisted yet, so we have to do it know
			if (!isPersistencyEnabled()) {
				this.idxLock.writeLock().lock();
				try {
					_bulkWrite(this.cache.keySet());
				} finally {
					this.idxLock.writeLock().unlock();
				}
			}

			// clear the cache
			this.cache.clear();
		} finally {
			this.cacheLock.writeLock().unlock();
		}
	}

	/**
	 * Gets the currently used cleaning-factor.
	 * 
	 * @return the currently used cleaning-factor
	 */
	public double getCacheCleaningFactor() {
		return cacheCleaningFactor;
	}

	/**
	 * Gets the maximal file-size in bytes.
	 * 
	 * @return the maximal file-size in bytes
	 */
	public int getMaxFileSizeInByte() {
		return maxFileSizeInByte;
	}

	/**
	 * Gets the amount of currently used bitmap-files.
	 * 
	 * @return the amount of currently used bitmap-files
	 */
	public int getNumberOfFiles() {
		return curFileNumber + 1;
	}

	@Override
	public synchronized boolean setPersistency(final boolean enable) {

		/*
		 * Make sure that there is no other thread working with the cache
		 * currently.
		 */
		this.cacheLock.writeLock().lock();
		this.idxLock.writeLock().lock();
		try {
			final boolean oldPersistency = this.persistency;
			this.persistency = enable;

			// nothing to do, nothing was changed
			if (oldPersistency == this.persistency) {
				// nothing
			}
			/*
			 * Persistency was enabled and is disabled now.
			 */
			else if (oldPersistency) {
				notPersisted.clear();
			}
			/*
			 * Persistency was disabled and is enabled now, write the not
			 * persisted once.
			 */
			else {
				_bulkWrite(null);
			}

			return oldPersistency;
		} finally {
			this.idxLock.writeLock().unlock();
			this.cacheLock.writeLock().unlock();
		}
	}

	/**
	 * Persists the {@code notPersisted} data in a bulk-mode, whereby the data
	 * written can be filtered by the specified {@code filterIds}.
	 * 
	 * @param filterIds
	 *            the {@code BitmapIds} to filter the {@code notPersisted} by,
	 *            i.e. the method will persist the intersection between the
	 *            {@code notPersisted} and the {@code filterIds}, can be
	 *            {@code null} if all {@code notPersisted} should be persisted
	 */
	protected void _bulkWrite(final Collection<BitmapId<?>> filterIds) {

		// define the BitmapIds which really should be written
		final Set<BitmapId<?>> toBeWritten = new HashSet<BitmapId<?>>(
				notPersisted);
		if (filterIds != null) {
			toBeWritten.retainAll(filterIds);
		}

		// log the writing
		if (LOG.isDebugEnabled()) {
			if (filterIds == null) {
				LOG.debug("Persisting " + toBeWritten.size()
						+ " cacheables during bulk write ("
						+ getClass().getSimpleName() + ").");
			} else {
				LOG.debug("Persisting " + toBeWritten.size()
						+ " cacheables during bulk write ("
						+ getClass().getSimpleName() + ")e.");
			}
		}

		// write the once to be written
		for (final BitmapId<?> id : toBeWritten) {

			// get the instance to be persisted
			final T cacheable = _getFromCache(id);

			// persist it
			final IndexEntry entry = write(cacheable);
			_index(id, entry);

			// remove the one from the once that are not persisted
			notPersisted.remove(id);
		}

		// log the success
		if (LOG.isDebugEnabled()) {
			LOG.debug("Finished bulk write of " + toBeWritten.size()
					+ " cacheables (" + getClass().getSimpleName() + ").");
		}
	}

	/**
	 * Checks if the persistency is currently enabled.
	 * 
	 * @return {@code true} if persistency is enabled, otherwise {@code false}
	 */
	public boolean isPersistencyEnabled() {
		return persistency;
	}

	/**
	 * Gets a collection of all the cached {@code BitmapId} instances.
	 * 
	 * @return a collection of all the cached {@code BitmapId} instances
	 */
	public Collection<BitmapId<?>> getBitmapIdentifiers() {
		final List<BitmapId<?>> keys;

		idxLock.readLock().lock();
		try {
			keys = new ArrayList<BitmapId<?>>(idx.size());
			keys.addAll(idx.keySet());
		} finally {
			idxLock.readLock().unlock();
		}
		return keys;
	}

	@Override
	public Iterator<BitmapId<?>> iterator() {
		return getBitmapIdentifiers().iterator();
	}

	@Override
	public boolean contains(final BitmapId<?> bitmapId) {
		idxLock.readLock().lock();
		try {
			return idx.containsKey(bitmapId);
		} finally {
			idxLock.readLock().unlock();
		}
	}
}
