package net.meisen.dissertation.impl.cache;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.cache.IBitmapCache;
import net.meisen.dissertation.model.cache.IBitmapCacheConfig;
import net.meisen.dissertation.model.cache.IBitmapOwner;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Streams;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Implementation of a {@code BitmapCache}, which uses the file-system to
 * persist bitmaps and loads only a specific amount of bitmaps in memory.
 * 
 * @author pmeisen
 * 
 */
public class FileCache implements IBitmapCache {
	private final static Logger LOG = LoggerFactory.getLogger(FileCache.class);

	/**
	 * The name of the file used as index-table.
	 */
	protected final static String idxTableFileName = "bitmap.idx";
	/**
	 * The name of the file used to store the bitmaps.
	 */
	protected final static String bitmapFileName = "bitmap.data";

	private static final int idxBitmapIdSizeInBytes = BitmapId
			.getMaxBytesLength();
	private static final int idxLineSizeInBytes = idxBitmapIdSizeInBytes
			+ IndexEntry.idxEntrySizeInBytes;

	private static final int cacheMaxSize = 100000;
	private static final double cacheCleaningFactor = 0.2;
	private static final int cacheCleaningSize = (int) (cacheMaxSize * cacheCleaningFactor);

	private final Map<BitmapId<?>, IBitmapOwner> owners;
	private final Map<BitmapId<?>, Bitmap> cache;
	private final Map<BitmapId<?>, IndexEntry> idx;

	private final ReentrantReadWriteLock ownersLock;
	private final ReentrantReadWriteLock cacheLock;
	private final ReentrantReadWriteLock idxLock;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory factory;

	private boolean init;
	private File location;
	private File modelLocation;

	private File idxTableFile;
	private File bitmapFile;

	private RandomAccessFile bitmapReader;
	private DataOutputStream bitmapWriter;
	private RandomAccessFile idxTableWriter;

	public FileCache() {
		this.owners = new HashMap<BitmapId<?>, IBitmapOwner>();
		this.idx = new HashMap<BitmapId<?>, IndexEntry>();
		this.cache = new HashMap<BitmapId<?>, Bitmap>();

		this.ownersLock = new ReentrantReadWriteLock();
		this.cacheLock = new ReentrantReadWriteLock();
		this.idxLock = new ReentrantReadWriteLock();

		this.location = getDefaultLocation();
		this.init = false;
	}

	@Override
	public synchronized void initialize(final String modelId) {

		// if already initialized we are done
		if (this.init) {
			return;
		}

		// define the needed files
		this.modelLocation = new File(location, modelId);
		this.idxTableFile = new File(modelLocation, idxTableFileName);
		this.bitmapFile = new File(modelLocation, bitmapFileName);

		// decide if data has to be loaded or an instance has to be created
		if (modelLocation.exists()) {
			loadInstance();
		} else {
			createInstance();
		}

		// initialization is finished
		this.init = true;
	}

	/**
	 * Creates a new instance of a {@code FileCache} at the currently defined
	 * {@code modelLocation}.
	 * 
	 * @throws FileCacheException
	 *             if the {@code modelLocation} is {@code null}, if the
	 *             {@code modelLocation} already exists, if the
	 *             {@code modelLocation} could not be created, or if an
	 *             io-exception occurred during initialization
	 */
	protected synchronized void createInstance() throws FileCacheException {

		// make sure we can create an instance
		if (modelLocation == null) {
			exceptionRegistry.throwException(FileCacheException.class, 1005);
		} else if (modelLocation.exists()) {
			exceptionRegistry.throwException(FileCacheException.class, 1004,
					modelLocation);
		} else if (!modelLocation.mkdirs()) {
			exceptionRegistry.throwException(FileCacheException.class, 1002,
					modelLocation);
		} else if (LOG.isDebugEnabled()) {
			LOG.debug("Creating FileCache at '" + modelLocation + "'.");
		}

		try {
			// create the files
			idxTableFile.createNewFile();
			bitmapFile.createNewFile();

			// initialize the reader and writer
			initializeReaderAndWriter();
		} catch (final IOException e) {
			exceptionRegistry.throwException(FileCacheException.class, 1006, e);
		}
	}

	protected synchronized void loadInstance() {

		// make sure we can load an instance
		if (modelLocation == null) {
			exceptionRegistry.throwException(FileCacheException.class, 1005);
		} else if (!modelLocation.exists()) {
			exceptionRegistry.throwException(FileCacheException.class, 1007,
					modelLocation);
		} else if (!idxTableFile.exists() || !bitmapFile.exists()) {
			exceptionRegistry.throwException(FileCacheException.class, 1008,
					idxTableFile, bitmapFile);
		} else if (!idxTableFile.isFile() || !bitmapFile.isFile()) {
			exceptionRegistry.throwException(FileCacheException.class, 1009,
					idxTableFile, bitmapFile);
		} else if (LOG.isDebugEnabled()) {
			LOG.debug("Loading FileCache from '" + modelLocation + "'.");
		}

		try {

			// create the reader and writer
			initializeReaderAndWriter();

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
					// TODO e
					throw new IllegalStateException();
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
			// TODO add e
			throw new IllegalStateException();
		}
	}

	protected void initializeReaderAndWriter() throws IOException {

		// bitmap reader and writer
		bitmapReader = new RandomAccessFile(bitmapFile, "r");
		bitmapWriter = new DataOutputStream(new FileOutputStream(bitmapFile,
				true));

		// index writer (and reader)
		idxTableWriter = new RandomAccessFile(idxTableFile, "rws");
	}

	protected IndexEntry writeBitmap(final Bitmap bitmap) {
		final int priorSize, afterSize;

		try {
			synchronized (bitmapWriter) {
				priorSize = bitmapWriter.size();

				bitmap.serialize(bitmapWriter);
				bitmapWriter.flush();

				afterSize = bitmapWriter.size();
			}
		} catch (final IOException e) {
			// TODO add e
			throw new IllegalStateException();
		}

		// calculate some values
		return new IndexEntry(afterSize - priorSize, priorSize);
	}

	protected Bitmap readBitmap(final int pos) {
		final Bitmap bitmap;

		synchronized (bitmapReader) {
			try {
				bitmapReader.seek(pos);
				bitmap = Bitmap.createFromInput(factory, bitmapReader);
			} catch (final IOException e) {
				// TODO e
				throw new IllegalStateException();
			}
		}

		return bitmap;
	}

	@Override
	public Bitmap getBitmap(final BitmapId<?> bitmapId) {
		if (!init) {
			exceptionRegistry.throwException(FileCacheException.class, 1010);
		}

		boolean doCache = false;

		Bitmap bitmap;
		try {
			this.cacheLock.readLock().lock();
			this.idxLock.readLock().lock();

			bitmap = _getFromCache(bitmapId);
			if (bitmap == null) {
				bitmap = _getFromIndex(bitmapId);
				doCache = true;
			}

			// if we still don't have a bitmap create an empty one
			if (bitmap == null) {
				bitmap = factory.createBitmap();
				doCache = true;
			}
		} finally {
			this.idxLock.readLock().unlock();
			this.cacheLock.readLock().unlock();
		}

		/*
		 * Cache the retrieved bitmap if needed. The cacheBitmap method ensures
		 * that an more accurate bitmap is not overridden.
		 */
		if (doCache) {
			cacheBitmap(bitmapId, bitmap);
		}

		return bitmap;
	}

	protected void _cacheBitmap(final BitmapId<?> bitmapId, final Bitmap bitmap) {

		// check if it makes sense to cache the bitmap

		// add the value
		this.cache.put(bitmapId, bitmap);

		// check if the cache size is exceeded
		if (this.cache.size() >= cacheMaxSize) {

			// cacheCleaningSize;

			final BitmapId<?> removedId = null;
			final IBitmapOwner owner = this.owners.get(removedId);

			// release the bitmap
			owner.releaseBitmap();
		}
	}

	/**
	 * Indexes the specified {@code entry} for the specified {@code bitmapId}.
	 * 
	 * @param bitmapId
	 * @param entry
	 * 
	 * @return {@code true} if the bitmap refreshed (i.e. was new) the internal
	 *         cache, otherwise {@code false}
	 * 
	 * @throws FileCacheException
	 *             if the {@code entry} is already indexed (i.e.
	 *             {@link IndexEntry#getIndexFileNumber()} returns a value
	 *             unequal to {@code -1} or if an io-problem occurred
	 */
	protected boolean _indexBitmap(final BitmapId<?> bitmapId,
			final IndexEntry entry) throws FileCacheException {

		if (entry.getIndexFileNumber() != -1) {
			// TODO add e
			throw new IllegalStateException();
		}

		/*
		 * check if we already have a newer entry cached, this might have
		 * happened because of asynchronous events
		 */
		final IndexEntry curEntry = this.idx.get(bitmapId);
		if (curEntry != null
				&& curEntry.getBitmapFilePosition() >= entry
						.getBitmapFilePosition()) {
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
					// TODO add e
					throw new IllegalStateException();
				}
			}

			// index the value in memory
			this.idx.put(bitmapId, entry);

			return true;
		}
	}

	protected byte[] generateIndexLine(final BitmapId<?> bitmapId,
			final IndexEntry entry) {
		final byte[] bytesId = bitmapId.bytes();
		final byte[] bytesEntry = entry.bytes();

		if (bytesId.length > idxBitmapIdSizeInBytes) {
			// TODO e
			throw new IllegalStateException();
		} else if (bytesEntry.length != IndexEntry.idxEntrySizeInBytes) {
			// TODO e
			throw new IllegalStateException();
		} else {
			final byte[] filledBytesId = new byte[idxBitmapIdSizeInBytes];
			System.arraycopy(bytesId, 0, filledBytesId, 0, bytesId.length);

			return Streams.combineBytes(filledBytesId, bytesEntry);
		}
	}

	protected Bitmap _getFromCache(final BitmapId<?> bitmapId) {
		return cache.get(bitmapId);
	}

	protected Bitmap _getFromIndex(final BitmapId<?> bitmapId) {
		final IndexEntry entry = idx.get(bitmapId);

		if (entry == null) {
			return null;
		} else {
			return readBitmap(entry.getBitmapFilePosition());
		}
	}

	@Override
	public void cacheBitmap(final BitmapId<?> bitmapId, final Bitmap bitmap) {
		if (!init) {
			exceptionRegistry.throwException(FileCacheException.class, 1010);
		}

		final IndexEntry entry = writeBitmap(bitmap);

		this.cacheLock.writeLock().lock();
		this.idxLock.writeLock().lock();

		try {
			if (_indexBitmap(bitmapId, entry)) {
				_cacheBitmap(bitmapId, bitmap);
			}
		} finally {
			this.idxLock.writeLock().unlock();
			this.cacheLock.writeLock().unlock();
		}
	}

	@Override
	public void registerBitmapOwner(final IBitmapOwner owner) {
		if (!init) {
			exceptionRegistry.throwException(FileCacheException.class, 1010);
		}

		this.ownersLock.writeLock().lock();

		try {
			if (owners.put(owner.getBitmapId(), owner) != null) {
				if (LOG.isWarnEnabled()) {
					LOG.warn("The owner of '" + owner.getBitmapId()
							+ "' was changed.");
				}
			}
		} finally {
			this.ownersLock.writeLock().unlock();
		}
	}

	/**
	 * Gets the root-location of the caches. The {@code FileCache} generates a
	 * sub-folder within this {@code location}.
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
	public void setConfig(final IBitmapCacheConfig config) {
		if (init) {
			exceptionRegistry.throwException(FileCacheException.class, 1000);
		} else if (config == null) {
			this.location = getDefaultLocation();
		} else if (config instanceof FileCacheConfig) {
			final FileCacheConfig fcConfig = (FileCacheConfig) config;
			this.location = fcConfig.getLocation();
		} else {
			exceptionRegistry.throwException(FileCacheException.class, 1001,
					config.getClass().getName());
		}
	}

	/**
	 * Gets the default-location used by the {@code FileCache} if no other is
	 * specified.
	 * 
	 * @return the default-location used by the {@code FileCache}
	 */
	protected File getDefaultLocation() {
		return new File(".");
	}

	@Override
	public synchronized void release() {
		if (!this.init) {
			return;
		} else if (LOG.isDebugEnabled()) {
			LOG.debug("Releasing FileCache at '" + getModelLocation() + "'.");
		}

		/*
		 * set the initialization to be false, every call to the public methods
		 * is not permitted now, initialization is blocked because this method
		 * is synchronized
		 */
		this.init = false;

		// make sure we are the only once using the reader and writer
		synchronized (bitmapReader) {
			synchronized (bitmapWriter) {
				synchronized (idxTableWriter) {

					// release the indexTableWriter
					if (idxTableWriter != null) {
						try {
							idxTableWriter.close();
						} catch (final IOException e) {
							exceptionRegistry.throwException(
									FileCacheException.class, 1003, e,
									idxTableFile);
						}
					}

					// release the reader
					if (bitmapReader != null) {
						try {
							bitmapReader.close();
						} catch (final IOException e) {
							exceptionRegistry.throwException(
									FileCacheException.class, 1003, e,
									bitmapFile);
						}
					}

					// release the writer
					if (bitmapWriter != null) {
						try {
							bitmapWriter.close();
						} catch (final IOException e) {
							exceptionRegistry.throwException(
									FileCacheException.class, 1003, e,
									bitmapFile);
						}
					}
				}
			}
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

	public boolean isCached(final BitmapId<?> id) {

		this.cacheLock.writeLock().lock();
		try {
			return this.cache.containsKey(id);
		} finally {
			this.cacheLock.writeLock().unlock();
		}
	}

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

	public void clearCache() {
		this.cacheLock.writeLock().lock();
		try {
			this.cache.clear();
		} finally {
			this.cacheLock.writeLock().unlock();
		}
	}
}
