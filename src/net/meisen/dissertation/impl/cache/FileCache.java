package net.meisen.dissertation.impl.cache;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
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

public class FileCache implements IBitmapCache {
	private final static Logger LOG = LoggerFactory.getLogger(FileCache.class);

	/**
	 * A {@code IndexEntry} is an entry in the index-file (see
	 * {@link FileCache#indexTableFileName}) of the cache. It defines the
	 * 
	 * @author pmeisen
	 * 
	 */
	protected static class IndexEntry {

		/**
		 * The size of an {@code IndexEntry} as bytes.
		 */
		public static final int IndexEntrySizeInBytes = 12;

		private final int size;
		private final int bitmapFilePosition;

		private int indexFileNumber;

		/**
		 * Constructor to create a {@code IndexEntry} from a byte-array.
		 * 
		 * @param bytes
		 *            the array to read the {@code IndexEntry} from
		 */
		public IndexEntry(final byte[] bytes) {
			if (bytes.length != IndexEntrySizeInBytes) {
				// TODO e
				throw new IllegalStateException();
			}

			// deserialize the values
			final int size = Streams.byteToInt(Arrays.copyOfRange(bytes, 0, 4));
			final int bfp = Streams.byteToInt(Arrays.copyOfRange(bytes, 4, 8));
			final int ifp = Streams.byteToInt(Arrays.copyOfRange(bytes, 8, 12));

			// set the values
			this.size = size;
			this.bitmapFilePosition = bfp;
			this.indexFileNumber = ifp;
		}

		/**
		 * Constructor to create a not persisted entry. The
		 * {@code indexFileNumber} is set to {@code -1}.
		 * 
		 * @param size
		 *            the size of the entry in the bitmap-file (see
		 *            {@link FileCache#bitmapFileName}) in bytes
		 * @param bitmapFilePosition
		 *            the position (in bytes) of the entry within the
		 *            bitmap-file (see {@link FileCache#bitmapFileName})
		 */
		public IndexEntry(final int size, final int bitmapFilePosition) {
			this.size = size;
			this.bitmapFilePosition = bitmapFilePosition;
			this.indexFileNumber = -1;
		}

		/**
		 * Gets the size of the entry in the bitmap-file (see
		 * {@link FileCache#bitmapFileName}) in bytes.
		 * 
		 * @return the size of the entry in the bitmap-file (see
		 *         {@link FileCache#bitmapFileName}) in bytes
		 */
		public int getSize() {
			return size;
		}

		/**
		 * Gets the position (in bytes) of the entry within the bitmap-file (see
		 * {@link FileCache#bitmapFileName}).
		 * 
		 * @return the position (in bytes) of the entry within the bitmap-file
		 *         (see {@link FileCache#bitmapFileName})
		 */
		public int getBitmapFilePosition() {
			return bitmapFilePosition;
		}

		/**
		 * Gets the number of the {@code IndexEntry}, i.e. the zero-based
		 * position of the entry within the bitmap-file (see
		 * {@link FileCache#bitmapFileName}).
		 * 
		 * @return the number of the {@code IndexEntry} (zero-based)
		 */
		public int getIndexFileNumber() {
			return indexFileNumber;
		}

		/**
		 * Sets the number of the {@code IndexEntry} for the entry.
		 * 
		 * @param indexFileNumber
		 *            the number of the {@code IndexEntry} (zero-based)
		 */
		public void setIndexFileNumber(final int indexFileNumber) {
			this.indexFileNumber = indexFileNumber;
		}

		/**
		 * Gets the byte representation of the {@code IndexEntry}.
		 * 
		 * @return the byte representation of the {@code IndexEntry}
		 */
		public byte[] bytes() {
			final byte[] bytesSize = Streams.intToByte(size);
			final byte[] bytesBfp = Streams.intToByte(bitmapFilePosition);
			final byte[] bytesIfp = Streams.intToByte(indexFileNumber);

			return Streams.combineBytes(bytesSize, bytesBfp, bytesIfp);
		}
	}

	/**
	 * The name of the file used as index-table.
	 */
	protected final static String indexTableFileName = "bitmap.idx";
	/**
	 * The name of the file used to store the bitmaps.
	 */
	protected final static String bitmapFileName = "bitmap.data";

	private static final int IndexMaxBitmapSizeInBytes = 200;
	private static final int IndexLineSizeInBytes = IndexMaxBitmapSizeInBytes
			+ IndexEntry.IndexEntrySizeInBytes;

	private static final int cacheMaxSize = 100000;
	private static final double cacheCleaningFactor = 0.2;
	private static final int cacheCleaningSize = (int) (cacheMaxSize * cacheCleaningFactor);

	private final Map<BitmapId<?>, IBitmapOwner> owners;
	private final Map<BitmapId<?>, Bitmap> cache;
	private final Map<BitmapId<?>, IndexEntry> index;

	private final ReentrantReadWriteLock ownersLock;
	private final ReentrantReadWriteLock cacheLock;
	private final ReentrantReadWriteLock indexLock;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory factory;

	private boolean init;
	private File location;
	private File modelLocation;

	private File indexTableFile;
	private File bitmapFile;

	private RandomAccessFile bitmapReader;
	private DataOutputStream bitmapWriter;
	private RandomAccessFile indexTableWriter;

	public FileCache() {
		this.owners = new HashMap<BitmapId<?>, IBitmapOwner>();
		this.index = new HashMap<BitmapId<?>, IndexEntry>();
		this.cache = new HashMap<BitmapId<?>, Bitmap>();

		this.ownersLock = new ReentrantReadWriteLock();
		this.cacheLock = new ReentrantReadWriteLock();
		this.indexLock = new ReentrantReadWriteLock();

		this.location = getDefaultLocation();
		this.init = false;
	}

	@Override
	public synchronized void initialize(final String modelId) {

		// if already initialized we are done
		if (this.init) {
			return;
		}

		this.modelLocation = new File(location, modelId);
		if (modelLocation.exists()) {
			// TODO implement
			throw new UnsupportedOperationException("CURRENTLY NOT SUPPORTED.");
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
		}

		// create the directories
		if (!modelLocation.mkdirs()) {
			exceptionRegistry.throwException(FileCacheException.class, 1002,
					modelLocation);
		} else if (LOG.isDebugEnabled()) {
			LOG.debug("Creating FileCache at '" + modelLocation + "'.");
		}

		// determine the needed files
		final File indexTableFile = new File(modelLocation, indexTableFileName);
		final File bitmapFile = new File(modelLocation, bitmapFileName);

		// create the files
		try {
			indexTableFile.createNewFile();
			bitmapFile.createNewFile();

			bitmapReader = new RandomAccessFile(bitmapFile, "r");
			bitmapWriter = new DataOutputStream(new FileOutputStream(
					bitmapFile, true));

			indexTableWriter = new RandomAccessFile(indexTableFile, "rws");
		} catch (final IOException e) {
			exceptionRegistry.throwException(FileCacheException.class, 1006, e);
		}
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
		boolean doCache = false;

		Bitmap bitmap;
		try {
			this.cacheLock.readLock().lock();
			this.indexLock.readLock().lock();

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
			this.indexLock.readLock().unlock();
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
		final IndexEntry curEntry = this.index.get(bitmapId);
		if (curEntry != null
				&& curEntry.getBitmapFilePosition() >= entry
						.getBitmapFilePosition()) {
			return false;
		} else {

			// get the position of the entry within the file
			if (curEntry == null) {
				entry.setIndexFileNumber(this.index.size());
			} else {
				entry.setIndexFileNumber(curEntry.getIndexFileNumber());
			}

			// determine the position
			final byte[] bytes = generateIndexLine(bitmapId, entry);
			final int indexFilePos = entry.getIndexFileNumber()
					* IndexLineSizeInBytes;

			// persist it to the file
			synchronized (indexTableWriter) {
				try {
					indexTableWriter.seek(indexFilePos);
					indexTableWriter.write(bytes);
				} catch (final IOException e) {
					// TODO add e
					throw new IllegalStateException();
				}
			}

			// index the value in memory
			this.index.put(bitmapId, entry);

			return true;
		}
	}

	protected byte[] generateIndexLine(final BitmapId<?> bitmapId,
			final IndexEntry entry) {
		final byte[] bytesId = bitmapId.bytes();
		final byte[] bytesEntry = entry.bytes();

		if (bytesId.length > IndexMaxBitmapSizeInBytes) {
			// TODO e
			throw new IllegalStateException();
		} else if (bytesEntry.length != IndexEntry.IndexEntrySizeInBytes) {
			// TODO e
			throw new IllegalStateException();
		} else {
			final byte[] filledBytesId = new byte[IndexMaxBitmapSizeInBytes];
			System.arraycopy(bytesId, 0, filledBytesId, 0, bytesId.length);

			return Streams.combineBytes(filledBytesId, bytesEntry);
		}
	}

	protected Bitmap _getFromCache(final BitmapId<?> bitmapId) {
		return cache.get(bitmapId);
	}

	protected Bitmap _getFromIndex(final BitmapId<?> bitmapId) {
		final IndexEntry entry = index.get(bitmapId);

		if (entry == null) {
			return null;
		} else {
			return readBitmap(entry.getBitmapFilePosition());
		}
	}

	@Override
	public void cacheBitmap(final BitmapId<?> bitmapId, final Bitmap bitmap) {
		final IndexEntry entry = writeBitmap(bitmap);

		this.cacheLock.writeLock().lock();
		this.indexLock.writeLock().lock();

		try {
			if (_indexBitmap(bitmapId, entry)) {
				_cacheBitmap(bitmapId, bitmap);
			}
		} finally {
			this.indexLock.writeLock().unlock();
			this.cacheLock.writeLock().unlock();
		}
	}

	@Override
	public void registerBitmapOwner(final IBitmapOwner owner) {
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

		// make sure we are the only once using the reader and writer
		synchronized (bitmapReader) {
			synchronized (bitmapWriter) {
				synchronized (indexTableWriter) {

					// release the indexTableWriter
					if (indexTableWriter != null) {
						try {
							indexTableWriter.close();
						} catch (final IOException e) {
							exceptionRegistry.throwException(
									FileCacheException.class, 1003, e,
									indexTableFile);
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

		// set the initialization to be false
		this.init = false;
	}

	public boolean isCached(final BitmapId<?> id) {

		this.cacheLock.writeLock().lock();
		try {
			return this.cache.containsKey(id);
		} finally {
			this.cacheLock.writeLock().unlock();
		}
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
