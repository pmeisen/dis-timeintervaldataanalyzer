package net.meisen.dissertation.impl.cache;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.cache.IBitmapCache;
import net.meisen.dissertation.model.cache.IBitmapCacheConfig;
import net.meisen.dissertation.model.cache.IBitmapOwner;
import net.meisen.dissertation.model.data.TidaModel;
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
public class FileBitmapCache implements IBitmapCache {
	private final static Logger LOG = LoggerFactory.getLogger(FileBitmapCache.class);

	/**
	 * The name of the file used as index-table.
	 */
	protected final static String idxTableFileName = "bitmap.idx";
	/**
	 * The name of the file used to store the bitmaps.
	 */
	protected final static String bitmapFileName = "bitmap_{nr}.data";

	private static final int idxBitmapIdSizeInBytes = BitmapId
			.getMaxBytesLength();
	private static final int idxLineSizeInBytes = idxBitmapIdSizeInBytes
			+ IndexEntry.idxEntrySizeInBytes;

	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	private IExceptionRegistry exceptionRegistry;

	@Autowired
	@Qualifier(DefaultValues.INDEXFACTORY_ID)
	private BaseIndexFactory factory;

	private final CachingStrategy strategy;

	private final Map<BitmapId<?>, IBitmapOwner> owners;
	private final Map<BitmapId<?>, Bitmap> cache;
	private final Map<BitmapId<?>, IndexEntry> idx;

	private final ReentrantReadWriteLock ownersLock;
	private final ReentrantReadWriteLock cacheLock;
	private final ReentrantReadWriteLock idxLock;
	private final ReentrantReadWriteLock bitmapLock;

	private final List<RandomAccessFile> bitmapReaders;
	private final List<DataOutputStream> bitmapWriters;
	private final List<File> bitmapFiles;

	private boolean init;
	private File location;
	private File modelLocation;
	private int cacheSize;
	private int maxFileSizeInByte;
	private double cacheCleaningFactor;

	private int curFileNumber = -1;

	private RandomAccessFile idxTableWriter;
	private File idxTableFile;

	public FileBitmapCache() {
		this.strategy = new CachingStrategy();

		this.owners = new HashMap<BitmapId<?>, IBitmapOwner>();
		this.idx = new HashMap<BitmapId<?>, IndexEntry>();
		this.cache = new HashMap<BitmapId<?>, Bitmap>();

		this.ownersLock = new ReentrantReadWriteLock();
		this.cacheLock = new ReentrantReadWriteLock();
		this.idxLock = new ReentrantReadWriteLock();
		this.bitmapLock = new ReentrantReadWriteLock();

		this.location = null;
		this.cacheSize = getDefaultCacheSize();
		this.cacheCleaningFactor = getDefaultCacheCleaningFactor();

		this.bitmapReaders = new ArrayList<RandomAccessFile>();
		this.bitmapWriters = new ArrayList<DataOutputStream>();
		this.bitmapFiles = new ArrayList<File>();

		this.init = false;
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
		this.idxTableFile = new File(this.modelLocation, idxTableFileName);

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
	 * Creates a new instance of a {@code FileBitmapCache} at the currently defined
	 * {@code modelLocation}.
	 * 
	 * @throws FileBitmapCacheException
	 *             if the {@code modelLocation} is {@code null}, if the
	 *             {@code modelLocation} already exists, if the
	 *             {@code modelLocation} could not be created, or if an
	 *             {@code IOException} occurred during initialization
	 * 
	 * @see IOException
	 */
	protected void createInstance() throws FileBitmapCacheException {

		// make sure we can create an instance
		if (modelLocation == null) {
			exceptionRegistry.throwException(FileBitmapCacheException.class, 1005);
		} else if (idxTableFile.exists()) {
			exceptionRegistry.throwException(FileBitmapCacheException.class, 1004,
					modelLocation);
		} else if (!modelLocation.exists() && !modelLocation.mkdirs()) {
			exceptionRegistry.throwException(FileBitmapCacheException.class, 1002,
					modelLocation);
		} else if (LOG.isDebugEnabled()) {
			LOG.debug("Creating FileBitmapCache at '" + modelLocation + "'.");
		}

		try {
			// create the index file
			idxTableFile.createNewFile();

			// create the index writer
			idxTableWriter = new RandomAccessFile(idxTableFile, "rws");

			// create a first bitmap file
			_createNewBitmapFile();
		} catch (final IOException e) {
			exceptionRegistry.throwException(FileBitmapCacheException.class, 1006, e,
					idxTableFile);
		}
	}

	protected void loadInstance() {

		// make sure we can load an instance
		if (modelLocation == null) {
			exceptionRegistry.throwException(FileBitmapCacheException.class, 1005);
		} else if (!modelLocation.exists()) {
			exceptionRegistry.throwException(FileBitmapCacheException.class, 1007,
					modelLocation);
		} else if (!idxTableFile.exists()) {
			exceptionRegistry.throwException(FileBitmapCacheException.class, 1008,
					idxTableFile);
		} else if (!idxTableFile.isFile()) {
			exceptionRegistry.throwException(FileBitmapCacheException.class, 1009,
					idxTableFile);
		} else if (LOG.isDebugEnabled()) {
			LOG.debug("Loading FileBitmapCache from '" + modelLocation + "'.");
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

		// validate the read files
		for (final IndexEntry entry : this.idx.values()) {
			final int fileNr = entry.getFileNumber();
			final File file = getFile(fileNr);

			if (bitmapFiles.contains(file)) {
				continue;
			} else {

				// add the bitmapFile
				try {
					_addBitmapFile(file, fileNr);
				} catch (final IOException e) {
					// TODO add e
					throw new IllegalStateException();
				}
			}
		}

		// set the number of the file
		curFileNumber = bitmapFiles.size() - 1;
	}

	protected void _addBitmapFile(final File bitmapFile,
			final int expectedNumber) throws IOException {

		if (!bitmapFile.exists() || !bitmapFile.isFile()) {
			// TODO add e
			throw new IllegalStateException();
		}

		// bitmap reader and writer
		final RandomAccessFile reader = new RandomAccessFile(bitmapFile, "r");
		final DataOutputStream writer = new DataOutputStream(
				new FileOutputStream(bitmapFile, true));

		// add everything
		if (expectedNumber < bitmapFiles.size()) {
			if (bitmapFiles.set(expectedNumber, bitmapFile) != null
					|| bitmapReaders.set(expectedNumber, reader) != null
					|| bitmapWriters.set(expectedNumber, writer) != null) {

				// close the reader and writer cause they will not be used
				Streams.closeIO(reader);
				Streams.closeIO(writer);

				// TODO add e
				throw new IllegalStateException();
			}
		} else {

			// add some empty placeholders
			if (expectedNumber > bitmapFiles.size()) {
				for (int i = bitmapFiles.size(); i < expectedNumber; i++) {
					bitmapFiles.add(null);
					bitmapReaders.add(null);
					bitmapWriters.add(null);
				}
			}

			// add the bitmapFile and it's reader and writer
			bitmapFiles.add(bitmapFile);
			bitmapReaders.add(reader);
			bitmapWriters.add(writer);
		}
	}

	protected void _createNewBitmapFile() throws IOException {

		// the current file has to be increased by one
		curFileNumber++;

		// create the file
		final File file = getFile(curFileNumber);
		file.createNewFile();

		// bitmap reader and writer
		_addBitmapFile(file, curFileNumber);

		if (LOG.isTraceEnabled()) {
			LOG.trace("Created new BitmapFile with fileNumber '"
					+ curFileNumber + "' at '" + file + "'.");
		}
	}

	protected File getFile(final int nr) {
		final String fileName = bitmapFileName.replace("{nr}", "" + nr);
		return new File(getModelLocation(), fileName);
	}

	protected IndexEntry writeBitmap(final Bitmap bitmap) {

		// get the byte representation
		final ByteArrayOutputStream baos = new ByteArrayOutputStream();
		final DataOutputStream w = new DataOutputStream(baos);
		try {
			bitmap.serialize(w);
			w.flush();
		} catch (final IOException e) {
			// TODO add e
			throw new IllegalStateException();
		} finally {
			Streams.closeIO(w);
			Streams.closeIO(baos);
		}
		final byte[] result = baos.toByteArray();
		final int resSize = result.length;

		// check if the result can fit into a file
		if (resSize > getMaxFileSizeInByte()) {
			// TODO add e
			throw new IllegalStateException();
		}

		int priorSize;
		int fileNr;
		boolean written;

		// write the byte-array to the writer
		bitmapLock.readLock().lock();
		try {
			final DataOutputStream writer = bitmapWriters.get(curFileNumber);

			synchronized (writer) {
				priorSize = writer.size();
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
			// TODO add e
			throw new IllegalStateException();
		} finally {
			bitmapLock.readLock().unlock();
		}

		if (written) {

			// create the entry the bitmap is stored at
			return new IndexEntry(resSize, priorSize, fileNr);
		} else {

			// create a new file
			bitmapLock.writeLock().lock();
			try {

				// check if no other thread created a new file
				if (fileNr == curFileNumber) {
					_createNewBitmapFile();
				}
			} catch (final IOException e) {
				// TODO add e
				throw new IllegalStateException();
			} finally {
				bitmapLock.writeLock().unlock();
			}

			// try again to write the file
			return writeBitmap(bitmap);
		}
	}

	protected Bitmap readBitmap(final IndexEntry e) {
		final Bitmap bitmap;

		bitmapLock.readLock().lock();
		try {
			final RandomAccessFile reader = bitmapReaders
					.get(e.getFileNumber());

			synchronized (reader) {
				reader.seek(e.getBitmapFilePosition());
				bitmap = Bitmap.createFromInput(factory, reader);
			}
		} catch (final IOException ex) {
			// TODO e
			throw new IllegalStateException(ex);
		} finally {
			bitmapLock.readLock().unlock();
		}

		return bitmap;
	}

	@Override
	public Bitmap getBitmap(final BitmapId<?> bitmapId) {
		if (!init) {
			exceptionRegistry.throwException(FileBitmapCacheException.class, 1010);
		}

		boolean doCache = false;
		Bitmap bitmap;
		try {
			this.cacheLock.readLock().lock();
			this.idxLock.readLock().lock();

			// check the cache
			bitmap = _getFromCache(bitmapId);

			// if we couldn't find it get it from the hard-drive
			if (bitmap == null) {
				bitmap = _getFromIndex(idx.get(bitmapId));
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
					_cacheBitmap(bitmapId, bitmap);
				}
			} finally {
				this.cacheLock.writeLock().unlock();
			}

		}

		// mark the bitmap as used
		getStrategy().usedBitmap(bitmapId);

		return bitmap;
	}

	protected void _cacheBitmap(final BitmapId<?> bitmapId, final Bitmap bitmap) {

		// add the value
		if (cache.put(bitmapId, bitmap) == null) {

			/*
			 * check if the cache size is exceeded and if needed, trigger a
			 * re-organization of the cache
			 */
			if (cache.size() > cacheSize) {
				_organizeCache();
			}

			// register the bitmap because it wasn't registered yet
			getStrategy().registerBitmap(bitmapId);
		}
	}

	protected void _organizeCache() {

		// determine the cleaning size, i.e. how many elements to be removed
		final int cacheCleaningSize = Math.min(cacheSize,
				Math.max(1, (int) (cacheSize * getCacheCleaningFactor())));

		// determine which bitmaps to be released
		final List<BitmapId<?>> removedIds = getStrategy()
				.determineLessUsedList(cacheCleaningSize, true);

		// log what is removed
		if (LOG.isTraceEnabled()) {
			LOG.trace("Organizing cache (size: " + cache.size()
					+ ") by removing '" + removedIds.size() + "'");
		}

		// remove the bitmaps from the cache and the owner
		for (final BitmapId<?> removedId : removedIds) {
			final IBitmapOwner owner = this.owners.get(removedId);

			// remove the identifier from the cache
			cache.remove(removedId);

			// release the bitmap from the owner
			if (owner != null) {
				owner.releaseBitmap();
			}
		}

		if (LOG.isTraceEnabled()) {
			LOG.trace("Finalized organizing of cache (new size: "
					+ cache.size() + ")");
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
			return curEntry.getBitmapFilePosition() < expectedNewEntry
					.getBitmapFilePosition();
		}
	}

	/**
	 * Indexes the specified {@code entry} for the specified {@code bitmapId}.
	 * 
	 * @param bitmapId
	 * @param entry
	 * 
	 * @return {@code true} if the bitmap refreshed (i.e. was newer than the
	 *         internally cached one), otherwise {@code false}
	 * 
	 * @throws FileBitmapCacheException
	 *             if the {@code entry} is already indexed (i.e.
	 *             {@link IndexEntry#getIndexFileNumber()} returns a value
	 *             unequal to {@code -1} or if an io-problem occurred
	 */
	protected boolean _indexBitmap(final BitmapId<?> bitmapId,
			final IndexEntry entry) throws FileBitmapCacheException {

		if (entry.getIndexFileNumber() != -1) {
			// TODO add e
			throw new IllegalStateException();
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

	protected Bitmap _getFromIndex(final IndexEntry entry) {
		if (entry == null) {
			return null;
		} else {
			return readBitmap(entry);
		}
	}

	@Override
	public void cacheBitmap(final BitmapId<?> bitmapId, final Bitmap bitmap) {
		if (!init) {
			exceptionRegistry.throwException(FileBitmapCacheException.class, 1010);
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
			exceptionRegistry.throwException(FileBitmapCacheException.class, 1010);
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
	 * Gets the root-location of the caches. The {@code FileBitmapCache} generates a
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
			exceptionRegistry.throwException(FileBitmapCacheException.class, 1000);
		} else if (config == null) {
			this.location = null;
			this.cacheSize = getDefaultCacheSize();
			this.maxFileSizeInByte = getDefaultMaxFileSizeInByte();
		} else if (config instanceof FileBitmapCacheConfig) {
			final FileBitmapCacheConfig fcc = (FileBitmapCacheConfig) config;

			final File cLoc = fcc.getLocation();
			this.location = cLoc == null ? null : fcc.getLocation();

			final Integer cSize = fcc.getCacheSize();
			this.cacheSize = cSize == null ? getDefaultCacheSize() : cSize;

			final Double cFactor = fcc.getCacheCleaningFactor();
			this.cacheCleaningFactor = cFactor == null ? getDefaultCacheCleaningFactor()
					: cFactor;

			final Integer cMaxFileSizeInByte = fcc.getMaxFileSizeInByte();
			this.maxFileSizeInByte = cMaxFileSizeInByte == null ? getDefaultMaxFileSizeInByte()
					: cMaxFileSizeInByte;
		} else {
			exceptionRegistry.throwException(FileBitmapCacheException.class, 1001,
					config.getClass().getName());
		}
	}

	protected CachingStrategy getStrategy() {
		return strategy;
	}

	/**
	 * Gets the default-location used by the {@code FileBitmapCache} if no other is
	 * specified.
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

	protected double getDefaultCacheCleaningFactor() {
		return 0.2;
	}

	protected int getDefaultMaxFileSizeInByte() {
		return Integer.MAX_VALUE;
	}

	@Override
	public synchronized void release() {
		if (!this.init) {
			return;
		} else if (LOG.isDebugEnabled()) {
			LOG.debug("Releasing FileBitmapCache at '" + getModelLocation() + "'.");
		}

		/*
		 * set the initialization to be false, every call to the public methods
		 * is not permitted now, initialization is blocked because this method
		 * is synchronized
		 */
		this.init = false;

		// release the index writer
		synchronized (idxTableWriter) {

			// release the indexTableWriter
			if (idxTableWriter != null) {
				try {
					idxTableWriter.close();
				} catch (final IOException e) {
					exceptionRegistry.throwException(FileBitmapCacheException.class,
							1003, e, idxTableFile);
				}
			}
		}

		// make sure we are the only once using the reader and writer
		bitmapLock.writeLock().lock();
		try {

			// release the reader
			int fileCounter = 0;
			for (final RandomAccessFile raf : bitmapReaders) {
				try {
					raf.close();
				} catch (final IOException e) {
					exceptionRegistry.throwException(FileBitmapCacheException.class,
							1003, e, bitmapFiles.get(fileCounter));
				}

				fileCounter++;
			}

			// release the writer
			fileCounter = 0;
			for (final DataOutputStream dos : bitmapWriters) {
				try {
					dos.close();
				} catch (final IOException e) {
					exceptionRegistry.throwException(FileBitmapCacheException.class,
							1003, e, bitmapFiles.get(fileCounter));
				}

				fileCounter++;
			}

			// reset the files
			bitmapReaders.clear();
			bitmapWriters.clear();
			bitmapFiles.clear();

			// reset the number
			curFileNumber = -1;
		} finally {
			bitmapLock.writeLock().unlock();
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

	public int getMaxCacheSize() {
		return cacheSize;
	}

	public void clearCache() {
		this.cacheLock.writeLock().lock();
		try {
			this.cache.clear();
		} finally {
			this.cacheLock.writeLock().unlock();
		}
	}

	public double getCacheCleaningFactor() {
		return cacheCleaningFactor;
	}

	public int getMaxFileSizeInByte() {
		return maxFileSizeInByte;
	}

	public int getNumberOfBitmapFiles() {
		return curFileNumber + 1;
	}
}
