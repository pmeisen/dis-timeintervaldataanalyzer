package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Arrays;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.model.cache.IBitmapCacheConfig;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.IntervalIndex;
import net.meisen.dissertation.model.indexes.datarecord.MetaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

/**
 * Tests the implementation of a {@code FileCache}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestFileCache extends ModuleBasedTest {

	private TidaModel model;
	private FileCache fc;

	/**
	 * Create a test instance of a {@code FileCache}.
	 */
	@Before
	public void create() {
		setModulesHolder("/net/meisen/dissertation/impl/cache/fileCacheModel.xml");

		// create the instance of the cache
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
		fc = modulesHolder.getModule(DefaultValues.CACHE_ID);
		assertNotNull(fc);

		// make sure the location is deleted
		assertTrue(Files.deleteDir(fc.getLocation()));
	}

	/**
	 * Creates a {@code BitmapId} with the specified identifier.
	 * 
	 * @param nr
	 *            the identifier
	 * 
	 * @return the created {@code BitmapId}
	 */
	protected BitmapId<Integer> createBitmapId(final int nr) {
		return new BitmapId<Integer>(nr, IntervalIndex.class);
	}

	/**
	 * Helper method to cache some generated bitmaps to the {@code fc}.
	 * 
	 * @param fc
	 *            the {@code FileCache} to add data to
	 * @param amount
	 *            the amount of data to be generated
	 * @param dataOffset
	 *            the offset between the id and the data to be set within the
	 *            generated bitmap
	 */
	protected void cacheBitmap(final FileCache fc, final int amount,
			final int dataOffset) {
		for (int i = 0; i < amount; i++) {
			final BitmapId<?> id = createBitmapId(i);
			final Bitmap bmp = Bitmap.createBitmap(model.getIndexFactory(), i
					+ dataOffset);

			// cache the bitmap
			fc.cacheBitmap(id, bmp);
		}
	}

	/**
	 * Tests the exception to be thrown if the configuration is changed after
	 * initialization.
	 */
	@Test
	public void testConfigurationChangeException() {
		thrown.expect(FileCacheException.class);
		thrown.expectMessage("configuration cannot be changed");

		// initialize and change the configuration
		fc.initialize(model.getId());
		fc.setConfig(null);
	}

	/**
	 * Tests the exception to be thrown if an invalid configuration is used.
	 */
	@Test
	public void testInvalidConfigurationException() {
		thrown.expect(FileCacheException.class);
		thrown.expectMessage("cache does not support a configuration of type");

		// initialize and change the configuration
		fc.setConfig(new IBitmapCacheConfig() {
		});
	}

	/**
	 * Tests the exception to be thrown if an invalid location is used.
	 */
	@Test
	public void testInvalidLocationException() {
		thrown.expect(FileCacheException.class);
		thrown.expectMessage("unable to create the cache location");

		// use an invalid character for a folder
		fc.initialize("?");
	}

	/**
	 * Tests the creation of the model location, when the cache is initialized.
	 */
	@Test
	public void testCreationOfModelLocation() {
		fc.initialize(model.getId());

		// make sure the location is created
		assertNotNull(fc.getModelLocation());
		assertTrue(fc.getModelLocation().exists());
	}

	/**
	 * Tests the methods {@link IndexEntry#bytes()} and
	 * {@link IndexEntry#IndexEntry(byte[])}.
	 */
	@Test
	public void testIndexEntrySerializability() {
		IndexEntry e;

		// serialize it
		e = new IndexEntry(1000, 25000, (byte) 5);
		e.setIndexFileNumber(100);
		e = new IndexEntry(e.bytes());

		// deserialize it
		assertEquals(1000, e.getSize());
		assertEquals(25000, e.getBitmapFilePosition());
		assertEquals(100, e.getIndexFileNumber());
		assertEquals((byte) 5, e.getFileNumber());
	}

	/**
	 * Tests the loading of none-cached bitmaps.
	 */
	@Test
	public void testNoneCachedBitmapRetrieval() {
		fc.initialize(model.getId());

		final Bitmap emptyBitmap = model.getIndexFactory().createBitmap();

		// retrieve some bitmaps for different identifiers
		for (int i = 0; i < 1000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(i, IntervalIndex.class);

			assertFalse(fc.isCached(id));
			assertEquals(emptyBitmap, fc.getBitmap(id));
			assertTrue(fc.isCached(id));
		}
	}

	/**
	 * Tests the persisting of bitmaps to the hard-drive.
	 */
	@Test
	public void testPersistance() {
		fc.initialize(model.getId());

		for (int i = 0; i < 100000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(i, MetaIndex.class,
					"Classifier1");
			final Bitmap bmp = Bitmap.createBitmap(model.getIndexFactory(), i);

			// cache the bitmap
			fc.cacheBitmap(id, bmp);

			// clear the cache
			fc.clearCache();

			// retrieve the bitmap and check the cache status
			assertFalse(fc.isCached(id));
			assertEquals(bmp, fc.getBitmap(id));
			assertTrue(fc.isCached(id));
		}
	}

	/**
	 * The {@code FileCache} needs to create {@code lines} within the
	 * index-file, which are used to map a {@code BitmapId} to a
	 * {@code IndexEntry}. The {@code BitmapId} has to be transformed into a
	 * byte-array to be stored. This should only be done once for each new
	 * {@code BitmapId}. This tests checks, that the call is only done once for
	 * each {@code BitmapId}.
	 */
	@Test
	public void testUsageOfGenerateIndexLine() {
		final FileCache spy = Mockito.spy(fc);
		spy.initialize(model.getId());

		// let's cache 100 bitmaps for testing purposes
		cacheBitmap(spy, 100, 0);

		// the generation of a line should have been called exactly 100 times
		verify(spy, times(100)).generateIndexLine(Mockito.any(BitmapId.class),
				Mockito.any(IndexEntry.class));

		// modify the Bitmaps
		cacheBitmap(spy, 200, 100);

		/*
		 * The modification shouldn't have triggered the generation again,
		 * therefore in a total the generateIndexLine should only be called 200
		 * times.
		 */
		verify(spy, times(200)).generateIndexLine(Mockito.any(BitmapId.class),
				Mockito.any(IndexEntry.class));

		// release the FileCache
		spy.release();
	}

	/**
	 * Tests the usage of the maximal file-size.
	 */
	@Test
	public void testMaxFileSize() {
		setModulesHolder("/net/meisen/dissertation/impl/cache/fileCacheModelMaxFileSize.xml");

		// create the instance of the cache
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
		fc = modulesHolder.getModule(DefaultValues.CACHE_ID);
		assertNotNull(fc);

		// make sure the location is deleted
		assertTrue(Files.deleteDir(fc.getLocation()));

		// check the configuration
		assertEquals(1000, fc.getMaxCacheSize());
		assertEquals(0.8, fc.getCacheCleaningFactor(), 0.0);
		assertEquals(1024 * 1024, fc.getMaxFileSizeInByte());

		fc.initialize(model.getId());
		for (int i = 0; i < 100000; i++) {
			final Bitmap bmp = Bitmap.createBitmap(model.getIndexFactory(), 1,
					1000, 2000, Integer.MAX_VALUE - 2000);
			fc.cacheBitmap(createBitmapId(0), bmp);
		}

		assertEquals(8, fc.getNumberOfBitmapFiles());
	}

	/**
	 * Tests the loading of the internally used index from the hard-drive.
	 * Furthermore the tests checks the amounts of call to
	 * {@link FileCache#readBitmap(IndexEntry)}.
	 */
	@Test
	public void testIndexLoading() {
		final FileCache spy = Mockito.spy(fc);
		final FileCacheConfig config = new FileCacheConfig();
		config.setLocation(spy.getLocation());
		config.setMaxFileSize("1K");
		config.setCacheSize(100);
		config.setCacheCleaningFactor(1.0);
		spy.setConfig(config);

		// create a cache with some data
		spy.initialize(model.getId());
		cacheBitmap(spy, 100, 20);
		verify(spy, times(100)).cacheBitmap(Mockito.any(BitmapId.class),
				Mockito.any(Bitmap.class));
		verify(spy, times(100))._indexBitmap(Mockito.any(BitmapId.class),
				Mockito.any(IndexEntry.class));
		verify(spy, times(100))._cacheBitmap(Mockito.any(BitmapId.class),
				Mockito.any(Bitmap.class));

		// get the amount of created files
		final int nrOfFiles = spy.getNumberOfBitmapFiles();
		spy.release();

		// now initialize again and check if the data is loaded
		spy.initialize(model.getId());
		assertEquals(nrOfFiles, spy.getNumberOfBitmapFiles());
		assertEquals(0, fc.getCacheSize());

		// get the bitmap for 0, this cannot be empty now
		for (int i = 0; i < 100; i++) {
			final Bitmap bmp = spy.getBitmap(createBitmapId(i));
			assertEquals(1, bmp.getIds().length);
			assertTrue(Arrays.binarySearch(bmp.getIds(), i + 20) != -1);
		}

		// no new files were created and the cache is filled
		assertEquals(nrOfFiles, spy.getNumberOfBitmapFiles());
		assertEquals(100, spy.getCacheSize());

		// there was no need for reorganization
		verify(spy, times(0))._organizeCache();

		// the cache had to read every bitmap from disk
		verify(spy, times(100))._getFromCache(Mockito.any(BitmapId.class));
		verify(spy, times(100)).readBitmap(Mockito.any(IndexEntry.class));

		// every Bitmap had to be added to the cache, but not updated
		verify(spy, times(100)).cacheBitmap(Mockito.any(BitmapId.class),
				Mockito.any(Bitmap.class));
		verify(spy, times(100))._indexBitmap(Mockito.any(BitmapId.class),
				Mockito.any(IndexEntry.class));
		verify(spy, times(200))._cacheBitmap(Mockito.any(BitmapId.class),
				Mockito.any(Bitmap.class));

		// let's also get some values we don't have yet
		final Bitmap emptyBitmap = model.getIndexFactory().createBitmap();
		for (int i = 100; i < 200; i++) {
			final Bitmap bmp = spy.getBitmap(createBitmapId(i));
			assertEquals(0, bmp.getIds().length);
			assertEquals(emptyBitmap, bmp);
		}
		verify(spy, times(200))._getFromCache(Mockito.any(BitmapId.class));
		verify(spy, times(100)).readBitmap(Mockito.any(IndexEntry.class));
		verify(spy, times(1))._organizeCache();
		assertEquals(100, spy.getCacheSize());

		// we need to release the spy
		spy.release();
		assertTrue(Files.deleteDir(spy.getLocation()));
	}

	/**
	 * Tests the multi-threading of a {@code FileCache}.
	 * 
	 * @throws InterruptedException
	 *             if a thread is interrupted
	 */
	@Test
	public void testMultipleThreadSafety() throws InterruptedException {
		fc.initialize(model.getId());

		final Thread tCacheSize = new Thread() {

			@Override
			public void run() {
				while (fc.getCacheSize() == 0) {
					try {
						Thread.sleep(50);
					} catch (final InterruptedException e) {
						fail(e.getMessage());
					}
				}

				int i = 1;

				assertEquals(i, fc.getCacheSize());
				assertTrue(fc.isCached(createBitmapId(0)));
				assertFalse(fc.isCached(createBitmapId(1)));

				// this one is cached by
				assertNotNull(fc.getBitmap(createBitmapId(0)));

				// the first call 1 isn't cached
				if (!fc.isCached(createBitmapId(1))) {
					i++;
				}
				assertNotNull(fc.getBitmap(createBitmapId(1)));
			}
		};

		final Thread tCache = new Thread() {

			@Override
			public void run() {
				for (int i = 0; i < 100000; i++) {
					fc.cacheBitmap(createBitmapId(0), model.getIndexFactory()
							.createBitmap());
					assertTrue(fc.getCacheSize() > 0 && fc.getCacheSize() < 3);
				}
			}
		};

		// start the threads
		tCacheSize.start();
		tCache.start();

		// join with this one
		tCacheSize.join();
		tCache.join();

		// after everything there should be one left
		assertEquals(2, fc.getCacheSize());
	}

	/**
	 * Tests the cleaning of the cache.
	 */
	@Test
	public void testCleaning() {

		// create a new configuration
		final FileCacheConfig fcc = new FileCacheConfig();
		fcc.setCacheCleaningFactor(0.5);
		fcc.setCacheSize(10);
		fcc.setLocation(fc.getLocation());
		fc.setConfig(fcc);

		// initialize the model
		fc.initialize(model.getId());

		// add 100 values to the cache
		for (int i = 0; i < 100; i++) {
			fc.cacheBitmap(createBitmapId(i), model.getIndexFactory()
					.createBitmap());

			// use every 2nd bitmap
			if (i % 2 == 0) {
				fc.getBitmap(createBitmapId(i));
			}

			// there should never be more than 10 elements cached
			assertTrue(fc.getCacheSize() <= 10);

			// check the cache after the cache was cleaned
			if (i >= 10 && fc.getCacheSize() == 6) {
				final int v = i % 2 == 0 ? i : i - 1;

				assertTrue(fc.isCached(createBitmapId(i)));
				assertTrue(fc.isCached(createBitmapId(v)));
				assertTrue(fc.isCached(createBitmapId(v - 2)));
				assertTrue(fc.isCached(createBitmapId(v - 4)));
				assertTrue(fc.isCached(createBitmapId(v - 6)));
				assertTrue(fc.isCached(createBitmapId(v - 8)));
				if (i == v) {
					assertTrue(fc.isCached(createBitmapId(v - 10)));
				}
			}
		}
	}

	@Test
	public void testUsageScenario() throws InterruptedException {
		final FileCacheConfig config = new FileCacheConfig();
		config.setLocation(fc.getLocation());
		config.setMaxFileSize("10M");
		config.setCacheSize(80000);
		fc.setConfig(config);

		// initialize the model
		fc.initialize(model.getId());

		// create some bitmaps
		final Thread tIdBitmap = new Thread() {

			@Override
			public void run() {
				for (int i = 0; i < 100000; i++) {
					fc.getBitmap(createBitmapId(i));
					fc.cacheBitmap(createBitmapId(i),
							Bitmap.createBitmap(model.getIndexFactory(), i));
					fc.getBitmap(createBitmapId(i));
				}
			}
		};
		final Thread tIdPlus1Bitmap = new Thread() {

			@Override
			public void run() {
				for (int i = 0; i < 100000; i++) {
					fc.getBitmap(createBitmapId(i));
					fc.cacheBitmap(createBitmapId(i),
							Bitmap.createBitmap(model.getIndexFactory(), i + 1));
					fc.getBitmap(createBitmapId(i));
				}
			}
		};
		final Thread tIdPlus2Bitmap = new Thread() {

			@Override
			public void run() {
				for (int i = 0; i < 100000; i++) {
					fc.getBitmap(createBitmapId(i));
					fc.cacheBitmap(createBitmapId(i),
							Bitmap.createBitmap(model.getIndexFactory(), i + 2));
					fc.getBitmap(createBitmapId(i));
				}
			}
		};

		// start the threads
		tIdBitmap.start();
		tIdPlus1Bitmap.start();
		tIdPlus2Bitmap.start();

		// join with this one
		tIdBitmap.join();
		tIdPlus1Bitmap.join();
		tIdPlus2Bitmap.join();

		// now get the bitmaps
		final Thread tGetBitmap1 = new Thread() {
			@Override
			public void run() {
				for (int i = 0; i < 80000; i++) {
					final Bitmap bitmap = fc.getBitmap(createBitmapId(i));
					final int[] ids = bitmap.getIds();

					assertEquals(1, ids.length);
					assertTrue(bitmap.toString(), ids[0] == i
							|| ids[0] == i + 1 || ids[0] == i + 2);
				}
			}
		};
		final Thread tGetBitmap2 = new Thread() {

			@Override
			public void run() {
				for (int i = 20000; i < 100000; i++) {
					final Bitmap bitmap = fc.getBitmap(createBitmapId(i));
					final int[] ids = bitmap.getIds();

					assertEquals(1, ids.length);
					assertTrue(bitmap.toString(), ids[0] == i
							|| ids[0] == i + 1 || ids[0] == i + 2);
				}
			}
		};

		// start the threads
		tGetBitmap1.start();
		tGetBitmap2.start();

		// join with this one
		tGetBitmap1.join();
		tGetBitmap2.join();
	}

	/**
	 * CleanUp afterwards
	 */
	@After
	public void cleanUp() {

		// release the instance
		fc.release();

		// delete all the created files
		if (fc.getModelLocation() != null) {
			assertTrue("Cannot delete '" + fc.getLocation() + "'",
					Files.deleteDir(fc.getLocation()));
		}
	}
}
