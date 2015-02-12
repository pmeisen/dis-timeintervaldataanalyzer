package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.io.File;
import java.util.Arrays;
import java.util.Random;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.help.ThreadForTesting;
import net.meisen.dissertation.model.cache.IBitmapIdCacheConfig;
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
 * Tests the implementation of a {@code FileBitmapCache}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestFileBitmapCache extends ModuleBasedTest {

	private TidaModel model;
	private FileBitmapCache fc;

	/**
	 * Create a test instance of a {@code FileBitmapCache}.
	 */
	@Before
	public void create() {
		setModulesHolder("/net/meisen/dissertation/impl/cache/fileBitmapCacheModel.xml");

		// create the instance of the cache
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
		fc = modulesHolder.getModule(DefaultValues.BITMAPCACHE_ID);
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
	 *            the {@code FileBitmapCache} to add data to
	 * @param amount
	 *            the amount of data to be generated
	 * @param dataOffset
	 *            the offset between the id and the data to be set within the
	 *            generated bitmap
	 */
	protected void cache(final FileBitmapCache fc, final int amount,
			final int dataOffset) {
		for (int i = 0; i < amount; i++) {
			final BitmapId<?> id = createBitmapId(i);
			final Bitmap bmp = Bitmap.createBitmap(model.getIndexFactory(), i
					+ dataOffset);

			// cache the bitmap
			fc.cache(id, bmp);
		}
	}

	/**
	 * Tests the exception to be thrown if the configuration is changed after
	 * initialization.
	 */
	@Test
	public void testConfigurationChangeException() {
		thrown.expect(BaseFileBitmapIdCacheException.class);
		thrown.expectMessage("configuration cannot be changed");

		// initialize and change the configuration
		fc.initialize(model);
		fc.setConfig(null);
	}

	/**
	 * Tests the exception to be thrown if an invalid configuration is used.
	 */
	@Test
	public void testInvalidConfigurationException() {
		thrown.expect(BaseFileBitmapIdCacheException.class);
		thrown.expectMessage("cache does not support a configuration of type");

		// initialize and change the configuration
		fc.setConfig(new IBitmapIdCacheConfig() {
		});
	}

	/**
	 * Tests the exception to be thrown if an invalid modelId is used.
	 */
	@Test
	public void testInvalidModelIdException() {
		thrown.expect(BaseFileBitmapIdCacheException.class);
		thrown.expectMessage("unable to create the cache location");

		// use an invalid character for a folder
		final TidaModel model = new TidaModel() {
			@Override
			public String getId() {
				return "?";
			}

			@Override
			public File getLocation() {
				return null;
			};
		};
		fc.initialize(model);
	}

	/**
	 * Tests the exception to be thrown if an invalid location is used.
	 */
	@Test
	public void testInvalidLocationException() {
		thrown.expect(BaseFileBitmapIdCacheException.class);
		thrown.expectMessage("unable to create the cache location");

		// use an invalid character for a folder
		final FileBitmapIdCacheConfig config = new FileBitmapIdCacheConfig();
		config.setLocation(null);
		fc.setConfig(config);

		final TidaModel model = new TidaModel() {
			@Override
			public String getId() {
				return "modelId";
			}

			@Override
			public File getLocation() {
				return new File("?");
			};
		};
		fc.initialize(model);
	}

	/**
	 * Tests the creation of the model location, when the cache is initialized.
	 */
	@Test
	public void testCreationOfModelLocation() {
		fc.initialize(model);

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
		assertEquals(25000, e.getFilePosition());
		assertEquals(100, e.getIndexFileNumber());
		assertEquals((byte) 5, e.getFileNumber());
	}

	/**
	 * Tests the loading of none-cached bitmaps.
	 */
	@Test
	public void testNoneCachedBitmapRetrieval() {
		fc.initialize(model);

		final Bitmap emptyBitmap = model.getIndexFactory().createBitmap();

		// retrieve some bitmaps for different identifiers
		for (int i = 0; i < 1000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(i, IntervalIndex.class);

			assertFalse(fc.isCached(id));
			assertEquals(emptyBitmap, fc.get(id));
			assertTrue(fc.isCached(id));
		}
	}

	/**
	 * Tests the persisting of bitmaps to the hard-drive.
	 */
	@Test
	public void testPersistance() {
		fc.initialize(model);

		for (int i = 0; i < 50000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(i, MetaIndex.class,
					"Classifier1");
			final Bitmap bmp = Bitmap.createBitmap(model.getIndexFactory(), i);

			// cache the bitmap
			fc.cache(id, bmp);

			// clear the cache
			fc.clearCache();

			// retrieve the bitmap and check the cache status
			assertFalse(fc.isCached(id));
			assertEquals(bmp, fc.get(id));
			assertTrue(fc.isCached(id));
		}

		fc.clearCache();

		// check to read all the bitmaps
		for (int i = 0; i < 50000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(i, MetaIndex.class,
					"Classifier1");
			final Bitmap bmp = Bitmap.createBitmap(model.getIndexFactory(), i);

			// get the bitmap from the cache
			assertFalse(fc.isCached(id));
			assertEquals(bmp, fc.get(id));
			assertTrue(fc.isCached(id));
		}
	}

	/**
	 * Tests the bulk-write.
	 */
	@Test
	public void testBulkPersistance() {
		fc.initialize(model);

		// disable the persistancy
		fc.setPersistency(false);

		for (int i = 0; i < 50000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(i, MetaIndex.class,
					"Classifier1");
			final Bitmap bmp = Bitmap.createBitmap(model.getIndexFactory(), i);

			// cache the bitmap
			fc.cache(id, bmp);

			// retrieve the bitmap and check the cache status
			assertEquals(bmp, fc.get(id));
			assertTrue(fc.isCached(id));
		}

		// everything should be in cache
		assertEquals(50000, fc.getCacheSize());

		// enable it again
		fc.setPersistency(true);

		// clear all the cached data
		fc.clearCache();

		// check to read all the bitmaps
		for (int i = 0; i < 50000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(i, MetaIndex.class,
					"Classifier1");
			final Bitmap bmp = Bitmap.createBitmap(model.getIndexFactory(), i);

			// get the bitmap from the cache
			assertFalse(fc.isCached(id));
			assertEquals(bmp, fc.get(id));
			assertTrue(fc.isCached(id));
		}
	}

	/**
	 * Tests the bulk-write with using the
	 * {@link FileBitmapCache#_organizeCache()}.
	 */
	@Test
	public void testBulkPersistanceWithReorganization() {
		final FileBitmapIdCacheConfig config = new FileBitmapIdCacheConfig();
		config.setLocation(fc.getLocation());
		config.setCacheSize(10000);
		config.setCacheCleaningFactor(1.0);
		fc.setConfig(config);
		fc.initialize(model);

		// disable the persistancy
		fc.setPersistency(false);

		for (int i = 0; i < 50000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(i, MetaIndex.class,
					"Classifier1");
			final Bitmap bmp = Bitmap.createBitmap(model.getIndexFactory(), i);

			// cache the bitmap
			fc.cache(id, bmp);

			// retrieve the bitmap and check the cache status
			assertEquals(bmp, fc.get(id));
			assertTrue(fc.isCached(id));
		}

		// enable it again
		fc.setPersistency(true);

		// clear all the cached data
		fc.clearCache();

		// check to read all the bitmaps
		for (int i = 0; i < 50000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(i, MetaIndex.class,
					"Classifier1");
			final Bitmap bmp = Bitmap.createBitmap(model.getIndexFactory(), i);

			// get the bitmap from the cache
			assertFalse(fc.isCached(id));
			assertEquals(bmp, fc.get(id));
		}
	}

	/**
	 * Tests the bulk-write with using the {@link FileBitmapCache#clearCache()}.
	 */
	@Test
	public void testBulkPersistanceWithClearCache() {
		fc.initialize(model);

		fc.setPersistency(false);

		for (int i = 0; i < 50000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(i, MetaIndex.class,
					"Classifier1");
			final Bitmap bmp = Bitmap.createBitmap(model.getIndexFactory(), i);

			// cache the bitmap
			fc.cache(id, bmp);

			// clear the cache
			fc.clearCache();

			// retrieve the bitmap and check the cache status
			assertFalse(fc.isCached(id));
			assertEquals(bmp, fc.get(id));
			assertTrue(fc.isCached(id));
		}

		fc.setPersistency(true);
	}

	/**
	 * The {@code FileBitmapCache} needs to create {@code lines} within the
	 * index-file, which are used to map a {@code BitmapId} to a
	 * {@code IndexEntry}. The {@code BitmapId} has to be transformed into a
	 * byte-array to be stored. This should only be done once for each new
	 * {@code BitmapId}. This tests checks, that the call is only done once for
	 * each {@code BitmapId}.
	 */
	@Test
	public void testUsageOfGenerateIndexLine() {
		final FileBitmapCache spy = Mockito.spy(fc);
		spy.initialize(model);

		// let's cache 100 bitmaps for testing purposes
		cache(spy, 100, 0);

		// the generation of a line should have been called exactly 100 times
		verify(spy, times(100)).generateIndexLine(Mockito.any(BitmapId.class),
				Mockito.any(IndexEntry.class));

		// modify the Bitmaps
		cache(spy, 200, 100);

		/*
		 * The modification shouldn't have triggered the generation again,
		 * therefore in a total the generateIndexLine should only be called 200
		 * times.
		 */
		verify(spy, times(200)).generateIndexLine(Mockito.any(BitmapId.class),
				Mockito.any(IndexEntry.class));

		// release the FileBitmapCache
		spy.release();
	}

	/**
	 * Tests the usage of the maximal file-size.
	 */
	@Test
	public void testMaxFileSize() {
		setModulesHolder("/net/meisen/dissertation/impl/cache/fileBitmapCacheModelMaxFileSize.xml");

		// create the instance of the cache
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
		fc = modulesHolder.getModule(DefaultValues.BITMAPCACHE_ID);
		assertNotNull(fc);

		// make sure the location is deleted
		assertTrue(Files.deleteDir(fc.getLocation()));

		// check the configuration
		assertEquals(1000, fc.getMaxCacheSize());
		assertEquals(0.8, fc.getCacheCleaningFactor(), 0.0);
		assertEquals(1024 * 1024, fc.getMaxFileSizeInByte());

		fc.initialize(model);
		for (int i = 0; i < 100000; i++) {
			final Bitmap bmp = Bitmap.createBitmap(model.getIndexFactory(), 1,
					1000, 2000, Integer.MAX_VALUE - 2000);
			fc.cache(createBitmapId(0), bmp);
		}

		assertEquals(8, fc.getNumberOfFiles());
	}

	/**
	 * Tests the loading of the internally used index from the hard-drive.
	 * Furthermore the tests checks the amounts of call to
	 * {@link FileBitmapCache#read(IndexEntry)}.
	 */
	@Test
	public void testIndexLoading() {
		final FileBitmapCache spy = Mockito.spy(fc);
		final FileBitmapIdCacheConfig config = new FileBitmapIdCacheConfig();
		config.setLocation(spy.getLocation());
		config.setMaxFileSize("1K");
		config.setCacheSize(100);
		config.setCacheCleaningFactor(1.0);
		spy.setConfig(config);

		// create a cache with some data
		spy.initialize(model);
		cache(spy, 100, 20);
		verify(spy, times(100)).cache(Mockito.any(BitmapId.class),
				Mockito.any(Bitmap.class));
		verify(spy, times(100))._index(Mockito.any(BitmapId.class),
				Mockito.any(IndexEntry.class));
		verify(spy, times(100))._cache(Mockito.any(BitmapId.class),
				Mockito.any(Bitmap.class));

		// get the amount of created files
		final int nrOfFiles = spy.getNumberOfFiles();
		spy.release();

		// now initialize again and check if the data is loaded
		spy.initialize(model);
		assertEquals(nrOfFiles, spy.getNumberOfFiles());
		assertEquals(0, fc.getCacheSize());

		// get the bitmap for 0, this cannot be empty now
		for (int i = 0; i < 100; i++) {
			final Bitmap bmp = spy.get(createBitmapId(i));
			assertEquals(1, bmp.getIds().length);
			assertTrue(Arrays.binarySearch(bmp.getIds(), i + 20) != -1);
		}

		// no new files were created and the cache is filled
		assertEquals(nrOfFiles, spy.getNumberOfFiles());
		assertEquals(100, spy.getCacheSize());

		// there was no need for reorganization
		verify(spy, times(0))._organizeCache();

		// the cache had to read every bitmap from disk
		verify(spy, times(100))._getFromCache(Mockito.any(BitmapId.class));
		verify(spy, times(100)).read(Mockito.any(IndexEntry.class));

		// every Bitmap had to be added to the cache, but not updated
		verify(spy, times(100)).cache(Mockito.any(BitmapId.class),
				Mockito.any(Bitmap.class));
		verify(spy, times(100))._index(Mockito.any(BitmapId.class),
				Mockito.any(IndexEntry.class));
		verify(spy, times(200))._cache(Mockito.any(BitmapId.class),
				Mockito.any(Bitmap.class));

		// let's also get some values we don't have yet
		final Bitmap emptyBitmap = model.getIndexFactory().createBitmap();
		for (int i = 100; i < 200; i++) {
			final Bitmap bmp = spy.get(createBitmapId(i));
			assertEquals(0, bmp.getIds().length);
			assertEquals(emptyBitmap, bmp);
		}
		verify(spy, times(200))._getFromCache(Mockito.any(BitmapId.class));
		verify(spy, times(100)).read(Mockito.any(IndexEntry.class));
		verify(spy, times(1))._organizeCache();
		assertEquals(100, spy.getCacheSize());

		// we need to release the spy
		spy.release();
		assertTrue(Files.deleteDir(spy.getLocation()));
	}

	/**
	 * Tests the multi-threading of a {@code FileBitmapCache}.
	 * 
	 * @throws InterruptedException
	 *             if a thread is interrupted
	 */
	@Test
	public void testMultipleThreadSafety() throws InterruptedException {
		fc.initialize(model);

		final ThreadForTesting tCacheSize = new ThreadForTesting() {

			@Override
			public void _run() {
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
				assertNotNull(fc.get(createBitmapId(0)));

				// the first call 1 isn't cached
				if (!fc.isCached(createBitmapId(1))) {
					i++;
				}
				assertNotNull(fc.get(createBitmapId(1)));
			}
		};

		final ThreadForTesting tCache = new ThreadForTesting() {

			@Override
			public void _run() {
				for (int i = 0; i < 100000; i++) {
					fc.cache(createBitmapId(0), model.getIndexFactory()
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

		// validate the threads
		tCacheSize.validate();
		tCache.validate();

		// after everything there should be one left
		assertEquals(2, fc.getCacheSize());
	}

	/**
	 * Tests the multi-threading using bulk-write.
	 * 
	 * @throws InterruptedException
	 *             if the threads cannot be found
	 */
	@Test
	public void testMultiThreadWithBulkWrite() throws InterruptedException {
		final FileBitmapIdCacheConfig config = new FileBitmapIdCacheConfig();
		config.setLocation(fc.getLocation());
		config.setCacheSize(10000);
		config.setCacheCleaningFactor(0.3);
		fc.setConfig(config);
		fc.initialize(model);

		final Bitmap empty = model.getIndexFactory().createBitmap();

		final int amount = 50000;

		final ThreadForTesting tGet = new ThreadForTesting() {
			private Random r = new Random();

			@Override
			public void _run() {
				for (int i = 0; i < 4 * amount; i++) {
					int nr = r.nextInt(amount);
					final BitmapId<?> id = new BitmapId<Integer>(nr,
							MetaIndex.class, "Classifier1");
					final Bitmap res = fc.get(id);
					final Bitmap bmp = Bitmap.createBitmap(
							model.getIndexFactory(), nr);

					/*
					 * we must get the empty one or the one created by the other
					 * thread
					 */
					assertTrue(res.toString(),
							empty.equals(res) || bmp.equals(res));
				}
			}
		};

		final ThreadForTesting tBulkLoad = new ThreadForTesting() {

			@Override
			public void _run() {
				fc.setPersistency(false);
				for (int i = 0; i < amount; i++) {
					final BitmapId<?> id = new BitmapId<Integer>(i,
							MetaIndex.class, "Classifier1");
					final Bitmap bmp = Bitmap.createBitmap(
							model.getIndexFactory(), i);

					// cache the bitmap
					fc.cache(id, bmp);
				}
				fc.setPersistency(true);
			}
		};

		// start the threads
		tGet.start();
		tBulkLoad.start();

		// join with this one
		tGet.join();
		tBulkLoad.join();

		// validate the threads
		tGet.validate();
		tBulkLoad.validate();
	}

	/**
	 * Tests the cleaning of the cache.
	 */
	@Test
	public void testCleaning() {

		// create a new configuration
		final FileBitmapIdCacheConfig fcc = new FileBitmapIdCacheConfig();
		fcc.setCacheCleaningFactor(0.5);
		fcc.setCacheSize(10);
		fcc.setLocation(fc.getLocation());
		fc.setConfig(fcc);

		// initialize the model
		fc.initialize(model);

		// add 100 values to the cache
		for (int i = 0; i < 100; i++) {
			fc.cache(createBitmapId(i), model.getIndexFactory().createBitmap());

			// use every 2nd bitmap
			if (i % 2 == 0) {
				fc.get(createBitmapId(i));
			}

			// there should never be more than 10 elements cached
			assertTrue(fc.getCacheSize() <= 10);
		}
	}

	/**
	 * Tests some sample usage scenario.
	 * 
	 * @throws InterruptedException
	 *             if a thread gets interrupted
	 */
	@Test
	public void testUsageScenario() throws InterruptedException {
		final int amountOfBitmaps = 30000;
		final int amountOfRuns = 30000;

		final FileBitmapIdCacheConfig config = new FileBitmapIdCacheConfig();
		config.setLocation(fc.getLocation());
		config.setMaxFileSize(30 * amountOfBitmaps + "b");
		config.setCacheSize((int) (amountOfBitmaps * 0.7));
		fc.setConfig(config);

		// initialize the model
		fc.initialize(model);

		// create some bitmaps
		final ThreadForTesting tIdBitmap = new ThreadForTesting() {

			@Override
			public void _run() {
				_createBitmaps(amountOfBitmaps, 0);
			}
		};
		final ThreadForTesting tIdPlus1Bitmap = new ThreadForTesting() {

			@Override
			public void _run() {
				_createBitmaps(amountOfBitmaps, 1);
			}
		};
		final ThreadForTesting tIdPlus2Bitmap = new ThreadForTesting() {

			@Override
			public void _run() {
				_createBitmaps(amountOfBitmaps, 2);
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

		// validate the threads
		tIdBitmap.validate();
		tIdPlus1Bitmap.validate();
		tIdPlus2Bitmap.validate();

		// now get the bitmaps
		final ThreadForTesting tGetBitmap1 = new ThreadForTesting() {

			@Override
			public void _run() {
				_getRndBitmaps(new Random(), amountOfRuns,
						(int) (amountOfBitmaps * 0.5));
			}
		};
		final ThreadForTesting tGetBitmap2 = new ThreadForTesting() {

			@Override
			public void _run() {
				_getRndBitmaps(new Random(), amountOfRuns,
						(int) (amountOfBitmaps * 0.5));
			}
		};

		// start the threads
		tGetBitmap1.start();
		tGetBitmap2.start();

		// join with this one
		tGetBitmap1.join();
		tGetBitmap2.join();

		// validate the threads
		tGetBitmap1.validate();
		tGetBitmap2.validate();
	}

	/**
	 * Helper method for {@link #testUsageScenario()}. The method creates
	 * {@code amount} bitmaps, whereby the id is defined by the number of it's
	 * creation. The created bitmap will have the bit sets defined by
	 * {@code id + offset}.
	 * 
	 * @param amount
	 *            the amount of bitmaps to be created
	 * @param offset
	 *            the offset of the set bit to the bitmap's id
	 */
	protected void _createBitmaps(final int amount, final int offset) {
		for (int i = 0; i < amount; i++) {

			// get the bitmap and check the cardinality
			final Bitmap bitmap = fc.get(createBitmapId(i));
			assertTrue(bitmap.determineCardinality() == 0
					|| bitmap.determineCardinality() == 1);

			// cache a newly created bitmap
			fc.cache(createBitmapId(i),
					Bitmap.createBitmap(model.getIndexFactory(), i + offset));
		}
	}

	/**
	 * Picks random bitmaps and checks the values.
	 * 
	 * @param rnd
	 *            the randomizer
	 * @param runs
	 *            the amounts of runs to check
	 * @param maxNumber
	 *            the max-id (exclusive) of a bitmap to be retrieved
	 */
	protected void _getRndBitmaps(final Random rnd, final int runs,
			final int maxNumber) {
		for (int i = 0; i < runs; i++) {
			final int nr = rnd.nextInt(maxNumber);
			final Bitmap bitmap = fc.get(createBitmapId(nr));
			final int[] ids = bitmap.getIds();

			assertEquals(1, ids.length);
			assertTrue(bitmap.toString(), ids[0] == nr || ids[0] == nr + 1
					|| ids[0] == nr + 2);
		}
	}

	/**
	 * CleanUp afterwards
	 */
	@After
	public void cleanUp() {

		// release the instance
		fc.release();
		model.release(true);

		// delete all the created files
		if (fc.getModelLocation() != null && fc.getModelLocation().exists()) {
			assertTrue("Cannot delete '" + fc.getModelLocation() + "'",
					Files.deleteDir(fc.getModelLocation()));
		}
		if (fc.getLocation() != null && fc.getLocation().exists()) {
			assertTrue("Cannot delete '" + fc.getLocation() + "'",
					Files.deleteDir(fc.getLocation()));
		}
	}
}
