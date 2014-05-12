package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.cache.FileCache.IndexEntry;
import net.meisen.dissertation.model.cache.IBitmapCacheConfig;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.IntervalIndex;
import net.meisen.dissertation.model.indexes.datarecord.MetaIndexDimension;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

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
		e = new IndexEntry(1000, 25000);
		e.setIndexFileNumber(100);
		e = new IndexEntry(e.bytes());

		// deserialize it
		assertEquals(1000, e.getSize());
		assertEquals(25000, e.getBitmapFilePosition());
		assertEquals(100, e.getIndexFileNumber());
	}

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

	@Test
	public void testPersistance() throws InterruptedException {
		fc.initialize(model.getId());

		for (int i = 0; i < 100000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(i,
					MetaIndexDimension.class, "Classifier1");
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
