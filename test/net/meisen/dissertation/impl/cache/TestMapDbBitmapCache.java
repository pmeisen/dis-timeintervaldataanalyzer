package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.util.Random;
import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.help.Utilities;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.MetaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests the implementation of the {@code MapDbBitmapCache}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestMapDbBitmapCache extends ModuleBasedTest {

	private TidaModel model;
	private MapDbBitmapCache mc;

	/**
	 * Initialize an instance.
	 */
	@Before
	public void create() {
		System.setProperty("test.rndUuid", UUID.randomUUID().toString());
		setModulesHolder("/net/meisen/dissertation/impl/cache/mapDbBitmapCacheModel.xml");

		// create the instance of the cache
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
		mc = modulesHolder.getModule(DefaultValues.BITMAPCACHE_ID);
		assertNotNull(mc);

		// cleanUp temp of previous tests
		Utilities.deleteDir("^tmpMapDbBitmapCacheModel\\-.*$",
				new File(System.getProperty("java.io.tmpdir")));
	}

	/**
	 * Persist some bitmaps without bulk-write.
	 */
	@Test
	public void testPersistance() {
		mc.initialize(model);

		for (int i = 0; i < 1000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(i, MetaIndex.class,
					"Classifier1");
			final Bitmap bmp = Bitmap.createBitmap(model.getIndexFactory(), i);

			// cache the bitmap
			mc.cache(id, bmp);

			// retrieve the bitmap and check the cache status
			assertEquals(bmp, mc.get(id));
		}

		// retrieve random values
		final Random rnd = new Random();
		for (int i = 0; i < 1000000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(rnd.nextInt(1000),
					MetaIndex.class, "Classifier1");
			assertNotNull(mc.get(id));
		}
	}

	/**
	 * Tests the bulk-write.
	 */
	@Test
	public void testBulkPersistance() {
		mc.initialize(model);

		// disable the persistancy
		mc.setPersistency(false);

		for (int i = 0; i < 50000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(i, MetaIndex.class,
					"Classifier1");
			final Bitmap bmp = Bitmap.createBitmap(model.getIndexFactory(), i);

			// cache the bitmap
			mc.cache(id, bmp);

			// retrieve the bitmap and check the cache status
			assertEquals(bmp, mc.get(id));
		}

		// everything should be in cache
		assertEquals(50000, mc.size());

		// enable it again
		mc.setPersistency(true);

		// retrieve random values
		final Random rnd = new Random();
		for (int i = 0; i < 1000000; i++) {
			final BitmapId<?> id = new BitmapId<Integer>(rnd.nextInt(50000),
					MetaIndex.class, "Classifier1");
			assertNotNull(mc.get(id));
		}
	}

	/**
	 * CleanUp afterwards
	 * 
	 * @throws InterruptedException
	 *             waiting is interrupted
	 */
	@After
	public void cleanUp() throws InterruptedException {

		// release the instance
		mc.release();
		model.release(true);

		// delete all the created files
		if (mc.getModelLocation() != null && mc.getModelLocation().exists()) {
			Utilities.deleteDir(mc.getModelLocation());
		}
		if (mc.getLocation() != null && mc.getLocation().exists()) {
			Utilities.deleteDir(mc.getLocation());
		}
	}
}
