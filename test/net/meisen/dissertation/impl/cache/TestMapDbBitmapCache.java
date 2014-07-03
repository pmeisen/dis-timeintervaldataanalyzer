package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.MetaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceId;
import net.meisen.general.genmisc.types.Files;
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

		// cleanUp temp of previous tests
		Files.deleteOnExitDir(new File(System.getProperty("java.io.tmpdir")),
				"^tmpMapDbBitmapCacheModel\\-.*$");

		System.setProperty("test.rndUuid", UUID.randomUUID().toString());
		setModulesHolder("/net/meisen/dissertation/impl/cache/mapDbBitmapCacheModel.xml");

		// create the instance of the cache
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
		mc = modulesHolder.getModule(DefaultValues.BITMAPCACHE_ID);
		assertNotNull(mc);
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
	 * Tests the reloading, and modification.
	 */
	@Test
	public void testReloading() {
		mc.initialize(model);

		// create some slices for the test
		mc.setPersistency(false);
		final List<Slice<Integer>> slices = new ArrayList<Slice<Integer>>();
		for (int i = 0; i < 100000; i++) {
			final SliceId<Integer> id = new SliceId<Integer>(i,
					MetaIndex.class, "Classifier1");
			final Slice<Integer> slice = new Slice<Integer>(id, mc);
			slice.set(i);

			// check the type of reference
			assertTrue(slice.isWeakReference());
			assertFalse(slice.isStrongReference());

			slices.add(slice);
		}
		mc.setPersistency(true);

		// check the slices and the bitmaps
		mc.setPersistency(false);
		for (int i = 0; i < 100000; i++) {
			final Slice<Integer> slice = slices.get(i);

			// check the identifiers
			final int[] ids = slice.getBitmap().getIds();
			assertEquals(1, ids.length);
			assertEquals(i, ids[0]);

			// set the next value
			slice.set(ids[0] + 1);
		}
		mc.setPersistency(true);

		// check the modification
		for (int i = 0; i < 100000; i++) {
			final Slice<Integer> slice = slices.get(i);

			// check the identifiers
			final int[] ids = slice.getBitmap().getIds();
			assertEquals(2, ids.length);
			assertEquals(i, ids[0]);
			assertEquals(i + 1, ids[1]);
		}

		// release the model and thereby everything loaded so far
		model.release();

		// make sure the cache is not initialized anymore
		assertFalse(mc.isInit());

		// reload everything
		setModulesHolder("/net/meisen/dissertation/impl/cache/mapDbBitmapCacheModel.xml");
		final TidaModel intModel = modulesHolder
				.getModule(DefaultValues.TIDAMODEL_ID);
		final MapDbBitmapCache intMc = modulesHolder
				.getModule(DefaultValues.BITMAPCACHE_ID);
		assertFalse(intModel == model);
		assertFalse(intMc == mc);

		// initialize the new cache
		intMc.initialize(intModel);

		// check the retrieved values from the cache
		for (int i = 0; i < 100000; i++) {
			final SliceId<Integer> id = new SliceId<Integer>(i,
					MetaIndex.class, "Classifier1");
			final Slice<Integer> slice = new Slice<Integer>(id, intMc);

			// check the identifiers
			final int[] ids = slice.getBitmap().getIds();
			assertEquals(2, ids.length);
			assertEquals(i, ids[0]);
			assertEquals(i + 1, ids[1]);
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
			Files.deleteOnExitDir(mc.getModelLocation());
		}
		if (mc.getLocation() != null && mc.getLocation().exists()) {
			Files.deleteOnExitDir(mc.getLocation());
		}
	}
}
