package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.io.File;
import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;
import net.meisen.dissertation.jdbc.protocol.DataType;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests the implementation of {@code MapDbDataRecordCache}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestMapDbDataRecordCache extends ModuleBasedTest {

	private TidaModel model = null;
	private MapDbDataRecordCache recCache = null;

	/**
	 * Helper to load a {@code MemoryIdentifierCache}.
	 */
	@Before
	public void loadCache() {

		// cleanUp temp of previous tests
		Files.deleteOnExitDir(new File(System.getProperty("java.io.tmpdir")),
				"^tmpMapDbDataRecordCacheModel\\-.*$");

		// load the model and get the cache
		System.setProperty("test.rndUuid", UUID.randomUUID().toString());
		setModulesHolder("/net/meisen/dissertation/impl/cache/mapDbDataRecordCacheModel.xml");
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
		model.initialize();
		recCache = modulesHolder.getModule(DefaultValues.DATARECORDCACHE_ID);
	}

	/**
	 * Tests the caching and the reloading.
	 */
	@Test
	public void testCachingAndReloading() {
		final int amountOfRecords = 100;

		// cache some records
		recCache.setPersistency(false);
		for (int i = 0; i < amountOfRecords; i++) {
			final IDataRecord raw = new SingleStaticDataSet(i, i + 1, "" + i, i);
			model.loadRecord(raw);
		}
		recCache.setPersistency(true);

		// release the model
		model.release();

		// reload the cache
		setModulesHolder("/net/meisen/dissertation/impl/cache/mapDbDataRecordCacheModel.xml");
		assertFalse(model == modulesHolder
				.getModule(DefaultValues.TIDAMODEL_ID));
		assertFalse(recCache == modulesHolder
				.getModule(DefaultValues.DATARECORDCACHE_ID));

		// get the new cache
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
		model.initialize();
		recCache = modulesHolder.getModule(DefaultValues.DATARECORDCACHE_ID);

		// check if everything is loaded just fine
		assertNotNull(recCache.getDataTypes());
		assertEquals(5, recCache.getDataTypes().length);
		assertEquals(DataType.INT, recCache.getDataTypes()[0]);
		assertEquals(DataType.LONG, recCache.getDataTypes()[1]);
		assertEquals(DataType.LONG, recCache.getDataTypes()[2]);
		assertEquals(DataType.INT, recCache.getDataTypes()[3]);
		assertEquals(DataType.STRING, recCache.getDataTypes()[4]);
		for (int i = 0; i < amountOfRecords; i++) {
			final Object[] rec = recCache.get(i);

			assertEquals(i, rec[0]);
			assertEquals(new Long(i), rec[1]);
			assertEquals(new Long(i + 1), rec[2]);
			assertEquals(i, rec[3]);
			assertEquals("" + i, rec[4]);
		}
	}

	/**
	 * If a module was loaded any created folder is deleted.
	 */
	@After
	public void cleanUp() {
		if (recCache != null) {
			recCache.release();
		}
		if (model != null) {
			model.release(true);
		}

		if (recCache != null) {
			// delete all the created files
			if (recCache.getModelLocation() != null
					&& recCache.getModelLocation().exists()) {
				Files.deleteOnExitDir(recCache.getModelLocation());
			}
			if (recCache.getLocation() != null
					&& recCache.getLocation().exists()) {
				Files.deleteOnExitDir(recCache.getLocation());
			}
		}
	}
}
