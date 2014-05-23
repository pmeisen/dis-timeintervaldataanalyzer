package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.model.cache.IMetaDataCache;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.data.metadata.MetaDataCollection;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Test;

/**
 * Tests the implementation of a {@code MemoryMetaDataCache}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestMemoryMetaDataCache extends ModuleBasedTest {

	private TidaModel model = null;

	/**
	 * Tests the implementation of
	 * {@link MemoryMetaDataCache#createMetaDataCollection()} and the usage
	 * within a {@code TidaModel}.
	 */
	@Test
	public void testUsage() {
		setModulesHolder("/net/meisen/dissertation/impl/cache/memoryMetaDataCache.xml");

		// get the model and make sure it's not initialized
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
		assertFalse(model.isInitialized());

		// get the cache and check it
		final IMetaDataCache cache = modulesHolder
				.getModule(DefaultValues.METADATACACHE_ID);
		assertTrue(cache instanceof MemoryMetaDataCache);
		cache.initialize(model);

		// create the collection (should be the defined one)
		final MetaDataCollection collection = cache.createMetaDataCollection();
		assertEquals(1, collection.size());
		assertEquals(2, collection.size("AIRLINE"));

		// initialize the model so that the metaData is set
		model.initialize();

		// the cache should not be initialized anymore
		assertFalse(((MemoryMetaDataCache) cache).isInitialized());
	}

	/**
	 * If a module was loaded any created folder is deleted.
	 */
	@After
	public void cleanUp() {
		if (model != null) {
			model.release(true);
		}
	}
}
