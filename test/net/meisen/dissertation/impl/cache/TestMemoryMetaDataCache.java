package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.model.cache.IMetaDataCache;
import net.meisen.dissertation.model.data.MetaDataModel;
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
	 * {@link MemoryMetaDataCache#createMetaDataCollection()} and
	 * {@link MemoryMetaDataCache#createCollectionForModel(MetaDataModel)}.
	 */
	@Test
	public void testUsage() {
		MetaDataCollection collection;
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
		collection = cache.createMetaDataCollection();
		assertEquals(1, collection.size());
		assertEquals(2, collection.size("AIRLINE"));

		// initialize the model so that the metaData is set
		model.initialize();

		// the cache should not be initialized anymore
		assertFalse(((MemoryMetaDataCache) cache).isInitialized());

		// check the creation of the collection
		final MemoryMetaDataCache newCache = new MemoryMetaDataCache();
		collection = newCache
				.createCollectionForModel(model.getMetaDataModel());
		assertEquals(3, collection.size());
		assertEquals(1, collection.size("AIRLINE"));
		assertEquals(3, collection.sizeOfValues("AIRLINE"));
		assertEquals(1, collection.size("PAX"));
		assertEquals(0, collection.sizeOfValues("PAX"));
		assertEquals(1, collection.size("CREW"));
		assertEquals(1, collection.sizeOfValues("CREW"));
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
