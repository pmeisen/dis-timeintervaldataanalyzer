package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;
import net.meisen.dissertation.model.cache.IMetaDataCache;
import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.data.metadata.IMetaDataCollection;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
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
		model.initialize();

		// get the cache and check it
		final IMetaDataCache cache = modulesHolder
				.getModule(DefaultValues.METADATACACHE_ID);
		assertTrue(cache instanceof MemoryMetaDataCache);
		cache.initialize(model);

		// create the collection (should be the defined one)
		final IMetaDataCollection collection = cache.createMetaDataCollection();
		assertEquals(1, collection.size());
		assertEquals(2, collection.sizeOfValues("AIRLINE"));
		assertEquals(0, collection.sizeOfValues("CREW"));
		assertEquals(0, collection.sizeOfValues("PAX"));

		// let's add some values
		final DataStructure dataStructure = new DataStructure(
				new IntervalStructureEntry("START", 1),
				new IntervalStructureEntry("END", 2), new MetaStructureEntry(
						"AIRLINE", 3), new MetaStructureEntry("PAX", 4),
				new MetaStructureEntry("CREW", 5));

		model.loadRecord(dataStructure, new SingleStaticDataSet(0, 5, "LH", 5,
				10));
		assertEquals(3, collection.size());
		assertEquals(2, collection.sizeOfValues("AIRLINE"));
		assertEquals(1, collection.sizeOfValues("CREW"));
		assertEquals(1, collection.sizeOfValues("PAX"));

		model.loadRecord(dataStructure, new SingleStaticDataSet(0, 5, "AB", 7,
				null));
		assertEquals(3, collection.size());
		assertEquals(2, collection.sizeOfValues("AIRLINE"));
		assertEquals(2, collection.sizeOfValues("CREW"));
		assertEquals(2, collection.sizeOfValues("PAX"));

		model.loadRecord(dataStructure, new SingleStaticDataSet(0, 5, null, 5,
				null));
		assertEquals(3, collection.size());
		assertEquals(3, collection.sizeOfValues("AIRLINE"));
		assertEquals(2, collection.sizeOfValues("CREW"));
		assertEquals(2, collection.sizeOfValues("PAX"));
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
