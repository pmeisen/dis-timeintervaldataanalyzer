package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Collection;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;
import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.data.metadata.IMetaDataCollection;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Test;

/**
 * Tests the implementation of the {@code FileMetaDataCache}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestFileMetaDataCache extends LoaderBasedTest {

	/**
	 * Loads the {@code fileMetaDataCache}-model.
	 * 
	 * @return the loaded model
	 */
	protected TidaModel load() {
		final TidaModel m = m("/net/meisen/dissertation/impl/cache/fileMetaDataCache.xml");
		assertTrue(m.getMetaDataCache() instanceof FileMetaDataCache);

		return m;
	}

	/**
	 * Creates the {@code DataStructure} needed to add data to the
	 * {@code fileMetaDataCache}-model.
	 * 
	 * @return the created {@code DataStructure}
	 */
	protected DataStructure create() {
		return new DataStructure(new IntervalStructureEntry("START", 1),
				new IntervalStructureEntry("END", 2), new MetaStructureEntry(
						"AIRLINE", 3), new MetaStructureEntry("PAX", 4),
				new MetaStructureEntry("CREW", 5));
	}

	/**
	 * Tests the caching of the {@code FileMetaDataCache}.
	 */
	@Test
	public void testReloading() {
		TidaModel m;
		IMetaDataCollection mdc;
		Collection<Object> vals;

		// load the defined model
		m = load();
		mdc = m.getMetaDataCache().createMetaDataCollection();
		assertEquals(1, mdc.get("AIRLINE").size());
		vals = mdc.get("AIRLINE").iterator().next().getValues();
		assertEquals(2, vals.size());
		assertTrue(vals.contains("LH"));
		assertTrue(vals.contains("AB"));

		// let's add an LX airline
		m.loadRecord(create(), new SingleStaticDataSet(0, 5, "LX", 5, 10));
		mdc = m.getMetaDataCache().createMetaDataCollection();
		assertEquals(1, mdc.get("AIRLINE").size());
		vals = mdc.get("AIRLINE").iterator().next().getValues();
		assertEquals(3, vals.size());
		assertTrue(vals.contains("LH"));
		assertTrue(vals.contains("AB"));
		assertTrue(vals.contains("LX"));

		this.loader.unloadAll();

		// now load the model again, the data (LX) should be available
		m = load();
		mdc = m.getMetaDataCache().createMetaDataCollection();
		assertEquals(1, mdc.get("AIRLINE").size());
		vals = mdc.get("AIRLINE").iterator().next().getValues();
		assertEquals(3, vals.size());
		assertTrue(vals.contains("LH"));
		assertTrue(vals.contains("AB"));
		assertTrue(vals.contains("LX"));
	}

	/**
	 * Tests the persistence of the {@code FileMetaDataCache}.
	 */
	@Test
	public void testPersistence() {

		// load the defined model
		final TidaModel m = load();
		final FileMetaDataCache c = ((FileMetaDataCache) m.getMetaDataCache());
		assertEquals(0, c.getUnpersistedMetaDataCollection().size());

		// enable persistence
		m.setBulkLoad(true);
		m.loadRecord(create(), new SingleStaticDataSet(0, 5, "LX", 5, 10));
		m.loadRecord(create(), new SingleStaticDataSet(0, 5, "WK", 5, 10));
		m.loadRecord(create(), new SingleStaticDataSet(0, 5, "DL", 5, 10));
		assertEquals(2,
				c.getPersistedMetaDataCollection().sizeOfValues("AIRLINE"));
		assertEquals(3,
				c.getUnpersistedMetaDataCollection().sizeOfValues("AIRLINE"));

		// disable persistence
		m.setBulkLoad(false);
		assertEquals(5,
				c.getPersistedMetaDataCollection().sizeOfValues("AIRLINE"));
		assertEquals(0,
				c.getUnpersistedMetaDataCollection().sizeOfValues("AIRLINE"));
	}
}
