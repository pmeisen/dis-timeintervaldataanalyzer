package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;

import org.junit.After;
import org.junit.Test;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.data.metadata.MetaDataCollection;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

/**
 * Tests the {@code UtilMetaDataCache}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestUtilMetaDataCache extends ModuleBasedTest {

	private TidaModel model = null;

	/**
	 * Tests the implementation of
	 * {@link UtilMetaDataCache#createCollectionForModel(MetaDataModel)} .
	 */
	@Test
	public void testCreateCollectionForModel() {

		// check the creation of the collection
		final MetaDataCollection collection = UtilMetaDataCache
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
