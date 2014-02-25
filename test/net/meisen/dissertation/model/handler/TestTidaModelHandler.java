package net.meisen.dissertation.model.handler;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.help.Db;
import net.meisen.dissertation.help.DbBasedTest;
import net.meisen.dissertation.impl.persistence.FileLocation;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.IntervalDataHandling;
import net.meisen.dissertation.model.indexes.datarecord.MetaDataHandling;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests the implementation of a {@code TestTidaModelHandler}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
public class TestTidaModelHandler extends DbBasedTest {

	@Autowired
	private TidaModelHandler loader;

	/**
	 * Tests the loading of a {@code TidaModel}.
	 */
	@Test
	public void testLoadViaXslt() {
		assertNotNull(loader);

		final TidaModel model = loader.loadViaXslt("FullModel",
				"/net/meisen/dissertation/config/fullModel.xml");
		assertNotNull(model);
		assertEquals("myModel", model.getId());
	}

	@Test
	public void testSaveAndLoad() throws IOException, InterruptedException {

		// start the needed Database
		getDb("tidaPioneerData",
				"/net/meisen/dissertation/impl/hsqldbs/tidaPioneerData.zip");

		final TidaModel modelPioneerData1 = loader.loadViaXslt(
				"mh_tidaPioneerData1",
				"/net/meisen/dissertation/model/data/tidaModelPioneer.xml");
		assertEquals(MetaDataHandling.FAILONERROR,
				modelPioneerData1.getMetaDataHandling());
		assertEquals(IntervalDataHandling.FAILONNULL,
				modelPioneerData1.getIntervalDataHandling());

		// load the first module from the database
		modelPioneerData1.loadData();

		// save the data
		final File tmpFile = File.createTempFile("pioneer", ".zip");
		loader.save("mh_tidaPioneerData1", new FileLocation(tmpFile));

		// check if the file exists
		assertTrue(tmpFile.exists());
		assertTrue(tmpFile.length() > 0);

		// now load the file
		final TidaModel modelPioneerData2 = loader.load("mh_tidaPioneerData2",
				new FileLocation(tmpFile));
		assertEquals(modelPioneerData1.getNextDataId(),
				modelPioneerData2.getNextDataId());

		// delete the file
		assertTrue(tmpFile.delete());
	}

	@Test
	public void testSaveAndLoadOffline() throws IOException,
			InterruptedException {

		// start the needed Database
		final Db db = getDb("tidaPioneerData",
				"/net/meisen/dissertation/impl/hsqldbs/tidaPioneerData.zip");

		final TidaModel modelPioneerData1 = loader.loadViaXslt(
				"mh_tidaPioneerData1",
				"/net/meisen/dissertation/model/data/tidaModelPioneer.xml");
		assertEquals(MetaDataHandling.FAILONERROR,
				modelPioneerData1.getMetaDataHandling());
		assertEquals(IntervalDataHandling.FAILONNULL,
				modelPioneerData1.getIntervalDataHandling());

		// load the first module from the database
		modelPioneerData1.loadData();

		// save the data
		final File tmpFile = File.createTempFile("pioneer", ".zip");
		loader.save("mh_tidaPioneerData1", new FileLocation(tmpFile));

		// check if the file exists
		assertTrue(tmpFile.exists());
		assertTrue(tmpFile.length() > 0);

		// make sure the database is offline
		db.shutDownDb();

		// now load the file
		final TidaModel modelPioneerData2 = loader.load("mh_tidaPioneerData2",
				new FileLocation(tmpFile));
		assertEquals(modelPioneerData1.getNextDataId(),
				modelPioneerData2.getNextDataId());

		// delete the file
		assertTrue(tmpFile.delete());
	}

	/**
	 * Unloads all the loaded {@code TidaModel} instances.
	 */
	@After
	public void cleanUp() {
		loader.unloadAll();
	}
}
