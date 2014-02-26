package net.meisen.dissertation.model.handler;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.help.Db;
import net.meisen.dissertation.help.DbBasedTest;
import net.meisen.dissertation.impl.persistence.FileLocation;
import net.meisen.dissertation.model.data.OfflineMode;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler.ManipulatedXml;
import net.meisen.dissertation.model.indexes.datarecord.IntervalDataHandling;
import net.meisen.dissertation.model.indexes.datarecord.MetaDataHandling;
import net.meisen.general.genmisc.types.Streams;
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
	 * Test the xml manipulation done by the loader to load from a persistor.
	 * 
	 * @throws IOException
	 *             if the file cannot be found
	 */
	@Test
	public void testXmlManipulation() throws IOException {
		final ManipulatedXml xml = loader.manipulateXmlForLoading(Streams
				.copyStreamToByteArray(getClass().getResourceAsStream(
						"/net/meisen/dissertation/config/fullModel.xml")));

		// get the modified xml as string
		final String result = Streams.readFromStream(new ByteArrayInputStream(
				xml.getXml()));

		// compare the changes made
		assertTrue(result.contains("offlinemode=\"auto\""));
		assertEquals(xml.getOldValue("offlinemode"), "false");
	}

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

	/**
	 * Tests the saving and loading using the handler.
	 * 
	 * @throws IOException
	 *             if a file cannot be read
	 */
	@Test
	public void testSaveAndLoad() throws IOException {

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
		assertEquals(OfflineMode.find(null), modelPioneerData1.getOfflineMode());

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
		assertEquals(modelPioneerData1.getOfflineMode(),
				modelPioneerData2.getOfflineMode());

		// delete the file
		assertTrue(tmpFile.delete());
	}

	/**
	 * Tests the saving and loading using the handler and an offline database.
	 * 
	 * @throws IOException
	 *             if a file cannot be read
	 */
	@Test
	public void testSaveAndLoadOffline() throws IOException {

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
		assertEquals(OfflineMode.find(null), modelPioneerData1.getOfflineMode());

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
		assertEquals(modelPioneerData1.getOfflineMode(),
				modelPioneerData2.getOfflineMode());

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
