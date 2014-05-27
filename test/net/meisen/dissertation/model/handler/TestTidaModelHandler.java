package net.meisen.dissertation.model.handler;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.exceptions.TidaModelHandlerException;
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
	 * Tests the exception to be thrown when loading using a {@code null} as
	 * classpath.
	 */
	@Test
	public void testExceptionNullClassPath() {
		thrown.expect(TidaModelHandlerException.class);
		thrown.expectMessage("configuration for the moduleHolder isn't defined");

		loader.loadViaXslt((String) null);
	}

	/**
	 * Tests the exception to be thrown when loading using an invalid classpath.
	 */
	@Test
	public void testExceptionInvalidClassPath() {
		thrown.expect(TidaModelHandlerException.class);
		thrown.expectMessage("configuration for the moduleHolder isn't defined");

		loader.loadViaXslt("/i/never/exist/???/");
	}

	/**
	 * Tests the exception to be thrown when loading using a {@code null} as
	 * file.
	 */
	@Test
	public void testExceptionNullFile() {
		thrown.expect(TidaModelHandlerException.class);
		thrown.expectMessage("configuration for the moduleHolder isn't defined");

		loader.loadViaXslt((File) null);
	}

	/**
	 * Tests the exception to be thrown when loading using an invalid file.
	 */
	@Test
	public void testExceptionInvalidFile() {
		thrown.expect(TidaModelHandlerException.class);
		thrown.expectMessage("Failed to load the configuration file '???'");

		loader.loadViaXslt(new File("???"));
	}

	/**
	 * Tests the exception to be thrown when loading using a {@code null} as
	 * stream.
	 */
	@Test
	public void testExceptionNullStream() {
		thrown.expect(TidaModelHandlerException.class);
		thrown.expectMessage("configuration for the moduleHolder isn't defined");

		loader.loadViaXslt((InputStream) null);
	}

	/**
	 * Tests the exception to be thrown if an identifier for a moduleHolder is
	 * used multiple times.
	 */
	@Test
	public void testExceptionUsingIdTwice() {
		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/config/fullModel.xml");
		assertTrue("Reached this point", true);

		boolean exception = false;
		try {
			loader.loadViaXslt("/net/meisen/dissertation/config/fullModel.xml");
		} catch (final TidaModelHandlerException e) {
			assertTrue(e.getMessage().contains(
					"model with the identifier 'fullModel' is already defined"));
			exception = true;
		}
		assertTrue(exception);

		model.release(true);
	}

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

		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/config/fullModel.xml");
		assertNotNull(model);
		assertEquals("fullModel", model.getId());

		model.release(true);
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

		final TidaModel modelPioneerData1 = loader
				.loadViaXslt("/net/meisen/dissertation/model/data/tidaModelPioneer.xml");
		assertEquals(MetaDataHandling.FAILONERROR,
				modelPioneerData1.getMetaDataHandling());
		assertEquals(IntervalDataHandling.FAILONNULL,
				modelPioneerData1.getIntervalDataHandling());
		assertEquals(OfflineMode.find(null), modelPioneerData1.getOfflineMode());

		// load the first module from the database
		modelPioneerData1.bulkLoadDataFromDataModel();

		// save the data
		final File tmpFile = File.createTempFile("pioneer", ".zip");
		loader.save(modelPioneerData1.getId(), new FileLocation(tmpFile));

		// check if the file exists
		assertTrue(tmpFile.exists());
		assertTrue(tmpFile.length() > 0);

		// keep some information of the model
		final int nextId = modelPioneerData1.getNextDataId();
		final OfflineMode offlineMode = modelPioneerData1.getOfflineMode();

		// now load the file
		loader.unload(modelPioneerData1.getId());
		modelPioneerData1.release(true);

		final TidaModel modelPioneerData2 = loader.load(new FileLocation(
				tmpFile));
		assertEquals(nextId, modelPioneerData2.getNextDataId());
		assertEquals(offlineMode, modelPioneerData2.getOfflineMode());
		modelPioneerData2.release(true);

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

		final TidaModel modelPioneerData1 = loader
				.loadViaXslt("/net/meisen/dissertation/model/data/tidaModelPioneer.xml");
		assertEquals(MetaDataHandling.FAILONERROR,
				modelPioneerData1.getMetaDataHandling());
		assertEquals(IntervalDataHandling.FAILONNULL,
				modelPioneerData1.getIntervalDataHandling());
		assertEquals(OfflineMode.find(null), modelPioneerData1.getOfflineMode());

		// load the first module from the database
		modelPioneerData1.bulkLoadDataFromDataModel();

		// save the data
		final File tmpFile = File.createTempFile("pioneer", ".zip");
		loader.save(modelPioneerData1.getId(), new FileLocation(tmpFile));

		// check if the file exists
		assertTrue(tmpFile.exists());
		assertTrue(tmpFile.length() > 0);

		// make sure the database is offline
		db.shutDownDb();

		// keep some information of the model
		final int nextId = modelPioneerData1.getNextDataId();
		final OfflineMode offlineMode = modelPioneerData1.getOfflineMode();

		// unload and cleanup
		loader.unload(modelPioneerData1.getId());
		modelPioneerData1.release(true);

		// now load the file
		final TidaModel modelPioneerData2 = loader.load(new FileLocation(
				tmpFile));
		assertEquals(nextId, modelPioneerData2.getNextDataId());
		assertEquals(offlineMode, modelPioneerData2.getOfflineMode());

		// delete the file
		assertTrue(tmpFile.delete());
		modelPioneerData2.release(true);
	}

	/**
	 * Unloads all the loaded {@code TidaModel} instances.
	 */
	@After
	public void cleanUp() {
		loader.unloadAll();
	}
}
