package net.meisen.dissertation.model.data;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.help.DbBasedTest;
import net.meisen.dissertation.model.indexes.datarecord.IntervalDataHandling;
import net.meisen.dissertation.model.indexes.datarecord.MetaDataHandling;
import net.meisen.dissertation.model.loader.TidaModelLoader;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests the implementation of a {@code TidaModel}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
public class TestTidaModel extends DbBasedTest {

	@Autowired
	private TidaModelLoader loader;

	@Test
	public void testLoadPioneer() throws IOException, InterruptedException {

		// start the needed Database
		getDb("tidaPioneerData",
				"/net/meisen/dissertation/impl/hsqldbs/tidaPioneerData.zip");

		final TidaModel modelPioneerData1 = loader.load("mh_tidaPioneerData1",
				"/net/meisen/dissertation/model/data/tidaModelPioneer.xml");
		assertEquals(MetaDataHandling.FAILONERROR,
				modelPioneerData1.getMetaDataHandling());
		assertEquals(IntervalDataHandling.FAILONNULL,
				modelPioneerData1.getIntervalDataHandling());

		modelPioneerData1.initialize();
		modelPioneerData1.loadData();

		// save the data
		final File tmpFile = File.createTempFile("pioneer", ".zip");
		modelPioneerData1.save(tmpFile);

		// check if the file exists
		assertTrue(tmpFile.exists());
		assertTrue(tmpFile.length() > 0);

		// reload the model
		final TidaModel modelPioneerData2 = loader.load("mh_tidaPioneerData2",
				"/net/meisen/dissertation/model/data/tidaModelPioneer.xml");
		modelPioneerData2.initialize();
		assertEquals(0, modelPioneerData2.getIndex().getNextDataId());

		// now load the data
		modelPioneerData2.load(tmpFile);
		assertEquals(modelPioneerData1.getIndex().getNextDataId(),
				modelPioneerData2.getIndex().getNextDataId());

		// unload all the loaded modules
		loader.unloadAll();

		// delete the file
		assertTrue(tmpFile.delete());
	}

	// @Test
	// public void testLoadTasks() throws IOException {
	//
	// // start the needed Database
	// getDb("tidaGhTasks",
	// "/net/meisen/dissertation/impl/hsqldbs/tidaGhTasks.zip");
	//
	// final TidaModel model = loader.load("mh_tidaGhTasks",
	// "/net/meisen/dissertation/model/data/tidaModelGhTasks.xml");
	//
	// model.initialize();
	// model.loadData();
	//
	// final File tmpFile = File.createTempFile("tasks", ".zip");
	// model.save(tmpFile.toString());
	//
	// assertTrue(tmpFile.delete());
	//
	// loader.unloadAll();
	// }
}
