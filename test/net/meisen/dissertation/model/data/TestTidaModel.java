package net.meisen.dissertation.model.data;

import static org.junit.Assert.assertEquals;

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

		final TidaModel model = loader.load("mh_tidaPioneerData",
				"/net/meisen/dissertation/model/data/tidaModelPioneer.xml");
		assertEquals(MetaDataHandling.FAILONERROR, model.getMetaDataHandling());
		assertEquals(IntervalDataHandling.FAILONNULL,
				model.getIntervalDataHandling());

		model.initialize();

		loader.unloadAll();
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
	//
	// loader.unloadAll();
	// }
}
