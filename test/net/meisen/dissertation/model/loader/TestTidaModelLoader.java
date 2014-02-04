package net.meisen.dissertation.model.loader;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests the implementation of a {@code TidaModelLoader}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
public class TestTidaModelLoader {

	@Autowired
	private TidaModelLoader loader;

	/**
	 * Tests the loading of a {@code TidaModel}.
	 */
	@Test
	public void testLoading() {
		assertNotNull(loader);

		final TidaModel model = loader.load("FullModel",
				"/net/meisen/dissertation/config/fullModel.xml");
		assertNotNull(model);
		assertEquals("myModel", model.getId());
	}

	/**
	 * Unloads all the loaded {@code TidaModel} instances.
	 */
	@After
	public void cleanUp() {
		loader.unloadAll();
	}
}
