package net.meisen.dissertation.help;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Helper class to write loader dependent or module dependent test.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
@RunWith(JUnitConfigurationRunner.class)
public class LoaderBasedTest extends ExceptionBasedTest {

	/**
	 * The loader to load models.
	 */
	@Autowired
	@Qualifier(DefaultValues.HANDLER_ID)
	protected TidaModelHandler loader;

	/**
	 * Helper method to load a specific model.
	 * 
	 * @param xml
	 *            the xml file to be loaded
	 * 
	 * @return the loaded model
	 */
	protected TidaModel m(final String xml) {
		return m(xml, true);
	}

	/**
	 * Helper method to load a specific model and control the loading of the
	 * data.
	 * 
	 * @param xml
	 *            the xml file to be loaded
	 * @param loadData
	 *            {@code true} if all data should be loaded (see
	 *            {@link TidaModel#loadData()}, otherwise {@code false}
	 * 
	 * @return the loaded model
	 */
	protected TidaModel m(final String xml, final boolean loadData) {
		final TidaModel model = loader.loadViaXslt(xml);

		if (loadData) {
			model.loadData();
		}

		return model;
	}

	/**
	 * Make sure all the loaded modules are unloaded.
	 */
	@After
	public void unload() {
		loader.unloadAll();
	}
}
