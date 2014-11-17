package net.meisen.dissertation.help;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.TidaModelException;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.general.genmisc.types.Files;
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
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@RunWith(JUnitConfigurationRunner.class)
public class LoaderBasedTest extends ExceptionBasedTest {

	/**
	 * The loader to load models.
	 */
	@Autowired
	@Qualifier(DefaultValues.MODELHANDLER_ID)
	protected TidaModelHandler loader;

	private List<TidaModel> models = new ArrayList<TidaModel>();

	/**
	 * Helper method to load a specific model. The method loads the data of the
	 * model.
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
	 *            {@link TidaModel#bulkLoadDataFromDataModel()}, otherwise
	 *            {@code false}
	 * 
	 * @return the loaded model
	 */
	protected TidaModel m(final String xml, final boolean loadData) {
		final TidaModel model = loader.loadViaXslt(xml);

		if (loadData) {
			model.bulkLoadDataFromDataModel();
		}

		models.add(model);

		return model;
	}

	/**
	 * Gets the model for the specified {@code modelId}.
	 * 
	 * @param modelId
	 *            the identifier of the model to be retrieved
	 * 
	 * @return the {@code TidaModel} or {@code null} if not found
	 */
	protected TidaModel getTidaModel(final String modelId) {
		return loader.getTidaModel(modelId);
	}

	/**
	 * Make sure all the loaded modules are unloaded.
	 */
	@After
	public void unload() {
		loader.unloadAll();

		// release all models
		for (final TidaModel model : models) {
			try {
				model.release(true);
			} catch (final TidaModelException exception) {
				// ignore
			}
		}

		// clear the models
		models.clear();

		// delete the directory of the system
		Files.deleteOnExitDir(new File(loader.getDefaultLocation()));
	}
}
