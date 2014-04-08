package net.meisen.dissertation.performance.paper.in2014.smc;

import java.io.File;
import java.io.IOException;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.help.ModuleAndDbBasedTest;
import net.meisen.dissertation.help.Performance;
import net.meisen.dissertation.impl.persistence.FileLocation;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

import com.google.common.primitives.Longs;

@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
public class TestPerformance extends ModuleAndDbBasedTest {

	@Autowired
	private TidaModelHandler loader;

	private String loadFromXslt(final String dbName, final String dbPath,
			final String modelPath) throws IOException {

		// initialize the database
		getDb(dbName, dbPath);

		// load the model
		return loader.loadViaXslt(modelPath).getId();
	}

	private void persistsModel(final String name) throws IOException {

		// load the model and save it
		final String id = loadFromXslt(
				"tida",
				"/net/meisen/dissertation/performance/paper/in2014/smc/data/ghdataHsql.zip",
				"/net/meisen/dissertation/performance/paper/in2014/smc/model/"
						+ name + ".xml");
		final TidaModel tidaModel = loader.getTidaModel(id);

		tidaModel.loadData();
		tidaModel.getIndex().toStatistic();

		final File modelLocation = getModelLocation(name);
		loader.save(id, new FileLocation(modelLocation));
	}

	private TidaModel loadModel(final String name) {

		// load the persisted instances
		final File modelLocation = getModelLocation(name);
		return loader.load(new FileLocation(modelLocation));
	}

	private File getModelLocation(final String name) {
		return new File(
				"test/net/meisen/dissertation/performance/paper/in2014/smc/model/"
						+ name + ".tidamodel");
	}

	protected void persist(final boolean force) throws IOException {

		if (force || !getModelLocation("tida-model-day").exists()) {
			persistsModel("tida-model-day");
		}

		if (force || !getModelLocation("tida-model-minute").exists()) {
			persistsModel("tida-model-minute");
		}
	}

	@Before
	public void create() throws IOException {
		persist(false);
	}

	@Test
	public void testLala() throws IOException {
		// final TidaModel model = loadModel("tida-model-day");
		final TidaModel model = loadModel("tida-model-minute");

		System.out.println("Start...");
		final Performance performance = new Performance();
		performance.start();

		// TODO add performance for queries

		final long[] result = performance.stop();
		System.out.println(Longs.asList(result));
	}

	@After
	public void cleanUp() {
		loader.unloadAll();
	}

}
