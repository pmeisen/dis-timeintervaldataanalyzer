package net.meisen.dissertation.performance.paper.in2015.kdd;

import org.junit.Before;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.performance.implementations.model.DataHolder;
import net.meisen.dissertation.performance.implementations.similarity.IBSM;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
@RunWith(JUnitConfigurationRunner.class)
public class TestSimiliarity {

	@Autowired
	private TidaModelHandler loader;

	private DataHolder holder;
	private TidaModel model;

	@Before
	public void initialize() {
		loader.unloadAll();

		model = loader
				.loadViaXslt("/net/meisen/dissertation/performance/implementations/model/tida-model-minute.xml");
		holder = new DataHolder(model);
	}

	public void testIBSM() {
		final IBSM ibsm = new IBSM(model, holder.getRecords(),
				"INTERVAL_START", "INTERVAL_END", "PERSON");
	}
}
