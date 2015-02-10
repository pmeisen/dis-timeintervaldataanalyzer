package net.meisen.dissertation.performance.implementations.similarity.mbsm;

import static org.junit.Assert.assertEquals;

import java.util.List;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.performance.implementations.model.DataHolder;
import net.meisen.dissertation.performance.implementations.similarity.DistanceType;
import net.meisen.dissertation.performance.implementations.similarity.EventTable;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the matching using {@code MBSM}. MBSM is an extension of {@code IBSM}.
 * The method uses the creation of TimeSeries to generally match
 * {@code EventTable}. Instead of creating the count-measure it creates any
 * measured value within the time-series and supports groups as well as
 * filtering.<br/>
 * <br/>
 * The idea of the implementation is an enhanced version of the IBMS (without
 * reductions) from Kotsifakos et al., 2013, <i>IBSM: Interval-Based Sequence
 * Matching</i>.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
@RunWith(JUnitConfigurationRunner.class)
public class TestMBSM {
	private final static String modelPath = "/net/meisen/dissertation/performance/implementations/model/tida-model-minute.xml";

	@Autowired
	private TidaModelHandler loader;

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private QueryFactory queryFactory;

	private TidaModel model;

	/**
	 * Initialize the model to be used
	 */
	@Before
	public void initialize() {
		loader.unloadAll();

		model = loader.loadViaXslt(modelPath);
	}

	/**
	 * Tests the distance measurement.
	 */
	@Test
	public void testDistance() {
		final DataHolder holder = new DataHolder(
				model,
				"/net/meisen/dissertation/performance/implementations/model/ghdataHsqlSmallFaked.zip");

		// get the query parsed for the usage
		SelectQuery query = queryFactory
				.parseQuery("SELECT TIMESERIES OF COUNT(PERSON) AS CP FROM tidaModel IN [02.01.2008, 03.01.2008) WHERE PERSON='Paul'");

		final MBSM tsm = new MBSM(model, holder.getRecords(10),
				"INTERVAL_START", "INTERVAL_END");

		// fill the tree
		tsm.fillIntervalTree();

		// create the event-table
		final List<EventTable> ets = tsm.createEventTables(query);

		// compare
		final DistanceType m = DistanceType.MANHATTAN;
		final DistanceType e = DistanceType.EUCLID;
		assertEquals(0.0, ets.get(0).distance(ets.get(1), e), 0.0);
		assertEquals(0.0, ets.get(0).distance(ets.get(1), m), 0.0);
		assertEquals(Math.sqrt(61.0), ets.get(0).distance(ets.get(2), e), 0.0);
		assertEquals(Math.sqrt(209.0), ets.get(0).distance(ets.get(3), e), 0.0);
		assertEquals(Math.sqrt(209.0), ets.get(0).distance(ets.get(4), e), 0.0);
	}
}
