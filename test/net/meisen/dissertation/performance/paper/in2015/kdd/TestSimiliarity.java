package net.meisen.dissertation.performance.paper.in2015.kdd;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.help.Performance;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.performance.implementations.model.DataHolder;
import net.meisen.dissertation.performance.implementations.similarity.DistanceType;
import net.meisen.dissertation.performance.implementations.similarity.EventTable;
import net.meisen.dissertation.performance.implementations.similarity.ibsm.IBSM;
import net.meisen.dissertation.performance.implementations.similarity.mbsm.MBSM;
import net.meisen.dissertation.performance.implementations.similarity.tida.TimeSeriesSimilarityCollection;
import net.meisen.dissertation.performance.implementations.similarity.tida.TimeSeriesSimilarityEvaluator;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;

@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
@RunWith(JUnitConfigurationRunner.class)
public class TestSimiliarity {

	@Autowired
	private TidaModelHandler loader;

	@Autowired
	private QueryFactory queryFactory;

	private DataHolder holder;
	private TidaModel model;

	@Before
	public void initialize() {
		loader.unloadAll();

		model = loader
				.loadViaXslt("/net/meisen/dissertation/performance/implementations/model/tida-model-minute.xml");
		holder = new DataHolder(model, false);
	}

	@Test
	public void testIBSMSimilarity() {
		final Performance p = new Performance();
		List<EventTable> ets;
		EventTable cmp;

		// get the query parsed for the usage
		SelectQuery query = queryFactory
				.parseQuery("SELECT TIMESERIES OF COUNT(PERSON) AS CP FROM tidaModel IN [01.01.2008, 02.01.2008)");

		final IBSM ibsm = new IBSM(model, holder.getRecords(),
				"INTERVAL_START", "INTERVAL_END");

		p.start();
		ibsm.fillIntervalTree();
		System.out.println("ADDDATA: " + p.printSecs(p.stop()));

		p.start();
		ets = ibsm.createEventTables(query);
		System.out.println("EVENTTABLES: " + p.printSecs(p.stop()));

		p.start();
		cmp = ets.get(0);
		print(cmp.createCompareList(ets, DistanceType.MANHATTAN), 10);
		System.out.println("COMPARE & SORT: " + p.printSecs(p.stop()));
	}

	@Test
	public void testMBSMSimilarity() {
		final Performance p = new Performance();

		List<EventTable> ets;
		EventTable cmp;

		// get the query parsed for the usage
		SelectQuery query = queryFactory
				.parseQuery("SELECT TIMESERIES OF COUNT(PERSON) AS CP FROM tidaModel IN [01.01.2008, 02.01.2008)");

		final MBSM mbsm = new MBSM(model, holder.getRecords(),
				"INTERVAL_START", "INTERVAL_END");

		p.start();
		mbsm.fillIntervalTree();
		System.out.println("ADDDATA: " + p.printSecs(p.stop()));

		p.start();
		ets = mbsm.createEventTables(query);
		System.out.println("EVENTTABLES: " + p.printSecs(p.stop()));

		p.start();
		cmp = ets.get(0);
		print(cmp.createCompareList(ets, DistanceType.MANHATTAN), 10);
		System.out.println("COMPARE & SORT: " + p.printSecs(p.stop()));
	}

	@Test
	public void testTidaMeasureSimilarity() {
		final Performance p = new Performance();

		SelectQuery query = queryFactory
				.parseQuery("SELECT TIMESERIES OF COUNT(PERSON) AS CP FROM tidaModel IN [01.01.2008, 02.01.2008)");

		// load the data
		final List<Map<String, Object>> records = holder.getRecords();
		p.start();
		model.setBulkLoad(true);
		try {
			for (final Map<String, Object> record : records) {
				final SingleStaticDataSet dataSet = new SingleStaticDataSet(
						record);
				model.loadRecord(dataSet);
			}
		} finally {
			model.setBulkLoad(false);
		}
		System.out.println("ADDDATA: " + p.printSecs(p.stop()));

		// create a TimeSeries and than do it as it's done in MBSM
		final TimeSeriesSimilarityEvaluator evaluator = new TimeSeriesSimilarityEvaluator(
				model);
		p.start();
		final List<TimeSeriesSimilarityCollection> res = evaluator
				.evaluateSimilarity(query);
		System.out.println("SIMILARITY: " + p.printSecs(p.stop()));

		p.start();
		Collections.sort(res);
		print(res, 10);
		System.out.println("SORTING: " + p.printSecs(p.stop()));

		p.start();
		final List<TimeSeriesSimilarityCollection> res2 = evaluator
				.evaluateSimilarity(queryFactory
						.<SelectQuery> parseQuery("SELECT TIMESERIES OF MAX(COUNT(PERSON)) AS CP ON TIME.DEF.DAY FROM tidaModel IN [01.01.2008, 01.02.2008) GROUP BY TASKTYPE"));
		System.out.println("SIMILARITY: " + p.printSecs(p.stop()));

		p.start();
		Collections.sort(res2);
		print(res2, 10);
		System.out.println("SORTING: " + p.printSecs(p.stop()));

		// do the similarity on record-base
	}

	private void print(final List<TimeSeriesSimilarityCollection> list,
			final int amount) {
		int counter = 0;
		for (final TimeSeriesSimilarityCollection e : list) {
			System.out.println(counter + ". " + e.getLabel(0) + " ("
					+ e.getDistance() + ")");

			counter++;
			if (counter == amount) {
				break;
			}
		}
	}

	private void print(final Map<EventTable, Double> map, final int amount) {
		int counter = 0;
		for (final Entry<EventTable, Double> e : map.entrySet()) {
			System.out.println(counter + ". " + e.getKey().getLabel() + " ("
					+ e.getValue() + ")");

			counter++;
			if (counter == amount) {
				break;
			}
		}
	}
}
