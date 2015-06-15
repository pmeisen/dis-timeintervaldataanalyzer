package net.meisen.dissertation.performance.paper.in2015.kdd;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.help.Performance;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry;
import net.meisen.dissertation.model.datastructure.IntervalStructureEntry.IntervalTypeFactory.IntervalType;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.performance.implementations.model.DataHolder;
import net.meisen.dissertation.performance.implementations.similarity.DistanceType;
import net.meisen.dissertation.performance.implementations.similarity.EventTable;
import net.meisen.dissertation.performance.implementations.similarity.ibsm.IBSM;
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

	private String strQuery;
	private int k;

	@Before
	public void initialize() {
		loader.unloadAll();

		model = loader
				.loadViaXslt("/net/meisen/dissertation/performance/implementations/model/tida-model-minute.xml");
		// /net/meisen/dissertation/performance/implementations/model/ghdataHsql.zip
		// /net/meisen/dissertation/performance/implementations/model/ghdataHsqlSmallFaked.zip
		holder = new DataHolder(
				model,
				"/net/meisen/dissertation/performance/implementations/model/ghdataHsql.zip",
				false);

		// @formatter:off
		/*
		 * SELECT TIMESERIES OF COUNT(PERSON) AS CP FROM tidaModel IN [01.01.2008, 02.01.2008)
		 * 
		 * SIMILARITY: 0,40625s, 0,40625s
		 * 0. 27.01.2008 00:00:00,000 (18120.0)
		 * 1. 13.01.2008 00:00:00,000 (19570.0)
		 * 2. 25.12.2008 00:00:00,000 (20951.0)
		 * 3. 23.03.2008 00:00:00,000 (21037.0)
		 * 4. 26.12.2008 00:00:00,000 (21092.0)
		 * 5. 20.01.2008 00:00:00,000 (21866.0)
		 * 6. 28.12.2008 00:00:00,000 (22274.0)
		 * 7. 03.02.2008 00:00:00,000 (24105.0)
		 * 8. 24.02.2008 00:00:00,000 (27514.0)
		 * 9. 09.03.2008 00:00:00,000 (28044.0)
		 * 
		 * 
		 * SELECT TIMESERIES OF SUM(PERSON) AS CP FROM tidaModel IN [01.01.2008, 02.01.2008)
		 * 
		 * SIMILARITY: 0,53125s, 0,53125s (vs. 2,734375s, 2,75000s + 0,046875s, 0,046875s)
		 * 0. 27.01.2008 00:00:00,000 (18120.0)
		 * 1. 13.01.2008 00:00:00,000 (19570.0)
		 * 2. 25.12.2008 00:00:00,000 (20951.0)
		 * 3. 23.03.2008 00:00:00,000 (21037.0)
		 * 4. 26.12.2008 00:00:00,000 (21092.0)
		 * 5. 20.01.2008 00:00:00,000 (21866.0)
		 * 6. 28.12.2008 00:00:00,000 (22274.0)
		 * 7. 03.02.2008 00:00:00,000 (24105.0)
		 * 8. 24.02.2008 00:00:00,000 (27514.0)
		 * 9. 09.03.2008 00:00:00,000 (28044.0)
		 */
		// @formatter:on
		// strQuery =
		// "SELECT TIMESERIES OF SUM(PERSON) AS CP ON TIME.DEF.HOUR FROM tidaModel IN [01.01.2008, 02.01.2008) WHERE PERSON='Paul'";
		// strQuery =
		// "SELECT TIMESERIES OF MAX(COUNT(PERSON)) AS CP ON TIME.DEF.HOUR FROM tidaModel IN [01.01.2008, 02.01.2008)";
		strQuery = "SELECT TIMESERIES OF COUNT(PERSON) AS CP FROM tidaModel IN [24.12.2008, 25.12.2008) WHERE TASKTYPE='short' AND WORKAREA='SEN.W07'";
		k = 1;
	}

	@Test
	public void testIBSMSimilarity() {
		final Performance p = new Performance();
		List<EventTable> ets;
		EventTable cmp;

		// get the query parsed and the group for the usage
		SelectQuery query = queryFactory.parseQuery(strQuery);
		final Set<Object> sels = query.getGroup().getSelectors();
		final String[] descriptors = new String[sels.size()];
		int i = 0;
		for (final Object sel : sels) {
			descriptors[i] = sel.toString();
			i++;
		}

		// get the start and end
		final DataStructure structure = model.getDataStructure();
		final IntervalStructureEntry s = structure
				.getIntervalEntryOfType(IntervalType.START);
		final IntervalStructureEntry e = structure
				.getIntervalEntryOfType(IntervalType.END);

		// create the ibsm
		final IBSM ibsm = new IBSM(model, holder.getRecords(), s.getName(),
				e.getName(), descriptors);

		p.start();
		ibsm.fillIntervalTree();
		System.out.println("ADDDATA: " + p.printSecs(p.stop()));

		p.start();
		ets = ibsm.createEventTables(query);
		System.out.println("EVENTTABLES: " + p.printSecs(p.stop()));

		p.start();
		cmp = ets.get(0);
		print(cmp.createCompareList(ets, DistanceType.MANHATTAN), k);
		System.out.println("COMPARE & SORT: " + p.printSecs(p.stop()));
	}

	@Test
	public void testTidaSimilarity() {
		final Performance p = new Performance();

		SelectQuery query = queryFactory.parseQuery(strQuery);

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
		evaluator.setSimilarity(false, false, true);
		evaluator.setTimePointThresholdStructureDistance(1500.00);

		p.start();
		final List<TimeSeriesSimilarityCollection> res = evaluator
				.evaluateSimilarity(query, k);
		System.out.println("SIMILARITY: " + p.printSecs(p.stop()));

		p.start();
		print(res, Integer.MAX_VALUE);
		System.out.println("OUTPUT: " + p.printSecs(p.stop()));

		// do the similarity on record-base
	}

	private void print(final List<TimeSeriesSimilarityCollection> list,
			final int amount) {
		int counter = 0;
		for (final TimeSeriesSimilarityCollection e : list) {
			System.out.println(counter + ". " + e.getLabel(0) + " ("
					+ e.getTotalDistance() + " => " + e.getMeasureDistance()
					+ ", " + e.getCountDistance() + ", "
					+ e.getStructureDistance() + ")");

//			System.out.println(e);

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
