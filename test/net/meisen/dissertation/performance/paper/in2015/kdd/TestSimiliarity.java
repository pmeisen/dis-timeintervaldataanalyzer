package net.meisen.dissertation.performance.paper.in2015.kdd;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;

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
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Before;
import org.junit.Ignore;
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
		holder = new DataHolder(model);
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
		System.out.println("COMPARE: " + p.printSecs(p.stop()));
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
		System.out.println("COMPARE: " + p.printSecs(p.stop()));
	}
	
//	@Ignore
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
				
				System.out.print(record.get("PERSON").toString().trim() + ";");
				System.out.print(record.get("TASKTYPE").toString().trim() + ";");
				System.out.print(record.get("WORKAREA").toString().trim() + ";");
				System.out.print(Dates.formatDate((Date) record.get("INTERVAL_START"), "dd.MM.yyyy HH:mm:ss") + ";");
				System.out.print(Dates.formatDate((Date) record.get("INTERVAL_END"), "dd.MM.yyyy HH:mm:ss") + ";");
				System.out.print((int) (Math.random() * 100) + ";");
				System.out.println(Math.random());
				
				final SingleStaticDataSet dataSet = new SingleStaticDataSet(
						record);
				model.loadRecord(dataSet);
			}
		} finally {
			model.setBulkLoad(false);
		}
		p.stop();
		
		// create a TimeSeries and than do it as it's done in MBSM
		
		// do the similarity on record-base
	}

	private void print(final TreeMap<Double, EventTable> map, final int amount) {
		int counter = 0;
		for (final Entry<Double, EventTable> e : map.entrySet()) {
			System.out.println(counter + ". " + e.getValue().getLabel() + " ("
					+ e.getKey() + ")");

			counter++;
			if (counter == amount) {
				break;
			}
		}
	}
}
