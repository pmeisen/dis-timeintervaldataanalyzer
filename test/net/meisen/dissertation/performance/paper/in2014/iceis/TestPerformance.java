package net.meisen.dissertation.performance.paper.in2014.iceis;

import static org.junit.Assert.assertEquals;

import java.lang.reflect.Constructor;
import java.util.List;
import java.util.Map;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.time.series.TimeSeriesCollection;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.parser.query.IQueryFactory;
import net.meisen.dissertation.performance.implementations.IImplementation;
import net.meisen.dissertation.performance.implementations.concrete.FilterCreatedIntervalTree;
import net.meisen.dissertation.performance.implementations.concrete.Naive;
import net.meisen.dissertation.performance.implementations.concrete.PriorFilterIntervalTree;
import net.meisen.dissertation.performance.implementations.concrete.Tida;
import net.meisen.dissertation.performance.implementations.model.DataHolder;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * The performance tests used for the ITNG paper 2015: {@code Bitmap-Based
 * On-Line Analytical Processing of Time Interval Data}
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
@RunWith(JUnitConfigurationRunner.class)
public class TestPerformance {
	private final static Logger LOG = LoggerFactory
			.getLogger(TestPerformance.class);

	private final int INIT_RUNS = 5;
	private final int RUNS = 100;

	@Autowired
	private TidaModelHandler loader;

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private IQueryFactory queryFactory;

	private DataHolder holder;

	private String[] queries;
	private int[] limits;
	private Class<? extends IImplementation>[] impls;

	@SuppressWarnings("unchecked")
	@Before
	public void initialize() {
		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/performance/implementations/model/tida-model-minute.xml");
		holder = new DataHolder(model);

		// @formatter:off
		queries = new String[] {
				"SELECT TIMESERIES OF COUNT(TASKTYPE) AS C FROM tidaModel IN [01.01.2008 00:00:00, 31.01.2008 23:59:00] WHERE WA.LOC.TYPE='Gate'",
				"SELECT TIMESERIES OF SUM(TASKTYPE) AS S ON TIME.DEF.DAY FROM tidaModel IN [01.01.2008 00:00:00, 31.01.2008 23:59:00] WHERE WORKAREA='SEN.W14'",
				"SELECT TIMESERIES OF MAX(COUNT(WORKAREA)) AS MC ON TIME.DEF.DAY FROM tidaModel IN [01.01.2008 00:00:00, 31.01.2008 23:59:00] WHERE TASKTYPE='short'",
				"SELECT TIMESERIES OF MAX(SUM(PERSON) / COUNT(PERSON)) AS MSC ON TIME.DEF.MIN5DAY FROM tidaModel IN [01.01.2008 00:00:00, 31.01.2008 23:59:00] WHERE TASKTYPE='long'",
				"SELECT TIMESERIES OF MIN(TASKTYPE) AS M ON TIME.DEF.MIN5DAY FROM tidaModel IN [01.01.2008 00:00:00, 31.01.2008 23:59:00] WHERE WA.LOC.TYPE='Ramp' OR PERSON='*9'",
				"SELECT TIMESERIES OF SUM(COUNT(TASKTYPE)) AS S ON TIME.DEF.MIN5DAY FROM tidaModel IN [01.01.2008 00:00:00, 31.01.2008 23:59:00] WHERE (WA.LOC.TYPE='Ramp' OR PERSON='*9') AND (TASKTYPE='long' OR TASKTYPE='short')" 
		};
		
		limits = new int[] {
				10,
				100,
				1000,
				10000,
				100000,
				1000000
		}; 

		
		impls = new Class[] {
				Naive.class,
				PriorFilterIntervalTree.class,
				FilterCreatedIntervalTree.class,
				Tida.class
		};
		// @formatter:on

		loader.unloadAll();
	}

	@Test
	public void testPerformance() throws Exception {

		for (final int limit : limits) {
			final List<Map<String, Object>> records = holder.getRecords(limit);

			// load the model
			final TidaModel model = loader
					.loadViaXslt("/net/meisen/dissertation/performance/implementations/model/tida-model-minute.xml");
			Tida.loadRecords(model, records);

			// run each query
			for (final String query : queries) {
				final SelectQuery parsedQuery = queryFactory
						.<SelectQuery> parseQuery(query);

				TimeSeriesCollection oldRes = null, res = null;

				for (final Class<? extends IImplementation> impl : impls) {
					final Constructor<? extends IImplementation> cstr = impl
							.getConstructor(TidaModel.class, List.class,
									int.class, int.class, IQueryFactory.class);

					final IImplementation conImpl = cstr.newInstance(model,
							records, INIT_RUNS, RUNS, queryFactory);

					if (LOG.isWarnEnabled()) {

						LOG.warn("RUNNING");
						LOG.warn("- limit: " + limit + " (select "
								+ Tida.getCount(queryFactory, parsedQuery)
								+ " of " + records.size() + ")");
						LOG.warn("- impl : " + impl.getSimpleName());
						LOG.warn("- query: " + query);
					}

					oldRes = res;
					res = conImpl.run(parsedQuery);

					// validate that we got the same results
					if (oldRes != null) {
						assertEquals(oldRes, res);
					}
				}
			}

			// unload the model again
			loader.unloadAll();
		}
	}

	/**
	 * CleanUp afterwards.
	 */
	@After
	public void cleanUp() {
		loader.unloadAll();
	}
}
