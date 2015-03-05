package net.meisen.dissertation.performance.implementations.similarity.tida;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import java.text.ParseException;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.impl.datasets.SingleStaticDataSet;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.genmisc.types.Objects;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests the implementation of the {@code TimeSeriesSimilarityEvaluator}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
@RunWith(JUnitConfigurationRunner.class)
public class TestTimeSeriesSimilarityEvaluator {

	@Autowired
	private TidaModelHandler loader;

	@Autowired
	private QueryFactory queryFactory;

	private TidaModel model;
	private TimeSeriesSimilarityEvaluator evaluator;

	/**
	 * Helper method to load the default model for the test. Unloads all other
	 * models prior to loading it.
	 */
	@Before
	public void loadModel() {
		loader.unloadAll();

		model = loader
				.loadViaXslt("/net/meisen/dissertation/performance/implementations/similarity/tida/tida-model-timeSeriesSimilarityEvaluator.xml");
		evaluator = new TimeSeriesSimilarityEvaluator(model);
	}

	/**
	 * Helper method to load data into the model.
	 * 
	 * @param name
	 *            the name
	 * @param ideas
	 *            the ideas
	 * @param start
	 *            the start
	 * @param end
	 *            the end
	 */
	protected void loadData(final String name, final int ideas,
			final String start, final String end) {
		try {
			loadData(name, ideas,
					Dates.parseDate(start, "dd.MM.yyyy HH:mm:ss"),
					Dates.parseDate(end, "dd.MM.yyyy HH:mm:ss"));
		} catch (final ParseException e) {
			fail(e.getMessage());
		}
	}

	/**
	 * Helper method to load data into the model.
	 * 
	 * @param name
	 *            the name
	 * @param ideas
	 *            the ideas
	 * @param start
	 *            the start
	 * @param end
	 *            the end
	 */
	protected void loadData(final String name, final int ideas,
			final Date start, final Date end) {
		final Map<String, Object> values = new HashMap<String, Object>();

		// add the values
		values.put("NAME", name);
		values.put("IDEAS", ideas);
		values.put("START", start);
		values.put("END", end);

		loadData(values);
	}

	/**
	 * Helper method to load data into the model.
	 * 
	 * @param values
	 *            the values to be loaded
	 */
	protected void loadData(final Map<String, Object> values) {
		model.loadRecord(new SingleStaticDataSet(values));
	}

	/**
	 * Helper method to create a query with the specified measure and filter
	 * (can be {@code null})
	 * 
	 * @param measure
	 *            the measure to be used
	 * @param filter
	 *            the filter, can be {@code null}
	 * 
	 * @return the parsed query
	 */
	protected SelectQuery getQuery(final String measure, final String filter) {
		return getQuery("SELECT TIMESERIES OF "
				+ measure
				+ " FROM testTimeSeriesSimilarityEvaluator IN [01.01.2015, 02.01.2015)"
				+ (Objects.empty(filter) ? "" : " WHERE " + filter));
	}

	/**
	 * Helper method to get the passed query parsed.
	 * 
	 * @param query
	 *            the query to be parsed
	 * 
	 * @return the parsed query
	 */
	protected SelectQuery getQuery(final String query) {
		return queryFactory.parseQuery(query);
	}

	/**
	 * This test is just use to initialize everything and ensure a better
	 * performance measure for all the other tests.
	 */
	@Test
	public void testInit() {
		/*
		 * This test is just use to initialize everything and ensure a better
		 * performance measure for all the other tests.
		 */
	}

	/**
	 * Tests the similarity calculated with measure-similarity only.
	 */
	@Test
	public void testMeasureSimilarity() {
		List<TimeSeriesSimilarityCollection> res;
		TimeSeriesSimilarityCollection r;

		// we are interested in measure now
		evaluator.setSimilarity(true, false, false);

		// add some data
		// @formatter:off
		// 01.01.15: (00:00) ++++++++++++ (02:00) Philipp (5)
		// 31.12.15: (00:00) ++++++++++++ (02:00) Tobias  (5)
		// @formatter:on
		loadData("Philipp", 5, "01.01.2015 00:00:00", "01.01.2015 02:00:00");
		loadData("Tobias", 5, "31.12.2015 00:00:00", "31.12.2015 02:00:00");

		// get the similar once on a global measure level
		res = evaluator.evaluateSimilarity(
				getQuery("SUM(IDEAS) AS IDEAS", null), 1);

		assertEquals(1, res.size());
		r = res.get(0);
		assertEquals(0.0, r.getMeasureDistance(), 0.0);
		assertEquals(0.0, r.getTotalDistance(), 0.0);
		// 1451520000000l == 31 Dec 2015 00:00:00 UTC
		assertEquals(1451520000000l, ((Date) r.getLabelValue(0)).getTime());

		// add another data package
		// @formatter:off
		// 01.01.15: (00:00) ++++++++++++ (02:00) Philipp (5)
		// 30.11.15: (00:10)  +++++       (01:00) Philipp (5)
		// 31.12.15: (00:00) ++++++++++++ (02:00) Tobias  (5)
		// @formatter:on
		loadData("Philipp", 5, "30.11.2015 00:10:00", "30.11.2015 01:00:00");

		// get the similar once on a filtered measure level
		res = evaluator.evaluateSimilarity(
				getQuery("SUM(IDEAS) AS IDEAS", "NAME='Philipp'"), 1);
		r = res.get(0);
		// 70 minutes (00:00 - 00:09 and 01:01 - 02:00), each 5 => 350
		assertEquals(350.0, r.getMeasureDistance(), 0.0);
		assertEquals(350.0, r.getTotalDistance(), 0.0);
		// 1448841600000l == 30 Nov 2015 00:00:00 UTC
		assertEquals(1448841600000l, ((Date) r.getLabelValue(0)).getTime());

		// add another data package
		// @formatter:off
		// 01.01.15: (00:00) ++++++++++++ (02:00) Philipp (5)
		//           (00:10)  ++++        (00:50) Edison  (5)
		// 30.11.15: (00:00) +++++        (00:50) Edison  (5) 
		//           (00:10)  +++++       (01:00) Philipp (5)
		//           (01:00)       ++++++ (02:00) Philipp (5)
		// 31.12.15: (00:00) ++++++++++++ (02:00) Tobias  (5)
		// @formatter:on
		loadData("Edison", 5, "30.11.2015 00:00:00", "30.11.2015 00:50:00");
		loadData("Philipp", 5, "30.11.2015 01:00:00", "30.11.2015 02:00:00");
		loadData("Edison", 5, "01.01.2015 00:10:00", "01.01.2015 00:50:00");

		// get the similar once on a filtered measure level
		res = evaluator.evaluateSimilarity(
				getQuery("SUM(IDEAS) AS IDEAS", null), 1);
		r = res.get(0);
		// 1 minute (01:00:00), each 5 as value => 5
		assertEquals(5.0, r.getMeasureDistance(), 0.0);
		assertEquals(5.0, r.getTotalDistance(), 0.0);
		// 1448841600000l == 30 Nov 2015 00:00:00 UTC
		assertEquals(1448841600000l, ((Date) r.getLabelValue(0)).getTime());

		// fire one with dimensions
		// @formatter:off
		// 01.01.15: (00:00) ++++++++++++ (02:00) Philipp (5)
		//           (00:10)  ++++        (00:50) Edison  (5)
		// 30.11.15: (00:00) +++++        (00:50) Edison  (5) 
		//           (00:10)  +++++       (01:00) Philipp (5)
		//           (01:00)       ++++++ (02:00) Philipp (5)
		// 31.12.15: (00:00) ++++++++++++ (02:00) Tobias  (5)
		// @formatter:on
		res = evaluator.evaluateSimilarity(
				getQuery("MAX(SUM(IDEAS)) AS IDEAS ON TIME.DEF.HOUR", null), 1);

		assertEquals(1, res.size());
		r = res.get(0);
		// 1 minute (01:00:00), each 5 for the whole hour (* 60) as value => 300
		assertEquals(300.0, r.getMeasureDistance(), 0.0);
		assertEquals(300.0, r.getTotalDistance(), 0.0);
		assertEquals("R20151130_0000_0059", r.getLabelValue(0));
	}

	/**
	 * Tests the similarity calculated with count-similarity only.
	 */
	@Test
	public void testCountSimilarity() {
		List<TimeSeriesSimilarityCollection> res;
		TimeSeriesSimilarityCollection r;

		// we are interested in measure now
		evaluator.setSimilarity(false, true, false);

		// add some data
		// @formatter:off
		// 01.01.15: (00:00) ++++++++++++ (02:00) Philipp (5)
		// 31.12.15: (00:00) ++++++++++++ (02:00) Tobias  (5)
		// @formatter:on
		loadData("Philipp", 15, "01.01.2015 00:00:00", "01.01.2015 02:00:00");
		loadData("Tobias", 5, "31.12.2015 00:00:00", "31.12.2015 02:00:00");

		// get the similar once on a global measure level
		res = evaluator.evaluateSimilarity(
				getQuery("MAX(IDEAS) AS IDEAS ON TIME.DEF.HOUR", null), 1);

		assertEquals(1, res.size());
		r = res.get(0);
		assertEquals(0.0, r.getCountDistance(), 0.0);
		assertEquals(0.0, r.getTotalDistance(), 0.0);
		// 1451520000000l == 31 Dec 2015 00:00:00 UTC
		assertEquals(1451520000000l, ((Date) r.getLabelValue(0)).getTime());

		// add another data package
		// @formatter:off
		// 01.01.15: (00:00) ++++++++++++ (02:00) Philipp (5)
		// 30.11.15: (00:10)  +++++       (01:00) Philipp (5)
		// 31.12.15: (00:00) ++++++++++++ (02:00) Tobias  (5)
		// @formatter:on
		loadData("Philipp", 5, "30.11.2015 00:10:00", "30.11.2015 01:00:00");

		// get the similar once on a filtered measure level
		res = evaluator.evaluateSimilarity(
				getQuery("MIN(IDEAS) AS IDEAS ON TIME.DEF.HOUR",
						"NAME='Philipp'"), 1);
		r = res.get(0);
		// 70 minutes (00:00 - 00:09 and 01:01 - 02:00)
		assertEquals(70.0, r.getCountDistance(), 0.0);
		assertEquals(70.0, r.getTotalDistance(), 0.0);
		// 1448841600000l == 30 Nov 2015 00:00:00 UTC
		assertEquals(1448841600000l, ((Date) r.getLabelValue(0)).getTime());

		// add another data package
		// @formatter:off
		// 01.01.15: (00:00) ++++++++++++ (02:00) Philipp (5)
		//           (00:10)  ++++         (00:50) Edison  (5)
		// 30.11.15: (00:00) +++++         (00:50) Edison  (5) 
		//           (00:10)  +++++        (01:00) Philipp (5)
		//           (01:00)       ++++++ (02:00) Philipp (5)
		// 31.12.15: (00:00) ++++++++++++ (02:00) Tobias  (5)
		// @formatter:on
		loadData("Edison", 5, "30.11.2015 00:00:00", "30.11.2015 00:50:00");
		loadData("Philipp", 5, "30.11.2015 01:00:00", "30.11.2015 02:00:00");
		loadData("Edison", 5, "01.01.2015 00:10:00", "01.01.2015 00:50:00");

		// get the similar once on a filtered measure level
		res = evaluator.evaluateSimilarity(
				getQuery("SUM(IDEAS) AS IDEAS ON TIME.DEF.HOUR", null), 1);

		assertEquals(1, res.size());
		r = res.get(0);
		// 1 minute (01:00:00)
		assertEquals(1.0, r.getCountDistance(), 0.0);
		assertEquals(1.0, r.getTotalDistance(), 0.0);
		// 1448841600000l == 30 Nov 2015 00:00:00 UTC
		assertEquals(1448841600000l, ((Date) r.getLabelValue(0)).getTime());
	}

	/**
	 * Tests some scenarios with structural similarity.
	 */
	@Test
	public void testStructureSimilarity1() {
		List<TimeSeriesSimilarityCollection> res;
		TimeSeriesSimilarityCollection r;

		// we are interested in measure now
		evaluator.setSimilarity(false, false, true);

		// add some data
		// @formatter:off
		// 01.01.15: (00:00) ++++++       (01:00) Philipp (3)
		// 01.01.15: (00:00) ++++++       (01:00) Philipp (7)
		// 31.12.15: (00:00) ++++++       (01:00) Tobias  (13)
		// 31.12.15: (00:00) ++++++       (01:00) Tobias  (17)
		// @formatter:on
		loadData("Philipp", 3, "01.01.2015 00:00:00", "01.01.2015 01:00:00");
		loadData("Philipp", 7, "01.01.2015 00:00:00", "01.01.2015 01:00:00");
		loadData("Tobias", 13, "31.12.2015 00:00:00", "31.12.2015 01:00:00");
		loadData("Tobias", 17, "31.12.2015 00:00:00", "31.12.2015 01:00:00");

		// get the similar once on a filtered measure level
		res = evaluator.evaluateSimilarity(
				getQuery("SUM(IDEAS) AS IDEAS ON TIME.DEF.HOUR", null), 1);

		assertEquals(1, res.size());
		r = res.get(0);
		assertEquals(0.0, r.getStructureDistance(), 0.0);
		assertEquals(0.0, r.getTotalDistance(), 0.0);
		// 1451520000000l == 31 Dec 2015 00:00:00 UTC
		assertEquals(1451520000000l, ((Date) r.getLabelValue(0)).getTime());

		// add some data
		// @formatter:off
		// 01.01.15: (00:00) ++++++       (01:00) Philipp (3)
		// 01.01.15: (00:00) ++++++       (01:00) Philipp (7)
		// 01.01.15: (00:00) ++++++++++++ (02:00) Philipp (10)
		// 01.01.15: (00:00) +            (00:10) Philipp (10)
		// 31.12.15: (00:00) ++++++       (01:00) Tobias  (13)
		// 31.12.15: (00:00) ++++++       (01:00) Tobias  (17)
		// 31.12.15: (00:00) ++++++       (01:00) Tobias  (1)
		// 31.12.15: (01:01)       ++++++ (02:00) Tobias  (2)
		// 31.12.15: (00:00) +            (00:10) Philipp (10)
		// @formatter:on
		loadData("Philipp", 10, "01.01.2015 00:00:00", "01.01.2015 02:00:00");
		loadData("Philipp", 10, "01.01.2015 00:00:00", "01.01.2015 00:10:00");
		loadData("Tobias", 1, "31.12.2015 00:00:00", "31.12.2015 02:00:00");
		loadData("Tobias", 2, "31.12.2015 01:01:00", "31.12.2015 02:00:00");
		loadData("Philipp", 10, "31.12.2015 00:00:00", "31.12.2015 00:10:00");

		// get the similar once on a filtered measure level
		res = evaluator.evaluateSimilarity(
				getQuery("MAX(SUM(IDEAS)) AS IDEAS ON TIME.DEF.HOUR", null), 1);

		// assertEquals(1, res.size());
		r = res.get(0);
		assertEquals(3.2, r.getStructureDistance(), 0.0);
		assertEquals(3.2, r.getTotalDistance(), 0.0);
		// 1451520000000l == 31 Dec 2015 00:00:00 UTC
		assertEquals(1451520000000l, ((Date) r.getLabelValue(0)).getTime());
	}

	/**
	 * Tests some scenarios with structural similarity.
	 */
	@Test
	public void testStructureSimilarity2() {
		List<TimeSeriesSimilarityCollection> res;
		TimeSeriesSimilarityCollection r;

		// we are interested in measure now
		evaluator.setSimilarity(false, false, true);

		// add some data
		// @formatter:off
		// 01.01.15: (00:00) ++++++       (01:00) Philipp (3)
		// 01.01.15: (01:01)       +++    (01:30) Philipp (7)
		// 01.01.15: (01:20)         ++++ (02:00) Philipp (7)
		// 31.12.15: (00:00) ++++++       (01:00) Tobias  (13)
		// 31.12.15: (01:01)       +++    (01:30) Tobias  (17)
		// 31.12.15: (01:10)        +++++ (02:00) Tobias  (17)
		// @formatter:on
		loadData("Philipp", 3, "01.01.2015 00:00:00", "01.01.2015 01:00:00");
		loadData("Philipp", 7, "01.01.2015 01:01:00", "01.01.2015 01:30:00");
		loadData("Philipp", 7, "01.01.2015 01:20:00", "01.01.2015 02:00:00");
		loadData("Tobias", 13, "31.12.2015 00:00:00", "31.12.2015 01:00:00");
		loadData("Tobias", 17, "31.12.2015 01:01:00", "31.12.2015 01:30:00");
		loadData("Tobias", 17, "31.12.2015 01:10:00", "31.12.2015 02:00:00");

		// get the similar once on a filtered measure level
		res = evaluator.evaluateSimilarity(
				getQuery("SUM(IDEAS) AS IDEAS ON TIME.DEF.HOUR", null), 1);

		assertEquals(1, res.size());
		r = res.get(0);
		assertEquals(0.0, r.getStructureDistance(), 0.0);
		assertEquals(0.0, r.getTotalDistance(), 0.0);
		// 1451520000000l == 31 Dec 2015 00:00:00 UTC
		assertEquals(1451520000000l, ((Date) r.getLabelValue(0)).getTime());

		// add some data
		// @formatter:off
		// 01.01.15: (00:00) ++++++       (01:00) Philipp (3)
		// 01.01.15: (01:01)       +++    (01:30) Philipp (7)
		// 01.01.15: (01:20)         ++++ (02:00) Philipp (7)
		// 31.12.15: (00:00) ++++++       (01:00) Tobias  (13)
		// 31.12.15: (01:01)       +++    (01:30) Tobias  (17)
		// 31.12.15: (01:10)        +++++ (02:00) Tobias  (17)
		// 31.12.15: (01:30)          +++ (02:00) Tobias  (17)
		// @formatter:on
		loadData("Tobias", 17, "31.12.2015 01:30:00", "31.12.2015 02:00:00");

		// get the similar once on a filtered measure level
		res = evaluator.evaluateSimilarity(
				getQuery("SUM(IDEAS) AS IDEAS ON TIME.DEF.HOUR", null), 364);

		assertEquals(364, res.size());
		for (int i = 0; i < 363; i++) {
			r = res.get(i);
			assertEquals(3.0, r.getStructureDistance(), 0.0);
			assertEquals(3.0, r.getTotalDistance(), 0.0);
		}
		r = res.get(363);
		assertEquals(3.5, r.getStructureDistance(), 0.0);
		assertEquals(3.5, r.getTotalDistance(), 0.0);
		// 1451520000000l == 31 Dec 2015 00:00:00 UTC
		assertEquals(1451520000000l, ((Date) r.getLabelValue(0)).getTime());

		for (TimeSeriesSimilarityCollection a : res) {
			System.out.println((Date) a.getLabelValue(0));
			System.out.println(a);
		}
	}

	/**
	 * Cleans-up after every test.
	 */
	@After
	public void cleanUp() {
		if (model != null) {
			model.release(true);
			loader.unloadAll();
		}
	}
}
