package net.meisen.dissertation.performance.paper.in2014.itng;

import java.io.File;
import java.io.IOException;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleAndDbBasedTest;
import net.meisen.dissertation.help.Performance;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.persistence.FileLocation;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryFactory;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
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
public class TestPerformance extends ModuleAndDbBasedTest {

	@Autowired
	private TidaModelHandler loader;

	@Autowired
	@Qualifier(DefaultValues.QUERYFACTORY_ID)
	private IQueryFactory queryFactory;

	private String loadFromXslt(final String dbName, final String dbPath,
			final String modelPath) throws IOException {

		// initialize the database
		getDb(dbName, dbPath);

		// load the model
		return loader.loadViaXslt(modelPath).getId();
	}

	private void persistsModel(final String name, final File ml)
			throws IOException {

		// load the model and save it
		final String id = loadFromXslt(
				"tida",
				"/net/meisen/dissertation/performance/paper/in2014/itng/data/ghdataHsql.zip",
				"/net/meisen/dissertation/performance/paper/in2014/itng/model/"
						+ name + ".xml");
		final TidaModel tidaModel = loader.getTidaModel(id);

		tidaModel.bulkLoadDataFromDataModel();

		final File modelLocation = ml == null ? getModelLocation(name) : ml;
		loader.save(id, new FileLocation(modelLocation));
	}

	private void loadModel(final String name) {

		try {

			// load the persisted instances
			final File modelLocation = getModelLocation(name);
			loader.load(new FileLocation(modelLocation));
		} catch (final Exception e) {
			System.out.println(e.getMessage());
		}
	}

	private File getModelLocation(final String name) {
		return new File(
				"../test/net/meisen/dissertation/performance/paper/in2014/itng/model/"
						+ name + ".tidamodel");
	}

	/**
	 * Persist the test so that it doesn't has to be integrated again and again.
	 * 
	 * @param force
	 *            {@code force} if the persistence should be forced, otherwise
	 *            {@code false}
	 * @throws IOException
	 *             if the persistance failed
	 */
	protected void persist(final boolean force) throws IOException {

		if (force || !getModelLocation("tida-model-minute-roaring").exists()) {
			System.out.println("PERSISTING ROARING...");
			persistsModel("tida-model-minute-roaring", null);
		}
		if (force || !getModelLocation("tida-model-minute-ewah").exists()) {
			System.out.println("PERSISTING EWAH...");
			persistsModel("tida-model-minute-ewah", null);
		}
	}

	/**
	 * Make sure the models are available
	 * 
	 * @throws IOException
	 */
	@Before
	public void create() throws IOException {
		// persist(false);
	}

	/**
	 * Tests the insertion of values into a model.
	 * 
	 * @throws IOException
	 *             if the model cannot be read
	 */
	@Test
	// @Ignore
	public void testInsert() throws IOException {
		final int runs = 100;

		final int[] sizes = { 1000, 10000, 100000, 1000000 };
		final String[] model = { "ewah", "roaring" };

		// load the model and save it
		for (int k = 0; k < model.length; k++) {
			for (int i = 0; i < sizes.length; i++) {

				final long[] results = new long[runs];
				for (int l = 0; l < runs; l++) {

					final String id = loadFromXslt(
							"tida",
							"/net/meisen/dissertation/performance/paper/in2014/itng/data/ghdataHsql.zip",
							"/net/meisen/dissertation/performance/paper/in2014/itng/model/tida-model-minute-"
									+ model[k] + "-" + sizes[i] + ".xml");
					final TidaModel tidaModel = loader.getTidaModel(id);

					// measure the performance
					final Performance performance = new Performance();
					performance.start();
					tidaModel.bulkLoadDataFromDataModel();
					results[l] = performance.stop()[0];

					loader.unloadAll();
				}

				long sum = 0;
				long min = Long.MAX_VALUE;
				long max = Long.MIN_VALUE;
				for (int l = 0; l < runs; l++) {
					sum += results[l];
					min = results[l] < min ? results[l] : min;
					max = results[l] > max ? results[l] : max;
				}

				System.out.println(model[k] + "." + sizes[i] + ":");
				System.out.println("FINAL (AVG): " + (sum / runs));
				System.out.println("FINAL (MIN): " + min);
				System.out.println("FINAL (MAX): " + max);
			}
		}
	}

	/**
	 * Tests the roaring.
	 * 
	 * @throws IOException
	 *             if the model cannot be loaded
	 */
	@Test
	// @Ignore
	public void testRoaring() throws IOException {
		loadModel("tida-model-minute-roaring");
		measure("tidaModelRoaring");
		loader.unloadAll();
	}

	/**
	 * Tests the EWAH.
	 * 
	 * @throws IOException
	 *             if the model cannot be loaded
	 */
	@Test
	// @Ignore
	public void testEwah() throws IOException {
		loadModel("tida-model-minute-ewah");
		measure("tidaModelEWAH");
		loader.unloadAll();
	}

	/**
	 * Helper method for the tests.
	 * 
	 * @param model
	 *            the model to be tested
	 */
	protected void measure(final String model) {
		final int runs = 100;
		final String startDate = "01.08.2008";

		// "02.08.2008"; // "11.08.2008"; // "09.11.2008";
		final String endDate = "09.11.2008";
		System.out.println("Start...");

		// @formatter:off
		final IQuery[] queries = {
				queryFactory.parseQuery(("select timeseries of COUNT(PERSON) from ### in [" + startDate + "," + endDate + ")").replace("###", model)),
				queryFactory.parseQuery(("select timeseries of MIN(PERSON) from ### in [" + startDate + "," + endDate + ")").replace("###", model)),
				queryFactory.parseQuery(("select timeseries of MAX(PERSON) from ### in [" + startDate + "," + endDate + ")").replace("###", model)),
				queryFactory.parseQuery(("select timeseries of SUM(PERSON) from ### in [" + startDate + "," + endDate + ")").replace("###", model)),
				queryFactory.parseQuery(("select timeseries of COUNT(TASKTYPE) from ### in [" + startDate + "," + endDate + ")").replace("###", model)),
				queryFactory.parseQuery(("select timeseries of MIN(TASKTYPE) from ### in [" + startDate + "," + endDate + ")").replace("###", model)),
				queryFactory.parseQuery(("select timeseries of MAX(TASKTYPE) from ### in [" + startDate + "," + endDate + ")").replace("###", model)),
				queryFactory.parseQuery(("select timeseries of SUM(TASKTYPE) from ### in [" + startDate + "," + endDate + ")").replace("###", model)),
				queryFactory.parseQuery(("select timeseries of COUNT(WORKAREA) from ### in [" + startDate + "," + endDate + ")").replace("###", model)),
				queryFactory.parseQuery(("select timeseries of MIN(WORKAREA) from ### in [" + startDate + "," + endDate + ")").replace("###", model)),
				queryFactory.parseQuery(("select timeseries of MAX(WORKAREA) from ### in [" + startDate + "," + endDate + ")").replace("###", model)),
				queryFactory.parseQuery(("select timeseries of SUM(WORKAREA) from ### in [" + startDate + "," + endDate + ")").replace("###", model))
			  };
		// @formatter:on

		Performance performance;
		for (final IQuery query : queries) {
			long[] results = new long[runs];
			for (int i = 0; i < runs; i++) {
				performance = new Performance();

				performance.start();
				queryFactory.evaluateQuery(query, null);
				results[i] = performance.stop()[0];
			}

			long sum = 0;
			long min = Long.MAX_VALUE;
			long max = Long.MIN_VALUE;
			for (int i = 0; i < runs; i++) {
				sum += results[i];
				min = results[i] < min ? results[i] : min;
				max = results[i] > max ? results[i] : max;
			}

			System.out.println(query + " "
					+ ((SelectQuery) query).getMeasures());
			System.out.println("FINAL (AVG): " + (sum / runs));
			System.out.println("FINAL (MIN): " + min);
			System.out.println("FINAL (MAX): " + max);
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
