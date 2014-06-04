package net.meisen.dissertation.performance.loading;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.meisen.dissertation.help.LoaderAndDbBasedTest;
import net.meisen.dissertation.help.Performance;
import net.meisen.dissertation.impl.dataretriever.DbDataRetriever;
import net.meisen.dissertation.impl.dataretriever.DbQueryConfig;
import net.meisen.dissertation.impl.datasets.DataRetrieverDataSet;
import net.meisen.dissertation.model.data.OfflineMode;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.MultipleDataSetIterator;
import net.meisen.general.genmisc.types.Files;

import org.apache.log4j.Logger;
import org.apache.log4j.varia.NullAppender;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests the performance when loading data.
 * 
 * @author pmeisen
 * 
 */
public class TestLoadingPerformance extends LoaderAndDbBasedTest {

	private static Map<String, Long> results = new LinkedHashMap<String, Long>();

	/**
	 * Disables all loggers.
	 */
	@BeforeClass
	public static void init() {

		// remove all of them
		Logger.getRootLogger().removeAllAppenders();
		Logger.getRootLogger().addAppender(new NullAppender());

		final File modelLocation = new File(".",
				"tidaDbMinuteLoadingPerformance");
		assertTrue(Files.deleteDir(modelLocation));
	}

	/**
	 * Load 200k of data from an hsql database. The data is loaded in 20x 10k
	 * steps.
	 * 
	 * @throws IOException
	 *             if the database cannot be opened
	 */
	@Test
	public void testLoadingPerformance10000() throws IOException {
		results.put("testLoadingPerformance10000", runLoading(10000));

		// add four times the same data
		final int amount = 19;
		long sum = 0;
		for (int i = 0; i < amount; i++) {
			sum += runLoading(10000);
		}

		results.put("testLoadingPerformance10000 - Average",
				(long) (sum / amount));
	}

	/**
	 * Load 200k of data from an hsql database. The data is loaded in 4x 50k
	 * steps.
	 * 
	 * @throws IOException
	 *             if the database cannot be opened
	 */
	@Test
	public void testLoadingPerformance50000() throws IOException {
		results.put("testLoadingPerformance50000", runLoading(50000));

		// add four times the same data
		long sum = 0;
		sum += runLoading(50000);
		sum += runLoading(50000);
		sum += runLoading(50000);

		results.put("testLoadingPerformance50000 - Average", (long) (sum / 3l));
	}

	/**
	 * Load 200k of data from an hsql database. The data is loaded in 2x 100k
	 * steps.
	 * 
	 * @throws IOException
	 *             if the database cannot be opened
	 */
	@Test
	public void testLoadingPerformance100000() throws IOException {
		results.put("testLoadingPerformance100000", runLoading(100000));

		// add four times the same data
		long sum = 0;
		sum += runLoading(100000);

		results.put("testLoadingPerformance100000 - Average", (long) (sum / 1l));
	}

	/**
	 * Load 200k of data from an hsql database. The data is loaded in 1x 200k
	 * steps.
	 * 
	 * @throws IOException
	 *             if the database cannot be opened
	 */
	@Test
	public void testLoadingPerformance200000() throws IOException {
		results.put("testLoadingPerformance200000", runLoading(200000));
	}

	/**
	 * Helper method used to load the specified amount of data.
	 * 
	 * @param amount
	 *            the amount of data to be loaded
	 * 
	 * @return the amount of data loaded
	 * 
	 * @throws IOException
	 *             if the database cannot be opened
	 */
	protected long runLoading(final int amount) throws IOException {
		System.setProperty("tidaModelMinute.dataAmount", "" + amount);

		final Performance performance = new Performance();
		long[] performanceRes;

		TidaModel model = getTidaModel("tidaDbMinuteLoadingPerformance");
		if (model == null) {
			performance.start();
			getDb("tida",
					"/net/meisen/dissertation/performance/paper/in2014/smc/data/ghdataHsql.zip");
			performanceRes = performance.stop();
			System.out.println("Loading Database: "
					+ performance.printSecs(performanceRes));

			performance.start();
			model = m(
					"/net/meisen/dissertation/performance/loading/tida-model-minute.xml",
					false);
			performanceRes = performance.stop();
			System.out.println("Loading Model: "
					+ performance.printSecs(performanceRes));
		}

		// get the currentAmount
		final int curAmount = model.getAmountOfRecords();

		// get the database connection
		final DbDataRetriever retriever = (DbDataRetriever) model
				.getDataModel().getDataRetriever("db_tida");
		final DbQueryConfig config = new DbQueryConfig();
		config.setLanguage("SQL");
		config.setQuery("SELECT KEY, PERSON, TASKTYPE, WORKAREA, INTERVAL_START, INTERVAL_END FROM SMC_DATA WHERE ROWNUM() <= "
				+ amount);

		// add more data
		final MultipleDataSetIterator it = new MultipleDataSetIterator(
				OfflineMode.FALSE, new DataRetrieverDataSet(retriever, config));

		performance.start();
		model.bulkLoadData(it);
		performanceRes = performance.stop();

		// close the iterator
		it.close();

		// print some statistics
		final int addedAmount = model.getAmountOfRecords() - curAmount;
		System.out.println("Loading Data: (" + addedAmount + " records) "
				+ performance.printSecs(performanceRes));

		System.out.println("Loading per record: ["
				+ performance.formatSecs(performance.sec(performanceRes[0]
						/ addedAmount))
				+ ", "
				+ performance.formatSecs(performance.sec(performanceRes[1]
						/ addedAmount)) + "]");
		System.out.println("Loading per second: ["
				+ Math.round(addedAmount / (performanceRes[0] / 1000000000.0))
				+ ", "
				+ Math.round(addedAmount / (performanceRes[1] / 1000000000.0))
				+ "]");

		return Math.round(addedAmount / (performanceRes[1] / 1000000000.0));
	}

	/**
	 * Print the final results.
	 */
	@AfterClass
	public static void printResults() {
		for (final Entry<String, Long> result : results.entrySet()) {
			System.out.println(result.getKey() + ": " + result.getValue());
		}
	}
}
