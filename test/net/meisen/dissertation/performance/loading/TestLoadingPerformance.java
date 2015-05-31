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

	private static int total = 1000000;

	/**
	 * Disables all loggers.
	 */
	@BeforeClass
	public static void init() {

		// remove all of them
		Logger.getRootLogger().removeAllAppenders();
		Logger.getRootLogger().addAppender(new NullAppender());

		final File modelLocation = new File("_data/",
				"tidaDbMinuteLoadingPerformance");
		assertTrue(Files.deleteDir(modelLocation));
	}

	/**
	 * Load data from an hsql database. The data is loaded in 10k chunks.
	 * 
	 * @throws IOException
	 *             if the database cannot be opened
	 */
	@Test
	public void testLoadingPerformance10000() throws IOException {
		load(10000);
	}

	/**
	 * Load data from an hsql database. The data is loaded in 50k chunks.
	 * 
	 * @throws IOException
	 *             if the database cannot be opened
	 */
	@Test
	public void testLoadingPerformance50000() throws IOException {
		load(50000);
	}

	/**
	 * Load data from an hsql database. The data is loaded in 100k chunks.
	 * 
	 * @throws IOException
	 *             if the database cannot be opened
	 */
	@Test
	public void testLoadingPerformance100000() throws IOException {
		load(100000);
	}

	/**
	 * Load data from an hsql database. The data is loaded in 200k chunks.
	 * 
	 * @throws IOException
	 *             if the database cannot be opened
	 */
	@Test
	public void testLoadingPerformance200000() throws IOException {
		load(200000);
	}

	/**
	 * Load data from an hsql database. The data is loaded in 500k chunks.
	 * 
	 * @throws IOException
	 *             if the database cannot be opened
	 */
	@Test
	public void testLoadingPerformance500000() throws IOException {
		load(500000);
	}

	/**
	 * Load data from an hsql database. The data is loaded in 1000k chunks.
	 * 
	 * @throws IOException
	 *             if the database cannot be opened
	 */
	@Test
	public void testLoadingPerformance1000000() throws IOException {
		load(1000000);
	}

	/**
	 * Helper method to load the amount of data defind by {@code total}.
	 * 
	 * @param size
	 *            the size of the chunks to be loaded
	 * @throws IOException
	 */
	protected void load(long size) throws IOException {
		results.put("testLoadingPerformance" + size, runLoading(0, size));

		final long amount = total / size - 1;
		long sum = 0;
		for (int i = 0; i < amount; i++) {
			sum += runLoading((i + 1) * size, size);
		}

		results.put("testLoadingPerformance" + size + " - Average",
				amount == 0 ? 0 : (long) (sum / amount));
	}

	/**
	 * Helper method used to load the specified amount of data.
	 * 
	 * @param offset
	 *            the offset to pick the data from
	 * @param amount
	 *            the amount of data to be loaded
	 * 
	 * @return the amount of data loaded
	 * 
	 * @throws IOException
	 *             if the database cannot be opened
	 */
	protected long runLoading(final long offset, final long amount)
			throws IOException {
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
		config.setQuery("SELECT KEY, PERSON, TASKTYPE, WORKAREA, INTERVAL_START, INTERVAL_END FROM SMC_DATA LIMIT "
				+ amount + " OFFSET " + offset);

		// add more data
		final DataRetrieverDataSet dataSet = new DataRetrieverDataSet(
				retriever, config);
		final MultipleDataSetIterator it = new MultipleDataSetIterator(
				OfflineMode.FALSE, dataSet);

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

		// memory usage
		// Get the Java runtime
		final Runtime runtime = Runtime.getRuntime();
		// Run the garbage collector
		runtime.gc();
		// Calculate the used memory
		final long memory = runtime.totalMemory() - runtime.freeMemory();
		System.out.println("Used memory is bytes: " + memory);
		System.out.println("Used memory is megabytes: " + memory
				/ (1024l * 1024l));

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
