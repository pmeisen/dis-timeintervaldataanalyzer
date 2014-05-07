package net.meisen.dissertation.performance.loading;

import java.io.IOException;

import net.meisen.dissertation.help.LoaderAndDbBasedTest;
import net.meisen.dissertation.help.Performance;
import net.meisen.dissertation.model.data.TidaModel;

import org.apache.log4j.Logger;
import org.apache.log4j.varia.NullAppender;
import org.junit.Test;

/**
 * Tests the performance when loading data.
 * 
 * @author pmeisen
 * 
 */
public class TestLoadingPerformance extends LoaderAndDbBasedTest {

	/**
	 * Load some data from an hsql database.
	 * 
	 * @throws IOException
	 *             if the database cannot be opened
	 */
	@Test
	public void testLoading() throws IOException {

		// disable logging
		Logger.getRootLogger().removeAllAppenders();
		Logger.getRootLogger().addAppender(new NullAppender());

		final Performance performance = new Performance();
		long[] performanceRes;

		performance.start();
		getDb("tida",
				"/net/meisen/dissertation/performance/paper/in2014/smc/data/ghdataHsql.zip");
		performanceRes = performance.stop();
		System.out.println("Loading Database: "
				+ performance.printSecs(performanceRes));

		performance.start();
		final TidaModel model = m(
				"/net/meisen/dissertation/performance/loading/tida-model-minute.xml",
				false);
		performanceRes = performance.stop();
		System.out.println("Loading Model: "
				+ performance.printSecs(performanceRes));

		performance.start();
		model.loadData();
		performanceRes = performance.stop();
		System.out.println("Loading Data: ("
				+ model.getIndex().getAmountOfRecords() + " records) "
				+ performance.printSecs(performanceRes));

		System.out.println("Loading per record: ["
				+ performance.formatSecs(performance.sec(performanceRes[0]
						/ model.getIndex().getAmountOfRecords()))
				+ ", "
				+ performance.formatSecs(performance.sec(performanceRes[1]
						/ model.getIndex().getAmountOfRecords())) + "]");
		System.out.println("Loading per second: ["
				+ Math.round(model.getIndex().getAmountOfRecords()
						/ (performanceRes[0] / 1000000000.0))
				+ ", "
				+ Math.round(model.getIndex().getAmountOfRecords()
						/ (performanceRes[1] / 1000000000.0)) + "]");
	}
}
