package net.meisen.dissertation.performance.loading;

import java.io.IOException;

import net.meisen.dissertation.help.LoaderAndDbBasedTest;
import net.meisen.dissertation.help.Performance;
import net.meisen.dissertation.model.data.TidaModel;

import org.apache.log4j.Logger;
import org.apache.log4j.varia.NullAppender;
import org.junit.Test;

import com.google.common.primitives.Longs;

public class TestLoadingPerformance extends LoaderAndDbBasedTest {

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
		System.out.println("Loading Database: " + Longs.asList(performanceRes));

		performance.start();
		final TidaModel model = m(
				"/net/meisen/dissertation/performance/loading/tida-model-minute.xml",
				false);
		performanceRes = performance.stop();
		System.out.println("Loading Model: " + Longs.asList(performanceRes));

		performance.start();
		model.loadData();
		performanceRes = performance.stop();
		System.out.println("Loading Data: ("
				+ model.getIndex().getAmountOfRecords() + " records)"
				+ Longs.asList(performanceRes));

		System.out.println("Loading per record: ["
				+ (performanceRes[0] / model.getIndex().getAmountOfRecords())
				+ ", "
				+ (performanceRes[1] / model.getIndex().getAmountOfRecords())
				+ "]");

		// print the resulting values
		System.out.println(model.getIndex().toStatistic());
	}
}
