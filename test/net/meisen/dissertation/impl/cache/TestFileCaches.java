package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;

import java.io.IOException;

import net.meisen.dissertation.help.LoaderAndDbBasedTest;
import net.meisen.dissertation.impl.dataretriever.DbDataCollection;
import net.meisen.dissertation.impl.dataretriever.DbDataIterator;
import net.meisen.dissertation.impl.dataretriever.DbDataRetriever;
import net.meisen.dissertation.impl.dataretriever.DbQueryConfig;
import net.meisen.dissertation.impl.datasets.DataRetrieverDataSet;
import net.meisen.dissertation.impl.datasets.DataRetrieverDataSetRecord;
import net.meisen.dissertation.model.data.OfflineMode;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.MultipleDataSetIterator;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests the file-caches implemented in a more general scenario.
 * 
 * @author pmeisen
 * 
 */
public class TestFileCaches extends LoaderAndDbBasedTest {
	private static int amount = 1000;

	private static int cacheBmpSizeIterationLoad = -1;
	private static int cacheFactsSizeIterationLoad = -1;
	private static int cacheBmpSizeBulkLoad = -1;
	private static int cacheFactsSizeBulkLoad = -1;

	private TidaModel model;

	/**
	 * Initializes the database and the model.
	 * 
	 * @throws IOException
	 */
	@Before
	public void init() throws IOException {

		// open the database
		getDb("tida",
				"/net/meisen/dissertation/performance/paper/in2014/smc/data/ghdataHsql.zip");

		// get the model
		model = m("/net/meisen/dissertation/impl/cache/fileAllCacheTest.xml");
	}

	/**
	 * Test the loading of data iteratively.
	 */
	@Test
	public void testIterationLoad() {

		// get the database connection
		final DbDataRetriever retriever = (DbDataRetriever) model
				.getDataModel().getDataRetriever("db_tida");
		final DbQueryConfig config = new DbQueryConfig();
		config.setLanguage("SQL");
		config.setQuery("SELECT KEY, PERSON, TASKTYPE, WORKAREA, INTERVAL_START, INTERVAL_END FROM SMC_DATA WHERE ROWNUM() <= "
				+ amount);
		final DbDataCollection data = retriever.retrieve(config);
		final DbDataIterator it = data.iterator();

		// add the retrieved data step by step
		while (it.hasNext()) {
			model.loadRecord(new DataRetrieverDataSetRecord(it.next()));
		}

		cacheBmpSizeIterationLoad = ((FileBitmapCache) model.getBitmapCache())
				.getCacheSize();
		cacheFactsSizeIterationLoad = ((FileFactDescriptorModelSetCache) model
				.getFactsCache()).getCacheSize();

		// release everything
		data.release();
		retriever.release();
	}

	/**
	 * Tests the loading of data via bulk-load.
	 */
	@Test
	public void testBulkLoad() {

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
		model.bulkLoadData(it);

		cacheBmpSizeBulkLoad = ((FileBitmapCache) model.getBitmapCache())
				.getCacheSize();
		cacheFactsSizeBulkLoad = ((FileFactDescriptorModelSetCache) model
				.getFactsCache()).getCacheSize();

		// release everything
		it.close();
		retriever.release();
	}

	/**
	 * Check the final results, both caches should contain the same values
	 */
	@AfterClass
	public static void checkResults() {
		assertEquals(cacheBmpSizeIterationLoad, cacheBmpSizeBulkLoad);
		assertEquals(cacheFactsSizeIterationLoad, cacheFactsSizeBulkLoad);
	}
}
