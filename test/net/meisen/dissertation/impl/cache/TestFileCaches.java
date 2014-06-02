package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
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
import net.meisen.general.genmisc.types.Files;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Tests the file-caches implemented in a more general scenario.
 * 
 * @author pmeisen
 * 
 */
public class TestFileCaches {

	/**
	 * Tests the {@code default} and the {@code bulk} loading strategy.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class TestLoadingOfFileCaches extends LoaderAndDbBasedTest {
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
		 *             if the data cannot be loaded
		 */
		@Before
		public void init() throws IOException {
			assertTrue(Files
					.deleteDir(new File(".", "fileAllCacheLoadingTest")));

			// open the database
			getDb("tida",
					"/net/meisen/dissertation/performance/paper/in2014/smc/data/ghdataHsql.zip");

			// get the model
			model = m("/net/meisen/dissertation/impl/cache/fileAllCacheLoadingTest.xml");
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

			cacheBmpSizeIterationLoad = ((FileBitmapCache) model
					.getBitmapCache()).getCacheSize();
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
					OfflineMode.FALSE, new DataRetrieverDataSet(retriever,
							config));
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

	public static class TestReloading extends LoaderAndDbBasedTest {

		/**
		 * Initializes the database and the model.
		 * 
		 * @throws IOException
		 *             if the data cannot be loaded
		 */
		@Before
		public void init() throws IOException {
			assertTrue(Files.deleteDir(new File(".",
					"fileAllCacheReloadingTest")));
		}

		@Test
		public void lala() {

			// get the model
			final TidaModel loadedModel = m("/net/meisen/dissertation/impl/cache/fileAllCacheReloadingTest.xml");
			loadedModel.bulkLoadDataFromDataModel();
			loadedModel.bulkLoadDataFromDataModel();
			loadedModel.bulkLoadDataFromDataModel();
			loadedModel.bulkLoadDataFromDataModel();

			// get some values from the model
			final int amount = loadedModel.getAmountOfRecords();
			final int nextDataId = loadedModel.getNextDataId();

			// unload everything
			loader.unloadAll();

			// move the files, otherwise
			final TidaModel reloadedModel = m(
					"/net/meisen/dissertation/impl/cache/fileAllCacheReloadingTest.xml",
					false);

			assertEquals(amount, reloadedModel.getAmountOfRecords());
			assertEquals(nextDataId, reloadedModel.getNextDataId());
		}
	}

	/**
	 * Test different tests when using file-caches.
	 * 
	 * @author pmeisen
	 * 
	 */
	@RunWith(Suite.class)
	@Suite.SuiteClasses({ TestLoadingOfFileCaches.class, TestReloading.class })
	public static class TestFileCachesSuite {
		// just the suite with all the tests defined here
	}
}
