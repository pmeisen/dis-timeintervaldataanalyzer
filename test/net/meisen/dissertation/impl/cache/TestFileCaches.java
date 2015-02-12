package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

import net.meisen.dissertation.help.LoaderAndDbBasedTest;
import net.meisen.dissertation.impl.dataretriever.DbDataCollection;
import net.meisen.dissertation.impl.dataretriever.DbDataIterator;
import net.meisen.dissertation.impl.dataretriever.DbDataRetriever;
import net.meisen.dissertation.impl.dataretriever.DbQueryConfig;
import net.meisen.dissertation.impl.datasets.DataRetrieverDataSet;
import net.meisen.dissertation.impl.datasets.DataRetrieverDataSetRecord;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.OfflineMode;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.MultipleDataSetIterator;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.dissertation.model.indexes.datarecord.slices.SliceWithDescriptors;
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

	/**
	 * Tests the reloading of data from the file-system using the file-caches.
	 * 
	 * @author pmeisen
	 * 
	 */
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

		/**
		 * Tests the reloading of a model using caches.
		 */
		@Test
		public void test() {

			// get the model
			final TidaModel loadedModel = m("/net/meisen/dissertation/impl/cache/fileAllCacheReloadingTest.xml");
			loadedModel.bulkLoadDataFromDataModel();
			loadedModel.bulkLoadDataFromDataModel();
			loadedModel.bulkLoadDataFromDataModel();
			loadedModel.bulkLoadDataFromDataModel();

			// get some values from the model
			final int amount = loadedModel.getAmountOfRecords();
			final int nextDataId = loadedModel.getNextDataId();
			final int lastRecordId = loadedModel.getIndex().getLastRecordId();

			// unload everything
			loader.unloadAll();

			// move the files, otherwise
			final TidaModel reloadedModel = m(
					"/net/meisen/dissertation/impl/cache/fileAllCacheReloadingTest.xml",
					false);

			// check the general model and index values
			assertEquals(amount, reloadedModel.getAmountOfRecords());
			assertEquals(nextDataId, reloadedModel.getNextDataId());
			assertEquals(lastRecordId, reloadedModel.getIndex()
					.getLastRecordId());

			// check the timeSlices, i.e. intervalIndex
			int sliceNr = 0;
			for (final SliceWithDescriptors<?> slice : reloadedModel.getIndex()
					.getIntervalIndexSlices()) {
				if (sliceNr >= 50000 && sliceNr <= 59999) {
					assertNull("" + sliceNr, slice);
				} else {
					final int timeId = (Integer) slice.getId();
					assertEquals(sliceNr, timeId);

					for (int recNr = 0; recNr < 5; recNr++) {

						// recId 0 -> 1 - 10000, recId 1 -> 10001 -> 20000, ...
						final int start = (recNr * 10000);
						final int end = ((recNr + 1) * 10000 - 1);

						if (slice == null) {
							fail(timeId + " " + recNr + " slice is null.");
						}

						final int[] ids = slice.getBitmap().getIds();
						final int pos = Arrays.binarySearch(ids, recNr);

						if (timeId >= start && timeId <= end) {
							assertTrue(timeId + " " + recNr + " " + pos,
									pos > -1);
						} else {
							assertFalse(timeId + " " + recNr + " " + pos,
									pos > -1);
						}
					}

					// check the meta-values
					final FactDescriptorSet personSet = slice
							.getDescriptors("PERSON");
					final FactDescriptorSet taskSet = slice
							.getDescriptors("TASK");
					final FactDescriptorSet valueSet = slice
							.getDescriptors("VALUE");

					final FactDescriptor<?> personFactDesc = personSet
							.iterator().next();
					assertNull(personFactDesc.getId());
					final FactDescriptor<?> taskFactDesc = taskSet.iterator()
							.next();
					assertNull(taskFactDesc.getId());
					final FactDescriptor<?> valueFactDesc = valueSet.iterator()
							.next();
					assertNotNull(valueFactDesc.getId());
					final Descriptor<?, ?, ?> valueDesc = reloadedModel
							.getMetaDataModel().getDescriptor("VALUE",
									valueFactDesc.getId());

					// check the values
					if (sliceNr < 10000) {
						assertEquals(7, valueDesc.getValue());

						assertEquals(1, personSet.size());
						assertEquals(1, taskSet.size());
						assertEquals(1, valueSet.size());
					} else if (sliceNr < 20000) {
						assertEquals(6, valueDesc.getValue());

						assertEquals(1, personSet.size());
						assertEquals(1, taskSet.size());
						assertEquals(1, valueSet.size());
					} else if (sliceNr < 30000) {
						assertEquals(5, valueDesc.getValue());

						assertEquals(1, personSet.size());
						assertEquals(1, taskSet.size());
						assertEquals(1, valueSet.size());
					} else if (sliceNr < 40000) {
						assertEquals(4, valueDesc.getValue());

						assertEquals(1, personSet.size());
						assertEquals(1, taskSet.size());
						assertEquals(1, valueSet.size());
					} else if (sliceNr < 50000) {
						assertEquals(6, valueDesc.getValue());

						assertEquals(1, personSet.size());
						assertEquals(1, taskSet.size());
						assertEquals(2, valueSet.size());
					} else {
						fail("Unexpected sliceNr '" + sliceNr + "'");
					}
				}

				sliceNr++;
			}

			// check the metaIndex
			final BaseIndexFactory f = reloadedModel.getIndexFactory();
			final MetaDataModel metaModel = reloadedModel.getMetaDataModel();
			final TidaIndex index = reloadedModel.getIndex();
			assertEquals(3, metaModel.getDescriptorModels().size());
			assertEquals(6, metaModel.getDescriptorModel("PERSON").sizeAll());
			assertEquals(6, metaModel.getDescriptorModel("TASK").sizeAll());
			assertEquals(4, metaModel.getDescriptorModel("VALUE").sizeAll());

			Descriptor<?, ?, ?> d;
			Slice<?> s;

			d = metaModel.getDescriptorByValue("PERSON", null);
			s = index.getMetaIndexDimensionSlice("PERSON", d.getId());
			assertNull(s);

			d = metaModel.getDescriptorByValue("PERSON", "Philipp");
			s = index.getMetaIndexDimensionSlice("PERSON", d.getId());
			assertEquals(
					Bitmap.createBitmap(f, 0, 5, 7, 12, 14, 19, 21, 26, 28, 33),
					s.getBitmap());

			d = metaModel.getDescriptorByValue("PERSON", "Debbie");
			s = index.getMetaIndexDimensionSlice("PERSON", d.getId());
			assertEquals(
					Bitmap.createBitmap(f, 1, 6, 8, 13, 15, 20, 22, 27, 29, 34),
					s.getBitmap());

			d = metaModel.getDescriptorByValue("PERSON", "Uschi");
			s = index.getMetaIndexDimensionSlice("PERSON", d.getId());
			assertEquals(Bitmap.createBitmap(f, 2, 9, 16, 23, 30),
					s.getBitmap());

			d = metaModel.getDescriptorByValue("PERSON", "Hajo");
			s = index.getMetaIndexDimensionSlice("PERSON", d.getId());
			assertEquals(Bitmap.createBitmap(f, 3, 10, 17, 24, 31),
					s.getBitmap());

			d = metaModel.getDescriptorByValue("PERSON", "Edison");
			s = index.getMetaIndexDimensionSlice("PERSON", d.getId());
			assertEquals(Bitmap.createBitmap(f, 4, 11, 18, 25, 32),
					s.getBitmap());

			// all together the model presents 60000 slices
			assertEquals(60000, sliceNr);
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
