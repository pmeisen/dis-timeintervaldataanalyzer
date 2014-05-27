package net.meisen.dissertation.impl.cache;

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

import org.junit.Test;

public class TestFileCaches extends LoaderAndDbBasedTest {

	@Test
	public void testFileCaches() throws IOException {

		// open the database
		getDb("tida",
				"/net/meisen/dissertation/performance/paper/in2014/smc/data/ghdataHsql.zip");

		// get the model
		final TidaModel model = m("/net/meisen/dissertation/impl/cache/fileAllCacheTest.xml");

		// get the database connection
		final DbDataRetriever retriever = (DbDataRetriever) model
				.getDataModel().getDataRetriever("db_tida");
		final DbQueryConfig config = new DbQueryConfig();
		config.setLanguage("SQL");
		config.setQuery("SELECT KEY, PERSON, TASKTYPE, WORKAREA, INTERVAL_START, INTERVAL_END FROM SMC_DATA WHERE ROWNUM() <= 1000");
		final DbDataCollection data = retriever.retrieve(config);
		final DbDataIterator it = data.iterator();

		// add the retrieved data step by step
		while (it.hasNext()) {
			model.loadRecord(new DataRetrieverDataSetRecord(it.next()));
		}

		// check the caches
		final FileBitmapCache bmpCache = (FileBitmapCache) model
				.getBitmapCache();
		final FileFactDescriptorModelSetCache factCache = (FileFactDescriptorModelSetCache) model
				.getFactsCache();
		System.out.println(bmpCache.getCacheSize());
		System.out.println(factCache.getCacheSize());

		// add more data
		model.bulkLoadData(new MultipleDataSetIterator(OfflineMode.FALSE,
				new DataRetrieverDataSet(retriever, config)));
		System.out.println(bmpCache.getCacheSize());
		System.out.println(factCache.getCacheSize());
		
		// release everything
		data.release();
		retriever.release();
	}
}
