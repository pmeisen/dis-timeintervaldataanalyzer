package net.meisen.dissertation.model.indexes.datarecord;

import static org.junit.Assert.assertEquals;
import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.help.ModuleAndDbBasedTest;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests the implementation of a {@code MetaIndex}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestMetaIndex extends ModuleAndDbBasedTest {

	@Autowired
	private TidaModelHandler loader;

	/**
	 * Tests the usage when loading a static model.
	 */
	@Test
	public void testUsingStaticIndexModel() {
		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/tidaStaticMetaIndex.xml");

		final MetaIndex metaIndex = new MetaIndex(model);
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		int i = 0;
		while (it.hasNext()) {
			final IDataRecord rec = it.next();
			final ProcessedDataRecord dataRec = new ProcessedDataRecord(rec,
					model, i);
			metaIndex.index(dataRec);
			i++;
		}
		it.close();

		// check if we got all the dimensions
		assertEquals(1, metaIndex.getAmountOfDimensions());

		// cleanUp
		model.release(true);
	}

	/**
	 * Tests the usage when loading a static model.
	 */
	@Test
	public void testUsingRandomIndexModel() {
		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/tidaRandomMetaIndex.xml");

		// create the indexes
		final MetaIndex metaIndex = new MetaIndex(model);
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		int i = 0;
		while (it.hasNext()) {
			final IDataRecord rec = it.next();
			final ProcessedDataRecord dataRec = new ProcessedDataRecord(rec,
					model, i);

			// add the record
			metaIndex.index(dataRec);
			i++;
		}
		it.close();

		// check if we got all the dimensions
		assertEquals(2, metaIndex.getAmountOfDimensions());

		// cleanUp
		model.release(true);
	}

	/**
	 * CleanUp after test
	 */
	@After
	public void unloadAll() {
		loader.unloadAll();
	}
}
