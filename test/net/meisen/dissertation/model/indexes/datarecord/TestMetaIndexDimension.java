package net.meisen.dissertation.model.indexes.datarecord;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;
import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleAndDbBasedTest;
import net.meisen.dissertation.impl.persistence.FileLocation;
import net.meisen.dissertation.impl.persistence.ZipPersistor;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.indexes.datarecord.slices.Slice;
import net.meisen.dissertation.model.persistence.Group;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests the implementation of a {@code MetaIndexDimension}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestMetaIndexDimension extends ModuleAndDbBasedTest {

	@Autowired
	private TidaModelHandler loader;

	/**
	 * Tests the usage when loading a static model.
	 */
	@Test
	public void testUsingStaticIndexModel() {
		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/tidaStaticMetaIndex.xml");

		final MetaDataModel metaModel = model.getMetaDataModel();
		final DescriptorModel<?> descModel = metaModel
				.getDescriptorModel("FAMILY");

		@SuppressWarnings({ "rawtypes", "unchecked" })
		final MetaIndexDimension idx = new MetaIndexDimension(descModel,
				model.getBitmapCache(), model.getIndexFactory());
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		int i = 0;
		while (it.hasNext()) {
			final IDataRecord rec = it.next();
			final ProcessedDataRecord dataRec = new ProcessedDataRecord(rec,
					model, i);
			idx.index(dataRec);
			i++;
		}
		it.close();

		// check if we got all the data
		assertEquals(16, idx.getAmountOfSlices());
		final String[] vals = new String[] { "Hajo", "Uschi", "Edison",
				"Linus", "Hannah", "Lasse", "Silas", "Rasmus", "Philipp",
				"Tobias", "Holger", "Andrea", "Deborah", "Nicole", "Jörg",
				"Stephanie" };
		final int[] exp = new int[] { 8, 11, 5, 7, 11, 5, 8, 7, 2, 8, 5, 6, 4,
				5, 5, 3 };
		for (i = 0; i < vals.length; i++) {
			assertEquals(exp[i], idx.getSliceByValue(vals[i]).count());
		}

		// cleanUp
		loader.unloadAll();
		model.release(true);
	}

	/**
	 * Tests the usage when loading a static model.
	 */
	@Test
	public void testUsingRandomIndexModel() {
		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/tidaRandomMetaIndex.xml");

		// get the defined model and the structure
		final MetaDataModel metaModel = model.getMetaDataModel();
		final DescriptorModel<?> fixedDescModel = metaModel
				.getDescriptorModel("FIXED");
		final DescriptorModel<?> randomDescModel = metaModel
				.getDescriptorModel("RANDOMINT");

		// check the descriptors
		assertEquals(3, fixedDescModel.size());
		final int prevSize = randomDescModel.size();
		assertTrue("Found " + prevSize, prevSize <= 100000);

		// create the indexDimensions
		@SuppressWarnings({ "rawtypes", "unchecked" })
		final MetaIndexDimension fixedIdx = new MetaIndexDimension(
				fixedDescModel, model.getBitmapCache(), model.getIndexFactory());

		@SuppressWarnings({ "rawtypes", "unchecked" })
		final MetaIndexDimension randomIdx = new MetaIndexDimension(
				randomDescModel, model.getBitmapCache(),
				model.getIndexFactory());

		// add the data to the indexDimensions
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		int i = 0;
		int added = 0;
		while (it.hasNext()) {
			final IDataRecord rec = it.next();

			// check if it will be created
			if (randomDescModel.getDescriptorByValue(rec
					.getValue("RND_INTEGER")) == null) {
				added++;
			}

			// create the processedRecord and the needed data
			final ProcessedDataRecord dataRec = new ProcessedDataRecord(rec,
					model, i);

			// add the record
			fixedIdx.index(dataRec);
			randomIdx.index(dataRec);
			i++;
		}
		it.close();

		// check the DescriptorModels
		assertEquals(3, fixedDescModel.size());
		assertEquals(prevSize + added, randomDescModel.size());

		// check the slices
		assertEquals(1, fixedIdx.getAmountOfSlices());
		assertTrue("Created " + randomIdx.getAmountOfSlices(),
				randomIdx.getAmountOfSlices() <= 100000);

		// cleanUp
		loader.unloadAll();
		model.release(true);
	}

	/**
	 * Tests the saving and loading of a {@code MetaIndexDimension}.
	 * 
	 * @throws IOException
	 *             if the temporary file cannot be created
	 */
	@Test
	@SuppressWarnings("unchecked")
	public void testSaveAndLoad() throws IOException {

		// the testing persistor we use here
		final Group group = new Group("meta");
		final ZipPersistor persistor = new ZipPersistor(
				(IExceptionRegistry) configuration
						.getModule(DefaultValues.EXCEPTIONREGISTRY_ID));

		// get the model and the factory we use
		final TidaModel model = loader
				.loadViaXslt("/net/meisen/dissertation/model/indexes/datarecord/tidaStaticMetaIndex.xml");

		// get the defined model and the structure
		final MetaDataModel metaModel = model.getMetaDataModel();
		final DescriptorModel<?> descModel = metaModel
				.getDescriptorModel("FAMILY");

		// get the save-index
		@SuppressWarnings({ "rawtypes" })
		final MetaIndexDimension saveIdx = new MetaIndexDimension(descModel,
				model.getBitmapCache(), model.getIndexFactory());
		assertEquals(0, saveIdx.getAmountOfSlices());

		// add data
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterator();
		int i = 0;
		while (it.hasNext()) {
			final IDataRecord rec = it.next();
			final ProcessedDataRecord dataRec = new ProcessedDataRecord(rec,
					model, i);
			saveIdx.index(dataRec);
			i++;
		}
		it.close();
		assertTrue(saveIdx.getAmountOfSlices() > 0);

		// create a temporary file and save it
		final String prefixFile = UUID.randomUUID().toString();
		final File tmpFile = File.createTempFile(prefixFile, ".zip");
		assertTrue(tmpFile.length() == 0);

		/*
		 * save the metadata, the save method of saveIdx should be triggered via
		 * the registration
		 */
		persistor.register(group, saveIdx);
		persistor.save(new FileLocation(tmpFile));
		persistor.unregister(group);

		// get the load-index
		@SuppressWarnings({ "rawtypes" })
		final MetaIndexDimension loadIdx = new MetaIndexDimension(descModel,
				model.getBitmapCache(), model.getIndexFactory());
		assertEquals(0, loadIdx.getAmountOfSlices());

		/*
		 * load the metadata from the saved file
		 */
		persistor.register(group, loadIdx);
		persistor.load(new FileLocation(tmpFile));
		persistor.unregister(group);

		// check the loaded data
		assertEquals(saveIdx.getAmountOfSlices(), loadIdx.getAmountOfSlices());
		for (final Slice<?> slice : loadIdx.getSlices()) {
			assertEquals(slice.getBitmap(), saveIdx.getSliceById(slice.getId())
					.getBitmap());
		}

		// cleanUp
		loader.unloadAll();
		model.release(true);
		assertTrue(tmpFile.delete());
	}
}
