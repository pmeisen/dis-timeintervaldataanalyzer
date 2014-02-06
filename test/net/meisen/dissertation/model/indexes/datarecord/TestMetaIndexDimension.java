package net.meisen.dissertation.model.indexes.datarecord;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.List;
import java.util.UUID;

import net.meisen.dissertation.config.TidaConfig;
import net.meisen.dissertation.help.ModuleAndDbBasedTest;
import net.meisen.dissertation.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.impl.indexes.IndexedCollectionFactory;
import net.meisen.dissertation.model.data.DataStructure;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.dissertation.model.datasets.SingleStaticDataSet;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.datarecord.MetaIndexDimension;
import net.meisen.dissertation.model.loader.TidaModelLoader;
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
@ContextClass(TidaConfig.class)
@ContextFile("sbconfigurator-core.xml")
public class TestMetaIndexDimension extends ModuleAndDbBasedTest {

	@Autowired
	private TidaModelLoader loader;

	/**
	 * Tests the usage when creating a random model which is coded.
	 */
	@Test
	public void testUsingCreatedRandomModel() {
		final IndexedCollectionFactory idxFactory = new IndexedCollectionFactory();
		final MetaStructureEntry entry = new MetaStructureEntry("ID", 1);
		final DescriptorModel<Integer> model = new DescriptorModel<Integer>(
				"ID", "MODEL", GeneralDescriptor.class,
				new IntegerIdsFactory(), idxFactory);

		for (int i = 0; i < 100000; i++) {
			final String gen = UUID.randomUUID().toString();
			model.createDescriptor(gen).getId();
		}

		final MetaIndexDimension<Integer> idx = new MetaIndexDimension<Integer>(
				entry, model, idxFactory);
		for (int i = 1; i <= 100000; i++) {
			final Descriptor<?, ?, Integer> desc = model.getDescriptor(i);

			// add 10 DataRecords with the value of the model
			for (int k = 0; k < 10; k++) {

				// do it twice to make sure that there are no side-effects
				idx.index(k + (i - 1) * 10,
						new SingleStaticDataSet(desc.getValue()));
				idx.index(k + (i - 1) * 10,
						new SingleStaticDataSet(desc.getValue()));
			}

			// there should be more slices and each should have 10 entries
			assertEquals(i, idx.getAmountOfSlices());
			assertEquals(10, idx.getById(i).length);
		}

		// there should be ten selections for each id, shifted by 10
		for (int i = 1; i <= 100000; i++) {
			final int[] pos = idx.getById(i);
			assertEquals(10, pos.length);

			for (int k = (i - 1) * 10; k < i * 10; k++) {
				assertEquals(k, pos[k - (i - 1) * 10]);
			}
		}
	}

	/**
	 * Tests the usage when loading a static model.
	 */
	@Test
	public void testUsingStaticIndexModel() {
		final TidaModel model = loader
				.load("mh_tidaStaticIndexModel",
						"/net/meisen/dissertation/model/indexes/datarecord/tidaStaticIndexModel.xml");

		final MetaDataModel metaModel = model.getMetaDataModel();
		final DataStructure structure = model.getDataStructure();
		final DescriptorModel<?> descModel = metaModel
				.getDescriptorModel("FAMILY");
		final List<MetaStructureEntry> metaStructures = structure
				.getEntriesByClass(MetaStructureEntry.class);

		@SuppressWarnings({ "rawtypes", "unchecked" })
		final MetaIndexDimension idx = new MetaIndexDimension(
				metaStructures.get(0), descModel,
				new IndexedCollectionFactory());
		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterate();
		int i = 0;
		while (it.hasNext()) {
			final IDataRecord rec = it.next();
			idx.index(i, rec);
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
	}

	/**
	 * Tests the usage when loading a static model.
	 */
	@Test
	public void testUsingRandomIndexModel() {
		final IndexedCollectionFactory idxFactory = new IndexedCollectionFactory();
		final TidaModel model = loader
				.load("mh_tidaRandomIndexModel",
						"/net/meisen/dissertation/model/indexes/datarecord/tidaRandomIndexModel.xml");

		final MetaDataModel metaModel = model.getMetaDataModel();
		final DataStructure structure = model.getDataStructure();
		final DescriptorModel<?> fixedDescModel = metaModel
				.getDescriptorModel("FIXED");
		final DescriptorModel<?> randomDescModel = metaModel
				.getDescriptorModel("RANDOMINT");
		final List<MetaStructureEntry> metaStructures = structure
				.getEntriesByClass(MetaStructureEntry.class);
		final MetaStructureEntry fixedStructure = metaStructures.get(0);
		final MetaStructureEntry randomStructure = metaStructures.get(1);

		// check the descriptors
		assertEquals(3, fixedDescModel.size());
		final int prevSize = randomDescModel.size();
		assertTrue("Found " + prevSize, prevSize <= 100000);

		// create the indexes
		@SuppressWarnings({ "rawtypes", "unchecked" })
		final MetaIndexDimension fixedIdx = new MetaIndexDimension(
				fixedStructure, fixedDescModel, idxFactory);

		@SuppressWarnings({ "rawtypes", "unchecked" })
		final MetaIndexDimension randomIdx = new MetaIndexDimension(
				randomStructure, randomDescModel, idxFactory);

		final IClosableIterator<IDataRecord> it = model.getDataModel()
				.iterate();
		int i = 0;
		int added = 0;
		while (it.hasNext()) {
			final IDataRecord rec = it.next();

			// check if it will be created
			if (randomDescModel.getDescriptorByValue(rec
					.getValue("RND_INTEGER")) == null) {
				added++;
			}

			// add the record
			fixedIdx.index(i, rec);
			randomIdx.index(i, rec);
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
	}
}
