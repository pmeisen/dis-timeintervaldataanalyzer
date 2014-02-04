package net.meisen.dissertation.model.indexes.tida;

import static org.junit.Assert.assertEquals;

import java.util.UUID;

import net.meisen.dissertation.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.impl.indexes.IndexedCollectionFactory;
import net.meisen.dissertation.model.datasets.SingleStaticDataSet;
import net.meisen.dissertation.model.datastructure.MetaStructureEntry;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;

import org.junit.Test;

public class TestMetaIndexDimension {

	@Test
	public void test() {
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
				entry, model, new IndexedCollectionFactory());
		for (int i = 1; i <= 100000; i++) {
			final Descriptor<?, ?, Integer> desc = model.getDescriptor(i);

			// add 10 DataRecords with the value of the model
			for (int k = 0; k < 10; k++) {

				// do it twice to make sure that there are no side-effects
				idx.add(k, new SingleStaticDataSet(desc.getValue()));
				idx.add(k, new SingleStaticDataSet(desc.getValue()));
			}

			// there should be more slices and each should have 10 entries
			assertEquals(i, idx.getAmountOfSlices());
			assertEquals(10, idx.getById(i).length);
		}

		for (int i = 1; i <= 100000; i++) {
			assertEquals(10, idx.getById(i).length);
		}
	}
}
