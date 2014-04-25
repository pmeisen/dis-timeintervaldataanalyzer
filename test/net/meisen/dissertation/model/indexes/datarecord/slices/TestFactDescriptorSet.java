package net.meisen.dissertation.model.indexes.datarecord.slices;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.List;
import java.util.SortedSet;

import net.meisen.dissertation.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.impl.idfactories.ShortIdsFactory;
import net.meisen.dissertation.impl.indexes.IndexFactory;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;

import org.junit.Before;
import org.junit.Test;

/**
 * Tests the implementation of a {@code FactDescriptorSet}.
 * 
 * @author pmeisen
 * 
 */
public class TestFactDescriptorSet {
	private DescriptorModel<Integer> model1;
	private DescriptorModel<Short> model2;
	private DescriptorModel<Short> model3;

	/**
	 * Initializes the three test-models used.
	 */
	@Before
	public void init() {
		final IndexFactory indexFactory = new IndexFactory();

		model1 = new DescriptorModel<Integer>("model1", "model1",
				IntegerDescriptor.class, new IntegerIdsFactory(), indexFactory);
		model2 = new DescriptorModel<Short>("model2", "model2",
				IntegerDescriptor.class, new ShortIdsFactory(), indexFactory);
		model3 = new DescriptorModel<Short>("model3", "model3",
				GeneralDescriptor.class, new ShortIdsFactory(), indexFactory);
	}

	/**
	 * Tests a single model with unequal values.
	 */
	@Test
	public void testSingleModelWithUnequalValues() {
		final FactDescriptorSet set = new FactDescriptorSet();

		// create some descriptors
		final Descriptor<Integer, ?, Integer> desc1000 = model1
				.createDescriptor(1000);
		final Descriptor<Integer, ?, Integer> desc2000 = model1
				.createDescriptor(2000);
		final Descriptor<Integer, ?, Integer> desc1500 = model1
				.createDescriptor(1500);

		// add the descriptors to the set
		for (int i = 0; i < 10; i++) {
			set.addDescriptor(desc1000);
			set.addDescriptor(desc1000);
			set.addDescriptor(desc2000);
			set.addDescriptor(desc2000);
			set.addDescriptor(desc1500);
			set.addDescriptor(desc1500);
			set.addDescriptor(desc1000);
			set.addDescriptor(desc1000);
			set.addDescriptor(desc1500);
			set.addDescriptor(desc1500);
		}

		// check the final result, it should be sorted by the value
		final List<Descriptor<?, ?, ?>> list = set
				.createSortedDescriptorList(model1);
		assertEquals(3, list.size());
		assertTrue(desc1000 == list.get(0));
		assertTrue(desc1500 == list.get(1));
		assertTrue(desc2000 == list.get(2));
		assertEquals(1000.0, desc1000.getFactValue(null), 0.0);
		assertEquals(1500.0, desc1500.getFactValue(null), 0.0);
		assertEquals(2000.0, desc2000.getFactValue(null), 0.0);
	}

	/**
	 * Tests a single model with the same values, so sorting is done via
	 * identifier.
	 */
	@Test
	public void testSingleModelWithEqualValues() {
		final FactDescriptorSet set = new FactDescriptorSet();

		// create some descriptors
		final Descriptor<String, ?, Short> desc1 = model3.createDescriptor("1");
		final Descriptor<String, ?, Short> desc2 = model3.createDescriptor("2");
		final Descriptor<String, ?, Short> desc3 = model3.createDescriptor("3");

		// add the descriptors to the set
		for (int i = 0; i < 10; i++) {
			set.addDescriptor(desc3);
			set.addDescriptor(desc3);
			set.addDescriptor(desc1);
			set.addDescriptor(desc1);
			set.addDescriptor(desc1);
			set.addDescriptor(desc2);
			set.addDescriptor(desc2);
		}

		// check the final result, it should be sorted by id
		final List<Descriptor<?, ?, ?>> list = set
				.createSortedDescriptorList(model3);
		assertEquals(3, list.size());
		assertTrue(desc1 == list.get(0));
		assertTrue(desc2 == list.get(1));
		assertTrue(desc3 == list.get(2));
		assertEquals(1.0, desc1.getFactValue(null), 0.0);
		assertEquals(1.0, desc2.getFactValue(null), 0.0);
		assertEquals(1.0, desc3.getFactValue(null), 0.0);
	}

	/**
	 * Tests the usage of multiple models.
	 */
	@Test
	public void testMulitpleModels() {
		final int nrOfDescPerModel = 1000;
		final FactDescriptorSet set = new FactDescriptorSet();

		// create some descriptors
		for (int i = nrOfDescPerModel - 1; i >= 0; i--) {
			final Descriptor<Integer, ?, Integer> model1Desc = model1
					.createDescriptor(i);
			final Descriptor<String, ?, Short> model2Desc = model2
					.createDescriptor("" + i);
			final Descriptor<Integer, ?, Short> model3Desc = model3
					.createDescriptor(i);

			set.addDescriptor(model1Desc);
			set.addDescriptor(model2Desc);
			set.addDescriptor(model3Desc);
			set.addDescriptor(model1Desc);
			set.addDescriptor(model2Desc);
			set.addDescriptor(model3Desc);
		}

		SortedSet<Descriptor<?, ?, ?>> descriptors;
		descriptors = set.getDescriptors(model1);
		assertEquals(nrOfDescPerModel, descriptors.size());
		int intNr = 0;
		for (final Descriptor<?, ?, ?> desc : descriptors) {
			assertEquals(intNr, desc.getFactValue(null), 0.0);
			assertEquals(nrOfDescPerModel - intNr, desc.getId());
			intNr++;
		}

		descriptors = set.getDescriptors(model2);
		assertEquals(nrOfDescPerModel, descriptors.size());
		short shortNr = 0;
		for (final Descriptor<?, ?, ?> desc : descriptors) {
			assertEquals(shortNr, desc.getFactValue(null), 0.0);
			assertEquals((short) (nrOfDescPerModel - shortNr), desc.getId());
			shortNr++;
		}

		descriptors = set.getDescriptors(model3);
		assertEquals(nrOfDescPerModel, descriptors.size());
		short idNr = 1;
		for (final Descriptor<?, ?, ?> desc : descriptors) {
			assertEquals(1.0, desc.getFactValue(null), 0.0);
			assertEquals(idNr, desc.getId());
			idNr++;
		}

	}
}
