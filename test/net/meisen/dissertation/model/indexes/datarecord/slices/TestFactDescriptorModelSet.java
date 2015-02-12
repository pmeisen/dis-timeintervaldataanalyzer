package net.meisen.dissertation.model.indexes.datarecord.slices;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.Collection;
import java.util.HashSet;
import java.util.List;

import net.meisen.dissertation.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.impl.idfactories.ShortIdsFactory;
import net.meisen.dissertation.impl.indexes.IndexFactory;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.descriptors.FactDescriptor;

import org.junit.Before;
import org.junit.Test;

/**
 * Tests the implementation of a {@code FactDescriptorModelSet}.
 * 
 * @author pmeisen
 * 
 */
public class TestFactDescriptorModelSet {
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
		final FactDescriptorModelSet set = new FactDescriptorModelSet();

		// create some descriptors
		final Descriptor<Integer, ?, Integer> desc1000 = model1
				.createDescriptor(1000);
		final Descriptor<Integer, ?, Integer> desc2000 = model1
				.createDescriptor(2000);
		final Descriptor<Integer, ?, Integer> desc1500 = model1
				.createDescriptor(1500);

		// add the descriptors to the set
		for (int i = 0; i < 10; i++) {
			final boolean first = i == 0;

			assertEquals(first, set.addDescriptor(desc1000));
			assertEquals(first, set.addDescriptor(desc2000));
			assertEquals(first, set.addDescriptor(desc1500));
			assertFalse(set.addDescriptor(desc1000));
			assertFalse(set.addDescriptor(desc1500));
		}

		// check the final result, it should be sorted by the value
		final List<FactDescriptor<?>> list = set
				.createSortedDescriptorList(model1);
		assertEquals(3, list.size());
		assertTrue(desc1000.getFactDescriptor() == list.get(0));
		assertTrue(desc1500.getFactDescriptor() == list.get(1));
		assertTrue(desc2000.getFactDescriptor() == list.get(2));
		assertEquals(1000.0, desc1000.getFactDescriptor().getFact(), 0.0);
		assertEquals(1500.0, desc1500.getFactDescriptor().getFact(), 0.0);
		assertEquals(2000.0, desc2000.getFactDescriptor().getFact(), 0.0);
	}

	/**
	 * Tests a single model with the same values, so sorting is done via
	 * identifier.
	 */
	@Test
	public void testSingleModelWithEqualValues() {
		final FactDescriptorModelSet set = new FactDescriptorModelSet();

		// create some descriptors
		final Descriptor<Integer, ?, Integer> desc1 = model1
				.createDescriptor(1);
		final Descriptor<Integer, ?, Integer> desc2 = model1
				.createDescriptor(2);
		final Descriptor<Integer, ?, Integer> desc3 = model1
				.createDescriptor(3);

		// add the descriptors to the set
		for (int i = 0; i < 10; i++) {
			final boolean first = i == 0;

			assertEquals(first, set.addDescriptor(desc3));
			assertEquals(first, set.addDescriptors(desc1, desc3));
			assertFalse(set.addDescriptor(desc3));
			assertFalse(set.addDescriptor(desc1));
			assertFalse(set.addDescriptors(desc1, desc3));
			assertEquals(first, set.addDescriptor(desc2));
		}

		// check the final result, it should be sorted by id
		final List<FactDescriptor<?>> list = set
				.createSortedDescriptorList(model1);
		assertEquals(3, list.size());
		assertTrue(desc1.getFactDescriptor() == list.get(0));
		assertTrue(desc2.getFactDescriptor() == list.get(1));
		assertTrue(desc3.getFactDescriptor() == list.get(2));
		assertEquals(1.0, desc1.getFactDescriptor().getFact(), 0.0);
		assertEquals(2.0, desc2.getFactDescriptor().getFact(), 0.0);
		assertEquals(3.0, desc3.getFactDescriptor().getFact(), 0.0);
	}

	/**
	 * Tests the usage of multiple models.
	 */
	@Test
	public void testMulitpleModels() {
		final int nrOfDescPerModel = 1000;
		final FactDescriptorModelSet set = new FactDescriptorModelSet();

		// create some descriptors
		for (int i = nrOfDescPerModel - 1; i >= 0; i--) {
			final Descriptor<Integer, ?, Integer> model1Desc = model1
					.createDescriptor(i);
			final Descriptor<String, ?, Short> model2Desc = model2
					.createDescriptor("" + i);
			final Descriptor<Integer, ?, Short> model3Desc = model3
					.createDescriptor(i);

			assertTrue(set.addDescriptor(model1Desc));
			assertTrue(set.addDescriptor(model2Desc));
			assertEquals(i == nrOfDescPerModel - 1,
					set.addDescriptor(model3Desc));
			assertFalse(set.addDescriptor(model1Desc));
			assertFalse(set.addDescriptor(model2Desc));
			assertFalse(set.addDescriptor(model3Desc));
		}

		FactDescriptorSet descriptors;
		descriptors = set.getDescriptors(model1);
		assertEquals(nrOfDescPerModel, descriptors.size());
		int intNr = 0;
		for (final FactDescriptor<?> desc : descriptors) {
			assertEquals(intNr, desc.getFact(), 0.0);
			assertEquals(nrOfDescPerModel - intNr, desc.getId());
			intNr++;
		}

		descriptors = set.getDescriptors(model2);
		assertEquals(nrOfDescPerModel, descriptors.size());
		short shortNr = 0;
		for (final FactDescriptor<?> desc : descriptors) {
			assertEquals(shortNr, desc.getFact(), 0.0);
			assertEquals((short) (nrOfDescPerModel - shortNr), desc.getId());
			shortNr++;
		}

		descriptors = set.getDescriptors(model3);
		assertEquals(1, descriptors.size());
		for (final FactDescriptor<?> desc : descriptors) {
			assertEquals(1.0, desc.getFact(), 0.0);
			assertNull(desc.getId());
		}
	}

	/**
	 * Test the iteration over an empty {@code FactDescriptorModelSet}.
	 */
	@Test
	public void testEmptyIteration() {
		final FactDescriptorModelSet set = new FactDescriptorModelSet();

		// just iterate if there is any we are wrong
		for (final FactDescriptor<?> fd : set) {
			assertNotNull(fd);
			fail("Iterated over empty set: " + set);
		}

		assertFalse(set.iterator().hasNext());
	}

	/**
	 * Test the iteration over a {@code FactDescriptorModelSet}.
	 */
	@Test
	public void testIteration() {
		final Collection<FactDescriptor<?>> expected = new HashSet<FactDescriptor<?>>();
		final FactDescriptorModelSet set = new FactDescriptorModelSet();

		// add some sample values
		expected.add(new FactDescriptor<Integer>("Value", 500, 500.0));
		expected.add(new FactDescriptor<Integer>("Value", 600, 600.0));
		set.add(expected);

		int counter = 0;
		for (final FactDescriptor<?> fd : set) {
			assertTrue(expected.remove(fd));
			counter++;
		}
		assertEquals(0, expected.size());
		assertEquals(2, counter);
	}
}
