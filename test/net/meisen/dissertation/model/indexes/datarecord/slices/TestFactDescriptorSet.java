package net.meisen.dissertation.model.indexes.datarecord.slices;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import net.meisen.dissertation.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.impl.indexes.IndexFactory;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.dissertation.model.indexes.datarecord.slices.mock.VariantDescriptor;

import org.junit.Before;
import org.junit.Test;

/**
 * Tests the implementation of a {@link FactDescriptorSet}.
 * 
 * @author pmeisen
 * 
 */
public class TestFactDescriptorSet {
	private DescriptorModel<Integer> modelInvariant;
	private DescriptorModel<Integer> modelVariant;

	/**
	 * Initializes the three test-models used.
	 */
	@Before
	public void init() {
		final IndexFactory indexFactory = new IndexFactory();
		final IntegerIdsFactory idFactory = new IntegerIdsFactory();

		modelInvariant = new DescriptorModel<Integer>("modelInvariant",
				"modelInvariant", IntegerDescriptor.class, idFactory,
				indexFactory);
		modelVariant = new DescriptorModel<Integer>("modelVariant",
				"modelVariant", VariantDescriptor.class, idFactory,
				indexFactory);
	}

	/**
	 * Test the ordering of record variant descriptors.
	 */
	@Test
	public void testJustVariantOrder() {
		final FactDescriptorSet set = new FactDescriptorSet();

		assertTrue(set.add(modelVariant.createDescriptor("VAL3000")
				.getFactDescriptor()));
		assertTrue(set.add(modelVariant.createDescriptor("VAL2000")
				.getFactDescriptor()));
		assertTrue(set.add(modelVariant.createDescriptor("VAL1000")
				.getFactDescriptor()));

		assertEquals(set.size(), 3);
		assertTrue(set.containsVariantRecords());

		Iterator<FactDescriptor<?>> it;

		it = set.iterator();
		assertFalse(it.hasNext());

		it = set.variantIterator();
		assertEquals(1, it.next().getId());
		assertEquals(2, it.next().getId());
		assertEquals(3, it.next().getId());

		it = set.variantIterator();
		assertEquals(1, it.next().getId());
		assertEquals(2, it.next().getId());
		assertEquals(3, it.next().getId());
	}

	/**
	 * Test the ordering of record invariant descriptors.
	 */
	@Test
	public void testJustInvariantOrder() {
		final FactDescriptorSet set = new FactDescriptorSet();

		assertTrue(set.add(new FactDescriptor<Integer>("modelInvariant", 7,
				Double.NaN)));
		assertTrue(set.add(modelInvariant.createDescriptor(500)
				.getFactDescriptor()));
		assertTrue(set.add(modelInvariant.createDescriptor(100)
				.getFactDescriptor()));
		assertTrue(set.add(modelInvariant.createDescriptor(200)
				.getFactDescriptor()));

		assertEquals(set.size(), 4);
		assertFalse(set.containsVariantRecords());

		final Iterator<FactDescriptor<?>> it = set.iterator();
		assertEquals(2, it.next().getId());
		assertEquals(3, it.next().getId());
		assertEquals(1, it.next().getId());
		assertEquals(7, it.next().getId());
	}

	/**
	 * Tests the ordering of record in- and variant descriptors.
	 */
	@Test
	public void testMixed() {
		final FactDescriptorSet set = new FactDescriptorSet();

		assertTrue(set.add(modelInvariant.createDescriptor(500)
				.getFactDescriptor()));
		assertTrue(set.add(modelVariant.createDescriptor("VAL3000")
				.getFactDescriptor()));
		assertTrue(set.add(modelVariant.createDescriptor("VAL1000")
				.getFactDescriptor()));
		assertTrue(set.add(modelInvariant.createDescriptor(100)
				.getFactDescriptor()));
		assertTrue(set.add(new FactDescriptor<Integer>("modelInvariant", 7,
				Double.NaN)));
		assertTrue(set.add(modelVariant.createDescriptor("VAL2000")
				.getFactDescriptor()));
		assertTrue(set.add(modelInvariant.createDescriptor(200)
				.getFactDescriptor()));

		assertEquals(set.size(), 7);
		assertTrue(set.containsVariantRecords());

		Iterator<FactDescriptor<?>> it;
		it = set.iterator();
		assertEquals(4, it.next().getId());
		assertEquals(6, it.next().getId());
		assertEquals(1, it.next().getId());
		assertEquals(7, it.next().getId());

		it = set.variantIterator();
		assertEquals(3, it.next().getId());
		assertEquals(2, it.next().getId());
		assertEquals(5, it.next().getId());
	}

	/**
	 * Tests the creation of an array.
	 */
	@Test
	public void testToArray() {
		final FactDescriptorSet set = new FactDescriptorSet();

		assertTrue(set.add(modelInvariant.createDescriptor(500)
				.getFactDescriptor()));
		assertTrue(set.add(modelVariant.createDescriptor("VAL3000")
				.getFactDescriptor()));
		assertTrue(set.add(modelVariant.createDescriptor("VAL1000")
				.getFactDescriptor()));
		assertTrue(set.add(modelInvariant.createDescriptor(100)
				.getFactDescriptor()));
		assertTrue(set.add(modelVariant.createDescriptor("VAL2000")
				.getFactDescriptor()));
		assertTrue(set.add(modelInvariant.createDescriptor(200)
				.getFactDescriptor()));
		assertTrue(set.add(new FactDescriptor<Integer>("modelInvariant", 7,
				Double.NaN)));

		final List<Object> nr1 = Arrays.asList(set.toArray());
		final List<FactDescriptor<?>> nr2 = Arrays.asList(set
				.toArray(new FactDescriptor<?>[] {}));

		// check each list
		final List<?>[] lists = new List<?>[] { nr1, nr2 };
		for (final List<?> list : lists) {
			assertEquals(7, list.size());

			assertEquals(4, ((FactDescriptor<?>) list.get(0)).getId());
			assertEquals(6, ((FactDescriptor<?>) list.get(1)).getId());
			assertEquals(1, ((FactDescriptor<?>) list.get(2)).getId());
			assertEquals(7, ((FactDescriptor<?>) list.get(3)).getId());
			assertEquals(3, ((FactDescriptor<?>) list.get(4)).getId());
			assertEquals(2, ((FactDescriptor<?>) list.get(5)).getId());
			assertEquals(5, ((FactDescriptor<?>) list.get(6)).getId());
		}
	}

	/**
	 * Tests the adding of duplicates.
	 */
	@Test
	public void testModifiedDuplicates() {
		final FactDescriptorSet set = new FactDescriptorSet();

		assertTrue(set.add(new FactDescriptor<Integer>("modelInvariant", 7,
				Double.NaN)));
		assertEquals(1, set.size());

		assertTrue(set
				.add(new FactDescriptor<Integer>("modelInvariant", 7, 7.0)));
		assertEquals(1, set.size());

		assertFalse(set.add(new FactDescriptor<Integer>("modelInvariant", 7,
				7.0)));
		assertEquals(1, set.size());

		assertTrue(set.add(new FactDescriptor<Integer>("modelInvariant", 7,
				17.0)));
		assertEquals(1, set.size());

		assertTrue(set.add(new FactDescriptor<Integer>("modelInvariant", 7,
				Double.NaN)));
		assertEquals(1, set.size());

		assertTrue(set.add(new FactDescriptor<Integer>("modelInvariant", 8,
				Double.NaN)));
		assertEquals(2, set.size());
	}
}
