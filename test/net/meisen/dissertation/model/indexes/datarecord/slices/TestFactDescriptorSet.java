package net.meisen.dissertation.model.indexes.datarecord.slices;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import net.meisen.dissertation.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.impl.indexes.IndexFactory;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
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

		set.add(modelVariant.createDescriptor("VAL3000"));
		set.add(modelVariant.createDescriptor("VAL2000"));
		set.add(modelVariant.createDescriptor("VAL1000"));

		assertEquals(set.size(), 3);
		assertTrue(set.containsVariantRecords());

		Iterator<Descriptor<?, ?, ?>> it = set.iterator();
		assertEquals("VAL3000", it.next().getValue());
		assertEquals("VAL2000", it.next().getValue());
		assertEquals("VAL1000", it.next().getValue());

		it = set.iterator();
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

		set.add(modelInvariant.createDescriptor(500));
		set.add(modelInvariant.createDescriptor(100));
		set.add(modelInvariant.createDescriptor(200));

		assertEquals(set.size(), 3);
		assertFalse(set.containsVariantRecords());

		Iterator<Descriptor<?, ?, ?>> it = set.iterator();
		assertEquals(100, it.next().getValue());
		assertEquals(200, it.next().getValue());
		assertEquals(500, it.next().getValue());

		it = set.iterator();
		assertEquals(2, it.next().getId());
		assertEquals(3, it.next().getId());
		assertEquals(1, it.next().getId());
	}

	/**
	 * Tests the ordering of record in- and variant descriptors.
	 */
	@Test
	public void testMixed() {
		final FactDescriptorSet set = new FactDescriptorSet();

		set.add(modelInvariant.createDescriptor(500));
		set.add(modelVariant.createDescriptor("VAL3000"));
		set.add(modelVariant.createDescriptor("VAL1000"));
		set.add(modelInvariant.createDescriptor(100));
		set.add(modelVariant.createDescriptor("VAL2000"));
		set.add(modelInvariant.createDescriptor(200));

		assertEquals(set.size(), 6);
		assertTrue(set.containsVariantRecords());

		Iterator<Descriptor<?, ?, ?>> it = set.iterator();
		assertEquals("VAL3000", it.next().getValue());
		assertEquals("VAL1000", it.next().getValue());
		assertEquals("VAL2000", it.next().getValue());
		assertEquals(100, it.next().getValue());
		assertEquals(200, it.next().getValue());
		assertEquals(500, it.next().getValue());

		it = set.iterator();
		assertEquals(2, it.next().getId());
		assertEquals(3, it.next().getId());
		assertEquals(5, it.next().getId());
		assertEquals(4, it.next().getId());
		assertEquals(6, it.next().getId());
		assertEquals(1, it.next().getId());
	}
}
