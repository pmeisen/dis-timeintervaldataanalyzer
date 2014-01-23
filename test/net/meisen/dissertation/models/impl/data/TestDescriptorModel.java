package net.meisen.dissertation.models.impl.data;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.Locale;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.data.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.data.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.data.impl.descriptors.LongDescriptor;
import net.meisen.dissertation.data.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.data.impl.idfactories.LongIdsFactory;
import net.meisen.dissertation.data.impl.indexes.IndexedCollectionFactory;
import net.meisen.dissertation.data.impl.indexes.MultipleIndexedCollection;
import net.meisen.dissertation.data.impl.indexes.TroveIntIndexedCollection;
import net.meisen.dissertation.data.impl.indexes.TroveLongIndexedCollection;
import net.meisen.dissertation.exceptions.DescriptorModelException;
import net.meisen.general.sbconfigurator.config.DefaultConfiguration;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.matchers.JUnitMatchers;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of the {@code DescriptorModel}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "net/meisen/dissertation/models/impl/data/testDescriptorModel.xml")
public class TestDescriptorModel {

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private DefaultConfiguration configuration;

	/**
	 * Rule to evaluate exceptions
	 */
	@Rule
	public ExpectedException thrown = ExpectedException.none();

	private Locale oldDefault;

	/**
	 * Make sure we have {@code Locale.US} so that comparisons of errors will
	 * fit
	 */
	@Before
	public void setUp() {
		oldDefault = Locale.getDefault();
		Locale.setDefault(Locale.US);
	}

	/**
	 * Tests the implementation {@code DescriptorModel#getId()} and
	 * {@code DescriptorModel#getName()}.
	 */
	@Test
	public void testGetIdAndName() {
		DescriptorModel<Integer> model;

		// create some models
		model = new DescriptorModel<Integer>("id", GeneralDescriptor.class,
				new IntegerIdsFactory());
		assertEquals("id", model.getId());
		assertEquals("id", model.getName());

		model = new DescriptorModel<Integer>("anotherId", "anotherName",
				GeneralDescriptor.class, new IntegerIdsFactory());
		assertEquals("anotherId", model.getId());
		assertEquals("anotherName", model.getName());
	}

	/**
	 * Tests the implementation {@code DescriptorModel#getDescriptorIndex()}.
	 */
	@Test
	public void testGetDescriptorIndex() {
		// create a model using an integerIdsFactory
		final DescriptorModel<Integer> modelIntIds = new DescriptorModel<Integer>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new IntegerIdsFactory(), new IndexedCollectionFactory());

		// check some settings
		assertTrue(modelIntIds.getDescriptorIndex() instanceof MultipleIndexedCollection);
		final MultipleIndexedCollection idxInt = (MultipleIndexedCollection) modelIntIds
				.getDescriptorIndex();
		assertTrue(
				idxInt.getIndexedCollection(0).getClass().getName(),
				idxInt.getIndexedCollection(0) instanceof TroveIntIndexedCollection);

		final DescriptorModel<Long> modelLongIds = new DescriptorModel<Long>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new LongIdsFactory(), new IndexedCollectionFactory());

		// check some settings
		assertTrue(modelLongIds.getDescriptorIndex() instanceof MultipleIndexedCollection);
		final MultipleIndexedCollection idxLong = (MultipleIndexedCollection) modelLongIds
				.getDescriptorIndex();
		assertTrue(
				idxLong.getIndexedCollection(0).getClass().getName(),
				idxLong.getIndexedCollection(0) instanceof TroveLongIndexedCollection);
	}

	/**
	 * Tests the implementation of
	 * {@code DescriptorModel#addDescriptor(Descriptor)}.
	 */
	@Test
	public void testAddDescriptor() {

		// check if descriptors are added which are created in another
		// DescriptorModel
		final DescriptorModel<Long> modelLongIds1 = new DescriptorModel<Long>(
				"ModelId", "ModelName", LongDescriptor.class,
				new LongIdsFactory(), new IndexedCollectionFactory());
		final DescriptorModel<Long> modelLongIds2 = new DescriptorModel<Long>(
				"ModelId", "ModelName", LongDescriptor.class,
				new LongIdsFactory(), new IndexedCollectionFactory());

		// add some descriptors created by the one into the other
		for (long i = 1; i < 100; i++) {
			modelLongIds2.addDescriptor(modelLongIds1.createDescriptor(i));
		}
		assertEquals(99, modelLongIds2.getDescriptors().size());
	}

	/**
	 * Tests the implementation {@code DescriptorModel#createDescriptor(Object)}
	 * .
	 */
	@Test
	public void testCreateDescriptor() {

		// create a model using an integerIdsFactory
		final DescriptorModel<Integer> modelIntIds = new DescriptorModel<Integer>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new IntegerIdsFactory(), new IndexedCollectionFactory());

		// create a bunch of descriptors
		for (int i = 1; i < 100; i++) {
			final Descriptor<?, ?, ?> descriptor = modelIntIds
					.createDescriptor("TestValue" + i);
			assertNotNull(descriptor);
			assertEquals(descriptor.getId(), i);
			assertTrue(Integer.class.equals(descriptor.getId().getClass()));
			assertEquals(descriptor.getValue(), "TestValue" + i);
		}
		assertEquals(99, modelIntIds.getDescriptors().size());

		final DescriptorModel<Long> modelLongIds = new DescriptorModel<Long>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new LongIdsFactory(), new IndexedCollectionFactory());

		// create a bunch of descriptors
		for (long i = 1; i < 100; i++) {
			final Descriptor<?, ?, ?> descriptor = modelLongIds
					.createDescriptor("TestValue" + i);
			assertNotNull(descriptor);
			assertEquals(descriptor.getId(), i);
			assertTrue(Long.class.equals(descriptor.getId().getClass()));
			assertEquals(descriptor.getValue(), "TestValue" + i);
		}
		assertEquals(99, modelLongIds.getDescriptors().size());
	}

	/**
	 * Tests the implementation {@code DescriptorModel#getDescriptor(Object)} .
	 */
	@Test
	public void testGetDescriptor() {
		final DescriptorModel<Long> modelLongIds = new DescriptorModel<Long>(
				"ModelId", "ModelName", LongDescriptor.class,
				new LongIdsFactory(), new IndexedCollectionFactory());

		for (long i = 1; i < 100; i++) {
			modelLongIds.createDescriptor(100 - i);
		}

		// check the id and the value
		for (long i = 1; i < 100; i++) {
			final Descriptor<?, ?, Long> desc = modelLongIds.getDescriptor(i);
			assertEquals(new Long(i), desc.getId());
			assertEquals(new Long(100 - i), desc.getValue());
		}
	}

	/**
	 * Tests the implementation
	 * {@code DescriptorModel#getDescriptorByValue(Object)} .
	 */
	@Test
	public void testGetDescriptorByValue() {
		final DescriptorModel<Long> modelLongIds = new DescriptorModel<Long>(
				"ModelId", "ModelName", LongDescriptor.class,
				new LongIdsFactory(), new IndexedCollectionFactory());

		for (long i = 1; i < 100; i++) {
			modelLongIds.createDescriptor(100 - i);
		}

		// check the id and the value
		for (long i = 1; i < 100; i++) {
			final Descriptor<?, ?, Long> desc = modelLongIds
					.getDescriptorByValue(i);
			assertEquals(new Long(100 - i), desc.getId());
			assertEquals(new Long(i), desc.getValue());
		}
	}

	/**
	 * Tests the wiring of a {@code DescriptorModel} using {@code Spring}
	 */
	@Test
	public void testSpringCreation() {
		final DescriptorModel<Integer> model = new DescriptorModel<Integer>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new IntegerIdsFactory());
		configuration.wireInstance(model);

		// create a bunch of descriptors
		for (int i = 1; i < 100; i++) {
			final Descriptor<?, ?, ?> descriptor = model
					.createDescriptor("TestValue" + i);
			assertNotNull(descriptor);
			assertEquals(descriptor.getId(), i);
			assertTrue(Integer.class.equals(descriptor.getId().getClass()));
			assertEquals(descriptor.getValue(), "TestValue" + i);
		}
	}

	/**
	 * Tests if an exception is thrown when a value is used multiple times.
	 */
	@Test
	public void testExceptionDuplicatedValueDescriptorCreation() {
		thrown.expect(DescriptorModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("descriptor 'MyValue' already exists in the model 'ModelId'"));

		// get the model
		final DescriptorModel<Integer> model = new DescriptorModel<Integer>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new IntegerIdsFactory());
		configuration.wireInstance(model);

		// create a new resource for an invalid ResourceModel
		model.createDescriptor("MyValue");
		model.createDescriptor("MyValue");
	}

	/**
	 * Tests the adding of an invalid descriptor type to a
	 * {@code DescriptorModel}.
	 */
	@Test
	public void testExceptionInvalidDescriptorType() {
		thrown.expect(DescriptorModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("Failed to add the descriptor '1' to the model 'ModelId'"));

		// create to models one to create a descriptor and add it to the other
		final DescriptorModel<Integer> modelIntIds = new DescriptorModel<Integer>(
				"ModelId", "ModelName", IntegerDescriptor.class,
				new IntegerIdsFactory(), new IndexedCollectionFactory());
		configuration.wireInstance(modelIntIds);
		final DescriptorModel<Long> modelLongIds = new DescriptorModel<Long>(
				"ModelId", "ModelName", LongDescriptor.class,
				new LongIdsFactory(), new IndexedCollectionFactory());
		configuration.wireInstance(modelLongIds);

		// add an invalid descriptor to the model
		final Descriptor<?, ?, ?> desc = modelIntIds.createDescriptor(1);
		modelLongIds.addDescriptor(desc);
	}

	/**
	 * Reset the {@code Locale}
	 */
	@After
	public void cleanUp() {
		Locale.setDefault(oldDefault);
	}
}
