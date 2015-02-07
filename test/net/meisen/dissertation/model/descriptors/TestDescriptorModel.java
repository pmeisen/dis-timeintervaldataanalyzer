package net.meisen.dissertation.model.descriptors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.exceptions.DescriptorModelException;
import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.dissertation.impl.descriptors.DoubleDescriptor;
import net.meisen.dissertation.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.impl.descriptors.LongDescriptor;
import net.meisen.dissertation.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.impl.idfactories.LongIdsFactory;
import net.meisen.dissertation.impl.indexes.IndexFactory;
import net.meisen.dissertation.impl.indexes.MultipleIndexedCollection;
import net.meisen.dissertation.impl.indexes.TroveIntIndexedCollection;
import net.meisen.dissertation.impl.indexes.TroveLongIndexedCollection;
import net.meisen.dissertation.model.data.OfflineMode;
import net.meisen.general.sbconfigurator.config.DefaultConfiguration;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.Test;
import org.junit.matchers.JUnitMatchers;
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
@SystemProperty(property = "testBeans.selector", value = "net/meisen/dissertation/model/descriptors/testDescriptorModel.xml")
public class TestDescriptorModel extends ExceptionBasedTest {

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private DefaultConfiguration configuration;

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
	 * Test the setting and getting of the {@code OfflineMode}.
	 */
	@Test
	public void testOfflineMode() {
		final DescriptorModel<Integer> model = new DescriptorModel<Integer>(
				"id", GeneralDescriptor.class, new IntegerIdsFactory());

		// check the default
		assertEquals(OfflineMode.find(null), model.getOfflineMode());

		// check setting
		model.setOfflineMode(OfflineMode.FALSE);
		assertEquals(OfflineMode.FALSE, model.getOfflineMode());
		model.setOfflineMode(OfflineMode.AUTO);
		assertEquals(OfflineMode.AUTO, model.getOfflineMode());
		model.setOfflineMode(OfflineMode.TRUE);
		assertEquals(OfflineMode.TRUE, model.getOfflineMode());
	}

	/**
	 * Tests the implementation {@code DescriptorModel#getDescriptorIndex()}.
	 */
	@Test
	public void testGetDescriptorIndex() {
		// create a model using an integerIdsFactory
		final DescriptorModel<Integer> modelIntIds = new DescriptorModel<Integer>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new IntegerIdsFactory(), new IndexFactory());

		// check some settings
		assertTrue(modelIntIds.getDescriptorIndex() instanceof MultipleIndexedCollection);
		final MultipleIndexedCollection idxInt = (MultipleIndexedCollection) modelIntIds
				.getDescriptorIndex();
		assertTrue(
				idxInt.getIndexedCollection(0).getClass().getName(),
				idxInt.getIndexedCollection(0) instanceof TroveIntIndexedCollection);

		final DescriptorModel<Long> modelLongIds = new DescriptorModel<Long>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new LongIdsFactory(), new IndexFactory());

		// check some settings
		assertTrue(modelLongIds.getDescriptorIndex() instanceof MultipleIndexedCollection);
		final MultipleIndexedCollection idxLong = (MultipleIndexedCollection) modelLongIds
				.getDescriptorIndex();
		assertTrue(
				idxLong.getIndexedCollection(0).getClass().getName(),
				idxLong.getIndexedCollection(0) instanceof TroveLongIndexedCollection);
	}

	/**
	 * Tests the implementation {@code DescriptorModel#createDescriptor(Object)}
	 * .
	 */
	@Test
	public void testCreateDescriptorWithoutId() {

		// create a model using an integerIdsFactory
		final DescriptorModel<Integer> model1 = new DescriptorModel<Integer>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new IntegerIdsFactory(), new IndexFactory());

		// create a bunch of descriptors
		for (int i = 1; i < 100; i++) {
			final Descriptor<?, ?, ?> descriptor = model1
					.createDescriptor("TestValue" + i);
			assertNotNull(descriptor);
			assertEquals(descriptor.getId(), i);
			assertTrue(Integer.class.equals(descriptor.getId().getClass()));
			assertEquals(descriptor.getValue(), "TestValue" + i);
		}
		assertEquals(99, model1.getDescriptors().size());

		final DescriptorModel<Long> model2 = new DescriptorModel<Long>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new LongIdsFactory(), new IndexFactory());

		// create a bunch of descriptors
		for (long i = 1; i < 100; i++) {
			final Descriptor<?, ?, ?> descriptor = model2
					.createDescriptor("TestValue" + i);
			assertNotNull(descriptor);
			assertEquals(descriptor.getId(), i);
			assertTrue(Long.class.equals(descriptor.getId().getClass()));
			assertEquals(descriptor.getValue(), "TestValue" + i);
		}
		assertEquals(99, model2.getDescriptors().size());
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
				new LongIdsFactory(), new IndexFactory());
		final DescriptorModel<Long> modelLongIds2 = new DescriptorModel<Long>(
				"ModelId", "ModelName", LongDescriptor.class,
				new LongIdsFactory(), new IndexFactory());

		// add some descriptors created by the one into the other
		for (long i = 1; i < 100; i++) {
			modelLongIds2.addDescriptor(modelLongIds1.createDescriptor(i));
		}
		assertEquals(99, modelLongIds2.getDescriptors().size());
	}

	/**
	 * Tests the implementation {@code DescriptorModel#getDescriptor(Object)} .
	 */
	@Test
	public void testGetDescriptor() {
		final DescriptorModel<Long> modelLongIds = new DescriptorModel<Long>(
				"ModelId", "ModelName", LongDescriptor.class,
				new LongIdsFactory(), new IndexFactory());

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
				new LongIdsFactory(), new IndexFactory());

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

		// check the retrieval by string
		DescriptorModel<Long> modelDoubleDesc = new DescriptorModel<Long>(
				"ModelId", "ModelName", DoubleDescriptor.class,
				new LongIdsFactory(), new IndexFactory());

		final Descriptor<Double, ?, Long> oneDesc = modelDoubleDesc
				.addDescriptor(1000l, 1.00);
		assertEquals(oneDesc, modelDoubleDesc.getDescriptorByValue("1"));
		assertEquals(oneDesc, modelDoubleDesc.getDescriptorByValue("1.00"));
	}

	/**
	 * Tests the retrieval of a {@code LongDescriptor} via string using
	 * {@link DescriptorModel#getDescriptorByString(String)}.
	 */
	@Test
	public void testGetLongDescriptorByString() {

		// create the model
		final DescriptorModel<Integer> model = new DescriptorModel<Integer>(
				"ModelId", "ModelName", LongDescriptor.class,
				new IntegerIdsFactory(), new IndexFactory());

		// add some values
		for (long i = 1; i < 100; i++) {
			model.createDescriptor(100 - i);
		}

		// get the values by string
		Descriptor<?, ?, Integer> desc;

		// check the retrieval
		desc = model.getDescriptorByString("1");
		assertEquals(1l, desc.getValue());
		assertEquals(new Integer(99), desc.getId());

		desc = model.getDescriptorByString("99");
		assertEquals(99l, desc.getValue());
		assertEquals(new Integer(1), desc.getId());

		desc = model.getDescriptorByString("0");
		assertNull(desc);

		desc = model.getDescriptorByString("100");
		assertNull(desc);
	}

	/**
	 * Tests the retrieval of a {@code ResourceDescriptor} via string using
	 * {@link DescriptorModel#getDescriptorByString(String)}.
	 */
	@Test
	public void testGetResourceDescriptorByString() {

		// create the model
		final DescriptorModel<Integer> model = new DescriptorModel<Integer>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new IntegerIdsFactory(), new IndexFactory());
		model.setSupportsNullDescriptor(true);

		// add some values
		model.createDescriptor("Philipp");
		model.createDescriptor("Tobias");
		model.createDescriptor("Christian");
		model.createDescriptor("Marco");
		model.createDescriptor(null);

		// get the values by string
		Descriptor<?, ?, Integer> desc;

		// check the retrieval
		desc = model.getDescriptorByString("Philipp");
		assertEquals("Philipp", desc.getValue());
		assertEquals(new Integer(1), desc.getId());

		desc = model.getDescriptorByString("Marco");
		assertEquals("Marco", desc.getValue());
		assertEquals(new Integer(4), desc.getId());

		desc = model.getDescriptorByString(null);
		assertEquals(new Integer(5), desc.getId());
		assertNull(desc.getValue());

		desc = model.getDescriptorByString("Bodo");
		assertNull(desc);
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
				new IntegerIdsFactory(), new IndexFactory());
		configuration.wireInstance(modelIntIds);
		final DescriptorModel<Long> modelLongIds = new DescriptorModel<Long>(
				"ModelId", "ModelName", LongDescriptor.class,
				new LongIdsFactory(), new IndexFactory());
		configuration.wireInstance(modelLongIds);

		// add an invalid descriptor to the model
		final Descriptor<?, ?, ?> desc = modelIntIds.createDescriptor(1);
		modelLongIds.addDescriptor(desc);
	}

	/**
	 * Test the creation of a {@code Descriptor} using a {@code null} value.
	 */
	@Test
	public void testCreateUsingNull() {
		final DescriptorModel<Integer> model = new DescriptorModel<Integer>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new IntegerIdsFactory());
		model.setSupportsNullDescriptor(true);

		final Descriptor<?, ?, Integer> descriptor = model
				.createDescriptor(null);

		assertTrue(descriptor instanceof NullDescriptor);
		assertNull(descriptor.getValue());
		assertNotNull(model.getNullDescriptor());
		assertEquals(model.getNullDescriptor(), descriptor);
	}

	/**
	 * Test the creation of a {@code Descriptor} using a {@code null} value,
	 * which is not supported by the {@code DescriptorModel}.
	 */
	@Test
	public void testInvalidCreateUsingNull() {
		thrown.expect(DescriptorModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("Null-values are not supported by the model 'ModelId'"));

		final DescriptorModel<Integer> model = new DescriptorModel<Integer>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new IntegerIdsFactory());
		configuration.wireInstance(model);
		model.createDescriptor(null);
	}

	/**
	 * Tests the implementation of
	 * {@link DescriptorModel#addDescriptor(Object, Object)}.
	 */
	@Test
	public void testAddingDescriptorWithId() {
		final DescriptorModel<Integer> model = new DescriptorModel<Integer>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new IntegerIdsFactory(), new IndexFactory());
		configuration.wireInstance(model);
		model.setSupportsNullDescriptor(true);
		model.setFailOnDuplicates(false);

		// the model should only have the NullDescriptor so far
		assertEquals(model.getAllDescriptors().size(), 1);

		// add the NullDescriptor and make sure the result is equal
		assertEquals(model.getNullDescriptor(),
				model.addDescriptor(model.getNullDescriptorId(), null));

		// add a new descriptor and check if the adding returns the same object
		model.setFailOnDuplicates(true);
		Descriptor<UUID, ?, Integer> desc;
		desc = model.addDescriptor(5, UUID.randomUUID());
		assertTrue(desc == model.addDescriptor(desc.getId(), desc.getValue()));
	}

	/**
	 * Checks the exception to be thrown if another {@code NullDescriptor} is
	 * added with another id using
	 * {@link DescriptorModel#addDescriptor(Object, Object)}.
	 */
	@Test
	public void testExceptionAddingNullDescriptorWithDifferentId() {
		thrown.expect(DescriptorModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("cannot be added to the model"));

		// create the model
		final DescriptorModel<Integer> model = new DescriptorModel<Integer>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new IntegerIdsFactory(), new IndexFactory());
		configuration.wireInstance(model);
		model.setFailOnDuplicates(false);
		model.setSupportsNullDescriptor(true);

		// create the null descriptor
		model.getNullDescriptor();
		model.addDescriptor(model.getNullDescriptorId() + 1, null);
	}

	/**
	 * Checks the exception to be thrown if another {@code Descriptor} with the
	 * same id but another value is added using
	 * {@link DescriptorModel#addDescriptor(Object, Object)}.
	 */
	@Test
	public void testExceptionAddingDescriptorWithDifferentId() {
		thrown.expect(DescriptorModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("cannot be added to the model"));

		// create the model
		final DescriptorModel<Integer> model = new DescriptorModel<Integer>(
				"ModelId", "ModelName", GeneralDescriptor.class,
				new IntegerIdsFactory(), new IndexFactory());
		configuration.wireInstance(model);
		model.setFailOnDuplicates(false);
		model.setSupportsNullDescriptor(true);

		// create the null descriptor
		model.addDescriptor(1, UUID.randomUUID());
		model.addDescriptor(1, UUID.randomUUID());
	}
}
