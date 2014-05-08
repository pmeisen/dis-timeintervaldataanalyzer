package net.meisen.dissertation.model.data;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.InputStream;
import java.util.Collection;
import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.exceptions.DescriptorModelException;
import net.meisen.dissertation.exceptions.MetaDataModelException;
import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.dissertation.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.impl.idfactories.IntegerIdsFactory;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.api.IModuleHolder;
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
 * Tests the implementation of a {@code MetaDataModel}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "?")
public class TestMetaDataModel extends ExceptionBasedTest {
	private final static String testXmlModel = "/net/meisen/dissertation/model/data/testMetaDataModel.xml";

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private IConfiguration configuration;

	/**
	 * Helper method for the test to create a {@code MetaDataModel}.
	 * 
	 * @return an instance of a {@code MetaDataModel} used for testing purposes
	 */
	@SuppressWarnings("unchecked")
	protected MetaDataModel createTestModel() {
		final InputStream is = getClass().getResourceAsStream(testXmlModel);
		final IModuleHolder modules = configuration.loadDelayed("testloader",
				is);
		final MetaDataModel model = modules.getModule("metaDataModelId");
		model.addDescriptorModels((Collection<DescriptorModel<?>>) modules
				.getModule("descriptorModels"));

		return model;
	}

	/**
	 * Tests the creation of a {@code MetaDataModel}.
	 */
	@Test
	public void testMetaModelCreation() {
		final MetaDataModel model = createTestModel();

		// check the available resources and descriptors
		assertEquals(0, model.getDescriptors().size());

		// check the models
		assertNull(model.getDescriptorModel("NOMODEL"));
		assertNotNull(model.getDescriptorModel("ID1"));
		assertNotNull(model.getDescriptorModel("ID2"));

		// check some models more detailed
		final DescriptorModel<?> dModel1 = model.getDescriptorModel("ID1");
		assertEquals("ID1", dModel1.getId());
		assertEquals("ID1", dModel1.getName());
		assertEquals(GeneralDescriptor.class, dModel1.getDescriptorClass());
		final DescriptorModel<?> dModel2 = model.getDescriptorModel("ID2");
		assertEquals("ID2", dModel2.getId());
		assertEquals("ID2", dModel2.getName());
		assertEquals(GeneralDescriptor.class, dModel2.getDescriptorClass());
	}

	/**
	 * Tests the creation of a {@code Descriptor}.
	 */
	@Test
	public void testDescriptorCreation() {
		final MetaDataModel model = createTestModel();

		// create a new resource
		final UUID uuid = UUID.randomUUID();
		assertEquals(0, model.getDescriptors().size());
		model.createDescriptor("ID2", uuid);
		assertEquals(1, model.getDescriptors().size());

		// check the value
		@SuppressWarnings("unchecked")
		final GeneralDescriptor<Object> des = (GeneralDescriptor<Object>) model
				.getDescriptors().iterator().next();
		assertEquals(uuid, des.getValue());
	}

	/**
	 * Tests the retrieval from a {@code Descriptor} by value from the
	 * {@code MetaDataModel}.
	 */
	@Test
	public void testDescriptorRetrieval() {
		final MetaDataModel model = createTestModel();

		final UUID uuid = UUID.randomUUID();
		final Descriptor<?, ?, ?> descriptor = model.createDescriptor("ID2",
				uuid);
		assertEquals("ID2", descriptor.getModelId());
		assertEquals(uuid, descriptor.getValue());

		assertTrue(descriptor.getId() instanceof Integer);
		assertEquals(descriptor, model.getDescriptor("ID2", descriptor.getId()));
		assertEquals(descriptor, model.getDescriptorByValue("ID2", uuid));
		assertNull(model.getDescriptorByValue("ID1", "someName"));
	}

	/**
	 * Checks that an exception is thrown whenever a {@code DescriptorModel}
	 * with {@code null} is added.
	 */
	@Test
	public void testNullDescriptorModel() {
		thrown.expect(MetaDataModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("A DescriptorModel cannot be null"));

		final MetaDataModel model = createTestModel();
		model.addDescriptorModel(null);
	}

	/**
	 * Checks that an exception is thrown whenever a {@code DescriptorModel}
	 * with the same id is added.
	 */
	@Test
	public void testDuplicateDescriptorModel() {
		thrown.expect(MetaDataModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("A DescriptorModel with id 'ID2' already exists"));

		final MetaDataModel model = createTestModel();

		final DescriptorModel<Integer> descriptorModel = new DescriptorModel<Integer>(
				"ID2", GeneralDescriptor.class, new IntegerIdsFactory());
		model.addDescriptorModel(descriptorModel);
	}

	/**
	 * Tests if an exception is thrown when an invalid model identifier is used
	 * to create a descriptor.
	 */
	@Test
	public void testExceptionInvalidModelDescriptorCreation() {
		thrown.expect(MetaDataModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("Unable to find a model with id 'IDONTEXIST'"));

		// get the model
		final MetaDataModel model = createTestModel();

		// create a new resource for an invalid ResourceModel
		model.createDescriptor("IDONTEXIST", "My New ID3");
	}

	/**
	 * Tests if an exception is thrown when a value is used multiple times.
	 */
	@Test
	public void testExceptionDuplicatedValueDescriptorCreation() {
		thrown.expect(DescriptorModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("descriptor 'My New ID1' already exists in the model 'ID1'"));

		// get the model
		final MetaDataModel model = createTestModel();

		// create a new resource for an invalid ResourceModel
		model.createDescriptor("ID1", "My New ID1");
		model.createDescriptor("ID3", "My New ID3");
		model.createDescriptor("ID1", "My New ID1");
	}

	/**
	 * Tests the setting and getting of the {@code OfflineMode}.
	 */
	@Test
	public void testSetAndGetOfOfflineMode() {
		final MetaDataModel model = createTestModel();

		// check the default
		assertEquals(OfflineMode.find(null), model.getOfflineMode());
		for (final DescriptorModel<?> m : model.getDescriptorModels()) {
			assertEquals(OfflineMode.find(null), m.getOfflineMode());
		}

		// check the other values
		for (final OfflineMode mode : OfflineMode.values()) {
			model.setOfflineMode(mode);

			assertEquals(mode, model.getOfflineMode());
			for (final DescriptorModel<?> m : model.getDescriptorModels()) {
				assertEquals(mode, m.getOfflineMode());
			}
		}
	}
}
