package net.meisen.dissertation.models.impl.data;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Locale;
import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.data.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.exceptions.MetaDataModelException;
import net.meisen.general.sbconfigurator.api.IConfiguration;
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
 * Tests the implementation of a {@code MetaDataModel}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "net/meisen/dissertation/models/impl/data/testMetaDataModel.xml")
public class TestMetaDataModel {

	@Autowired(required = true)
	@Qualifier("resourceModels")
	private ArrayList<ResourceModel> resourceModels;

	@Autowired(required = true)
	@Qualifier("descriptorModels")
	private ArrayList<DescriptorModel> descriptorModels;

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private IConfiguration configuration;

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
	 * Helper method for the test to create a {@code MetaDataModel}.
	 * 
	 * @return an instance of a {@code MetaDataModel} used for testing purposes
	 */
	protected MetaDataModel createTestModel() {
		final MetaDataModel model = new MetaDataModel();
		configuration.wireInstance(model);
		model.init();

		model.addResourceModels(resourceModels);
		model.addDescriptorModels(descriptorModels);

		return model;
	}

	/**
	 * Tests the creation of a {@code MetaDataModel}.
	 */
	@Test
	public void testMetaModelCreation() {
		final MetaDataModel model = createTestModel();

		// check the available resources and descriptors
		assertEquals(0, model.getResources().size());
		assertEquals(0, model.getDescriptors().size());

		// check the models
		assertNull(model.getDescriptorModel("NOMODEL"));
		assertNull(model.getResourceModel("NOMODEL"));
		assertNotNull(model.getDescriptorModel("ID1"));
		assertNotNull(model.getDescriptorModel("ID2"));
		assertNotNull(model.getResourceModel("ID1"));
		assertNotNull(model.getResourceModel("ID2"));

		// check some models more detailed
		final ResourceModel rModel = model.getResourceModel("ID2");
		assertEquals("ID2", rModel.getId());
		assertEquals("ID2", rModel.getName());
		final DescriptorModel dModel1 = model.getDescriptorModel("ID1");
		assertEquals("ID1", dModel1.getId());
		assertEquals("ID1", dModel1.getName());
		assertEquals(String.class, dModel1.getDataType());
		final DescriptorModel dModel2 = model.getDescriptorModel("ID2");
		assertEquals("ID2", dModel2.getId());
		assertEquals("ID2", dModel2.getName());
		assertEquals(UUID.class, dModel2.getDataType());
	}

	/**
	 * Tests the creation of a {@code Resource}.
	 */
	@Test
	public void testResourceCreation() {
		final MetaDataModel model = createTestModel();

		// create a new resource
		assertEquals(0, model.getResources().size());
		model.createResource("ID2", "My New ID3");
		assertEquals(1, model.getResources().size());
		assertEquals("My New ID3", model.getResources().iterator().next()
				.getValue());
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
	 * Tests the retrieval from a {@code Descriptor} from the
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
		assertEquals(descriptor, model.getDescriptor(descriptor.getId()));
		assertEquals(descriptor, model.getDescriptor("ID2", uuid));
		assertNull(model.getDescriptor("ID1", "someName"));
	}

	/**
	 * Tests the retrieval from a {@code Resource} from the
	 * {@code MetaDataModel}.
	 */
	@Test
	public void testResourceRetrieval() {
		final MetaDataModel model = createTestModel();

		final Resource<?> resource = model.createResource("ID1", "someName");
		assertEquals("ID1", resource.getModelId());
		assertEquals("someName", resource.getValue());

		assertTrue(resource.getId() instanceof Integer);
		assertEquals(resource, model.getResource(resource.getId()));
		assertEquals(resource, model.getResource("ID1", "someName"));
		assertNull(model.getResource("ID2", "someName"));
	}

	/**
	 * Checks that an exception is thrown whenever a {@code ResourceModel} with
	 * {@code null} is added.
	 */
	@Test
	public void testNullResourceModel() {
		thrown.expect(MetaDataModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("A ResourceModel cannot be null"));

		final MetaDataModel model = createTestModel();
		model.addResourceModel(null);
	}

	/**
	 * Checks that an exception is thrown whenever a {@code ResourceModel} with
	 * the same id is added.
	 */
	@Test
	public void testDuplicateResourceModel() {
		thrown.expect(MetaDataModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("A ResourceModel with id 'ID1' already exists"));

		final MetaDataModel model = createTestModel();

		final ResourceModel resourceModel = new ResourceModel("ID1");
		model.addResourceModel(resourceModel);
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

		final DescriptorModel descriptorModel = new DescriptorModel("ID2",
				String.class);
		model.addDescriptorModel(descriptorModel);
	}

	/**
	 * Tests if an exception is thrown when an invalid model identifier is used
	 * to create a resource.
	 */
	@Test
	public void testExceptionInvalidModelResourceCreation() {
		thrown.expect(MetaDataModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("Unable to find a model with id 'IDONTEXIST'"));

		// get the model
		final MetaDataModel model = createTestModel();

		// create a new resource for an invalid ResourceModel
		model.createResource("IDONTEXIST", "My New ID3");
	}

	/**
	 * Tests if an exception is thrown when {@code null} as a model is used.
	 */
	@Test
	public void testExceptionNullModelResourceCreation() {
		thrown.expect(MetaDataModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("ResourceModel cannot be null"));

		// get the model
		final MetaDataModel model = createTestModel();

		// create a new resource for an invalid ResourceModel
		model.createResource((ResourceModel) null, "My New ID3");
	}

	/**
	 * Tests if an exception is thrown when a value is used multiple times.
	 */
	@Test
	public void testExceptionDuplicatedValueResourceCreation() {
		thrown.expect(MetaDataModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("resource 'My New ID3' already exists in the model 'ID1'"));

		// get the model
		final MetaDataModel model = createTestModel();

		// create a new resource for an invalid ResourceModel
		model.createResource("ID1", "My New ID3");
		model.createResource("ID2", "My New ID3");
		model.createResource("ID1", "My New ID3");
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
	 * Tests if an exception is thrown when {@code null} as a model is used.
	 */
	@Test
	public void testExceptionNullModelDescriptorCreation() {
		thrown.expect(MetaDataModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("DescriptorModel cannot be null"));

		// get the model
		final MetaDataModel model = createTestModel();

		// create a new resource for an invalid ResourceModel
		model.createDescriptor((DescriptorModel) null, "My New ID3");
	}

	/**
	 * Tests if an exception is thrown when a value is used multiple times.
	 */
	@Test
	public void testExceptionDuplicatedValueDescriptorCreation() {
		thrown.expect(MetaDataModelException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("descriptor 'My New ID3' already exists in the model 'ID1'"));

		// get the model
		final MetaDataModel model = createTestModel();

		// create a new resource for an invalid ResourceModel
		model.createDescriptor("ID1", "My New ID3");
		model.createDescriptor("ID3", "My New ID3");
		model.createDescriptor("ID1", "My New ID3");
	}

	/**
	 * Reset the {@code Locale}
	 */
	@After
	public void cleanUp() {
		Locale.setDefault(oldDefault);
	}
}
