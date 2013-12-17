package net.meisen.dissertation.data.impl.descriptors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.math.BigInteger;
import java.util.Locale;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.data.impl.descriptors.DescriptorsFactory;
import net.meisen.dissertation.data.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.exceptions.DescriptorsFactoryException;
import net.meisen.dissertation.models.impl.data.Descriptor;
import net.meisen.dissertation.models.impl.data.DescriptorModel;
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
 * Tests the implementation of the {@code DescriptorFactory}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "net/meisen/dissertation/data/impl/descriptors/testDescriptorsFactories.xml")
public class TestDescriptorsFactory {

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
	 * Tests the creation of a sample {@code Descriptor}.
	 */
	@Test
	public void testCreateDescriptor() {
		final DescriptorModel model = new DescriptorModel("SampleDescription",
				Integer.class);
		final DescriptorsFactory factory = configuration
				.getModule("descriptorsFactoryWithIntegerIds");
		final Descriptor<?, ?, ?> subject = factory
				.createDescriptor(model, "1000");

		// check the generated value
		assertNotNull(subject.getId());
		assertTrue(subject.getId() instanceof Integer);
		assertEquals(model.getName(), subject.getModelName());
		assertEquals("1000", subject.toString());
	}

	/**
	 * Tests the usage of the default {@code Descriptor} if no other is
	 * available.
	 */
	@Test
	public void testFallBackToDefault() {
		final DescriptorModel model = new DescriptorModel("SampleDescription",
				BigInteger.class);
		final DescriptorsFactory factory = configuration
				.getModule("descriptorsFactoryWithIntegerIds");
		final Descriptor<?, ?, ?> subject = factory.createDescriptor(model,
				BigInteger.valueOf(1000));

		// check the generated value
		assertNotNull(subject.getId());
		assertTrue(subject.getId() instanceof Integer);
		assertEquals(model.getName(), subject.getModelName());
		assertTrue(subject instanceof GeneralDescriptor);
		assertEquals(BigInteger.valueOf(1000),
				((GeneralDescriptor<?>) subject).getValue());
	}

	/**
	 * Tests the exception which should be generated when creating a
	 * {@code Descriptor} with an invalid value.
	 */
	@Test
	public void testCreateDescriptorWithInvalidValue() {
		thrown.expect(DescriptorsFactoryException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("Unable to create a descriptor"));

		final DescriptorModel model = new DescriptorModel("SampleDescription",
				Integer.class);
		final DescriptorsFactory factory = configuration
				.getModule("descriptorsFactoryWithIntegerIds");
		factory.createDescriptor(model, "1000.0");
	}

	/**
	 * Tests the creation of a {@code Descriptor} using an invalid type match
	 * (i.e. no constructor available).
	 */
	@Test
	public void testCreateDescriptorWithInvalidTypeMatch() {
		thrown.expect(DescriptorsFactoryException.class);
		thrown.expectMessage(JUnitMatchers
				.containsString("Unable to create a descriptor"));

		final DescriptorModel model = new DescriptorModel("SampleDescription",
				BigInteger.class);
		final DescriptorsFactory factory = configuration
				.getModule("descriptorsFactoryWithIntegerIds");
		factory.createDescriptor(model, "1000");
	}

	/**
	 * Reset the {@code Locale}
	 */
	@After
	public void cleanUp() {
		Locale.setDefault(oldDefault);
	}
}
