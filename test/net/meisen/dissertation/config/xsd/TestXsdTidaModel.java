package net.meisen.dissertation.config.xsd;

import net.meisen.general.sbconfigurator.config.exception.ValidationFailedException;
import net.meisen.general.sbconfigurator.config.transformer.DefaultXsdValidator;

import org.junit.Rule;
import org.junit.Test;
import org.junit.matchers.JUnitMatchers;
import org.junit.rules.ExpectedException;

/**
 * Tests the xsd of the tida model.
 * 
 * @author pmeisen
 * 
 */
public class TestXsdTidaModel {

	/**
	 * Rule to evaluate exceptions
	 */
	@Rule
	public ExpectedException thrown = ExpectedException.none();

	/**
	 * the default validator to be used
	 */
	private DefaultXsdValidator xsdValidator = new DefaultXsdValidator();

	/**
	 * Tests a full model to be validated without any exception.
	 * 
	 * @throws ValidationFailedException
	 *             if the validation is invalid
	 */
	@Test
	public void testFullModel() throws ValidationFailedException {
		xsdValidator
				.validateFromClasspath("/net/meisen/dissertation/config/fullModel.xml");
	}

	/**
	 * Tests a full model using external data to be validated without any
	 * exception.
	 * 
	 * @throws ValidationFailedException
	 *             if the validation is invalid
	 */
	@Test
	public void testFullModelDataFromExternal()
			throws ValidationFailedException {
		xsdValidator
				.validateFromClasspath("/net/meisen/dissertation/config/fullModelDataFromExternal.xml");
	}

	/**
	 * Tests a model which only defines resources and no descriptors.
	 * 
	 * @throws ValidationFailedException
	 *             if the validation is invalid
	 */
	@Test
	public void testNoResourcesModel() throws ValidationFailedException {
		xsdValidator
				.validateFromClasspath("/net/meisen/dissertation/config/xsd/noResourcesModel.xml");
	}

	/**
	 * Tests a model which only defines descriptors and no resources.
	 * 
	 * @throws ValidationFailedException
	 *             if the validation is invalid
	 */
	@Test
	public void testNoDescriptorsModel() throws ValidationFailedException {
		xsdValidator
				.validateFromClasspath("/net/meisen/dissertation/config/xsd/noDescriptorsModel.xml");
	}

	/**
	 * Tests the simpliest model, i.e. just intervals.
	 * 
	 * @throws ValidationFailedException
	 *             if the validation is invalid
	 */
	@Test
	public void testSimpliestModel() throws ValidationFailedException {
		xsdValidator
				.validateFromClasspath("/net/meisen/dissertation/config/xsd/simpliestModel.xml");
	}

	/**
	 * Tests the usage of a configuration and it's validation
	 * 
	 * @throws ValidationFailedException
	 *             if the validation is invalid
	 */
	@Test
	public void testSimpleConfig() throws ValidationFailedException {
		xsdValidator
				.validateFromClasspath("/net/meisen/dissertation/config/xsd/simpleConfig.xml");
	}

	/**
	 * Tests the failure if several resources models are defined.
	 * 
	 * @throws ValidationFailedException
	 *             if the validation is invalid
	 */
	@Test
	public void testExceptionDoubleResourcesModel()
			throws ValidationFailedException {
		thrown.expect(ValidationFailedException.class);
		thrown.expectMessage(JUnitMatchers.containsString("validation failed"));

		xsdValidator
				.validateFromClasspath("/net/meisen/dissertation/config/xsd/exceptionDoubleResourcesModel.xml");
	}

	/**
	 * Tests the failure if several descriptors models are defined.
	 * 
	 * @throws ValidationFailedException
	 *             if the validation is invalid
	 */
	@Test
	public void testExceptionDoubleDescriptorsModel()
			throws ValidationFailedException {
		thrown.expect(ValidationFailedException.class);
		thrown.expectMessage(JUnitMatchers.containsString("validation failed"));

		xsdValidator
				.validateFromClasspath("/net/meisen/dissertation/config/xsd/exceptionDoubleDescriptorsModel.xml");
	}

	/**
	 * Tests the failure if no values are defined within the model.
	 * 
	 * @throws ValidationFailedException
	 *             if the validation is invalid
	 */
	@Test
	public void testExceptionNoValuesModel() throws ValidationFailedException {
		thrown.expect(ValidationFailedException.class);
		thrown.expectMessage(JUnitMatchers.containsString("validation failed"));

		xsdValidator
				.validateFromClasspath("/net/meisen/dissertation/config/xsd/exceptionNoValuesModel.xml");
	}

	/**
	 * Tests the exception thrown when a resource's identifier is not unique.
	 * 
	 * @throws ValidationFailedException
	 *             if the validation is invalid
	 */
	@Test
	public void testExceptionUniqueResourceModel()
			throws ValidationFailedException {
		thrown.expect(ValidationFailedException.class);
		thrown.expectMessage(JUnitMatchers.containsString("validation failed"));

		xsdValidator
				.validateFromClasspath("/net/meisen/dissertation/config/xsd/exceptionUniqueResourceModel.xml");
	}

	/**
	 * Tests the exception thrown when a descriptor's identifier is not unique.
	 * 
	 * @throws ValidationFailedException
	 *             if the validation is invalid
	 */
	@Test
	public void testExceptionUniqueDescriptorModel()
			throws ValidationFailedException {
		thrown.expect(ValidationFailedException.class);
		thrown.expectMessage(JUnitMatchers.containsString("validation failed"));

		xsdValidator
				.validateFromClasspath("/net/meisen/dissertation/config/xsd/exceptionUniqueDescriptorModel.xml");
	}

	/**
	 * Tests the exception which should be thrown when a reference to a resource
	 * is invalid.
	 * 
	 * @throws ValidationFailedException
	 *             if the validation is invalid
	 */
	@Test
	public void testExceptionReferenceResource()
			throws ValidationFailedException {
		thrown.expect(ValidationFailedException.class);
		thrown.expectMessage(JUnitMatchers.containsString("validation failed"));

		xsdValidator
				.validateFromClasspath("/net/meisen/dissertation/config/xsd/exceptionInvalidResourceReference.xml");
	}

	/**
	 * Tests the exception which should be thrown when a reference to a
	 * descriptor is invalid.
	 * 
	 * @throws ValidationFailedException
	 *             if the validation is invalid
	 */
	@Test
	public void testExceptionReferenceDescriptor()
			throws ValidationFailedException {
		thrown.expect(ValidationFailedException.class);
		thrown.expectMessage(JUnitMatchers.containsString("validation failed"));

		xsdValidator
				.validateFromClasspath("/net/meisen/dissertation/config/xsd/exceptionInvalidDescriptorReference.xml");
	}
}
