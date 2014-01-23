package net.meisen.dissertation.config.xsd;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import net.meisen.general.sbconfigurator.config.exception.ValidationFailedException;
import net.meisen.general.sbconfigurator.config.transformer.DefaultXsdValidator;

import org.junit.Rule;
import org.junit.Test;
import org.junit.matchers.JUnitMatchers;
import org.junit.rules.ExpectedException;
import org.xml.sax.SAXParseException;

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
	 * Tests a model which only defines some descriptors.
	 * 
	 * @throws ValidationFailedException
	 *             if the validation is invalid
	 */
	@Test
	public void testSimpleDescriptorsModel() throws ValidationFailedException {
		xsdValidator
				.validateFromClasspath("/net/meisen/dissertation/config/xsd/simpleDescriptors.xml");
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

		assertException(
				"/net/meisen/dissertation/config/xsd/exceptionDoubleDescriptorsModel.xml",
				"Invalid content was found starting with element 'descriptors'");
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

		assertException(
				"/net/meisen/dissertation/config/xsd/exceptionNoValuesModel.xml",
				"The content of element 'meta' is not complete");
	}

	/**
	 * Helper method to validate the cause of a {@code SAXParseException}.
	 * 
	 * @param xml
	 *            the xml to be validated
	 * @param contains
	 *            the substring of the {@code SAXParseException}'s message
	 *            
	 * @throws ValidationFailedException
	 *             the main exception thrown
	 */
	protected void assertException(final String xml, final String contains)
			throws ValidationFailedException {

		try {
			xsdValidator.validateFromClasspath(xml);
		} catch (final ValidationFailedException e) {
			final Throwable cause = e.getCause();
			assertNotNull(cause);
			assertTrue(cause instanceof SAXParseException);
			assertTrue(cause.getMessage(), cause.getMessage()
					.contains(contains));

			throw e;
		}
	}
}
