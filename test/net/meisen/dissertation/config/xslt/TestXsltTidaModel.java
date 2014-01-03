package net.meisen.dissertation.config.xslt;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.meisen.dissertation.config.TIDAConfig;
import net.meisen.dissertation.config.xslt.mock.MyOwnTestDescriptor;
import net.meisen.dissertation.data.impl.dataretriever.DbConnectionConfig;
import net.meisen.dissertation.data.impl.dataretriever.DbDataRetriever;
import net.meisen.dissertation.data.impl.dataretriever.RandomConnectionConfig;
import net.meisen.dissertation.data.impl.dataretriever.RandomDataRetriever;
import net.meisen.dissertation.data.impl.idfactories.LongIdsFactory;
import net.meisen.dissertation.data.impl.idfactories.UuIdsFactory;
import net.meisen.dissertation.models.impl.data.Descriptor;
import net.meisen.dissertation.models.impl.data.DescriptorModel;
import net.meisen.dissertation.models.impl.data.MetaDataModel;
import net.meisen.dissertation.models.impl.data.Resource;
import net.meisen.dissertation.models.impl.data.ResourceModel;
import net.meisen.dissertation.models.impl.dataretriever.IDataRetrieverConfiguration;
import net.meisen.dissertation.models.impl.dataretriever.IQueryConfiguration;
import net.meisen.general.sbconfigurator.config.DefaultConfiguration;
import net.meisen.general.sbconfigurator.config.exception.InvalidXsltException;
import net.meisen.general.sbconfigurator.config.exception.TransformationFailedException;
import net.meisen.general.sbconfigurator.config.transformer.ClasspathXsltUriResolver;
import net.meisen.general.sbconfigurator.config.transformer.DefaultXsltTransformer;
import net.meisen.general.sbconfigurator.config.transformer.DefaultXsltUriResolver;
import net.meisen.general.sbconfigurator.factories.MergedCollection;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Test to test the transformation of a model using xslt and the interpretation
 * of the created Spring-beans.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TIDAConfig.class)
@ContextFile("sbconfigurator-core.xml")
public class TestXsltTidaModel {
	private final String pathToFM = "/net/meisen/dissertation/config/fullModel.xml";
	private final String pathToFMDFE = "/net/meisen/dissertation/config/fullModelDataFromExternal.xml";

	/**
	 * the default xslt transformer used for testing
	 */
	private DefaultXsltTransformer transformer;

	/**
	 * {@code OutputStream} to write to
	 */
	private final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private DefaultConfiguration configuration;

	/**
	 * Initializes the {@code transformer} to point to the correct {@code xslt}.
	 * 
	 * @throws InvalidXsltException
	 *             if the xslt is invalid
	 */
	@Before
	public void init() throws InvalidXsltException {
		transformer = new DefaultXsltTransformer(
				new DefaultXsltUriResolver(new ClasspathXsltUriResolver(),
						new DataRetrieverXsltResolver()));
		transformer
				.setXsltTransformer("/net/meisen/dissertation/config/xslt/modelToSpring.xslt");

	}

	/**
	 * Tests if an additional {@code Descriptor} implementation is added
	 * correctly to the map.
	 * 
	 * @throws TransformationFailedException
	 *             if the XSLT cannot be applied
	 */
	@Test
	public void testAddedDescriptors() throws TransformationFailedException {
		// @formatter:off
		 transformer.transformFromClasspath("/net/meisen/dissertation/config/xslt/configAddDescriptors.xml",
						outputStream);
		final String output = getOutput();
		
		assertTrue(output, match(output,
						"<entry key=\"java.util.List\" value=\"" + MyOwnTestDescriptor.class.getName() + "\"/>",
						"</map>"));
		// @formatter:on
	}

	/**
	 * Tests the changing of the {@code ResourcesFactory}/
	 * {@code DescriptorsFactory} and their {@code IdsFactory}.
	 * 
	 * @throws TransformationFailedException
	 *             if the XSLT cannot be applied
	 */
	@Test
	public void testChangeOfFactories() throws TransformationFailedException {
		// @formatter:off
		transformer.transformFromClasspath("/net/meisen/dissertation/config/xslt/configChangeFactories.xml",
						outputStream);
		final String output = getOutput();

		assertTrue(output, match(output,
						"<bean id=\"descriptorsFactory-\\E[a-z\\-0-9]+\\Q\" class=\"" + DefaultValues.getDescriptorsFactoryImplementation() + "\">",
						"<constructor-arg type=\"" + net.meisen.dissertation.data.IIdsFactory.class.getName() + "\">",
						"<bean class=\"" + LongIdsFactory.class.getName() + "\"/>",
						"</constructor-arg>"));
		assertTrue(output, match(output,
						"<bean id=\"resourcesFactory-\\E[a-z\\-0-9]+\\Q\" class=\"my.own.impl.ResourceFactory\">",
						"<constructor-arg type=\"" + net.meisen.dissertation.data.IIdsFactory.class.getName() + "\">",
						"<bean class=\"" + UuIdsFactory.class.getName() + "\"/>",
						"</constructor-arg>"));
		// @formatter:on
	}

	/**
	 * Tests some transformations against the full configuration.
	 * 
	 * @throws TransformationFailedException
	 *             if the XSLT cannot be applied
	 */
	@Test
	public void testFullModelTransformation()
			throws TransformationFailedException {
		transformer.transformFromClasspath(pathToFM, outputStream);

		final String output = getOutput();

		// check the resourcemodels
		for (int i = 1; i <= 3; i++) {
			final String id = "R" + i;

			// @formatter:off
			assertTrue(output, match(output,
							"<bean id=\"resourcemodel-\\E[a-z\\-0-9]+\\Q-" + id + "\" class=\"net.meisen.dissertation.models.impl.data.ResourceModel\">",
							"<constructor-arg type=\"java.lang.String\">",
							"<value>" + id + "</value>", "</constructor-arg>"));
			// @formatter:on
		}

		// check descriptormodels
		for (int i = 1; i <= 4; i++) {
			final String id = "D" + i;

			// @formatter:off
			assertTrue(output, match(output,
							"<bean id=\"descriptormodel-\\E[a-z\\-0-9]+\\Q-" + id + "\" class=\"net.meisen.dissertation.models.impl.data.DescriptorModel\">",
							"<constructor-arg type=\"java.lang.String\">",
							"<value>" + id + "</value>", "</constructor-arg>"));
			// @formatter:on
		}
	}

	/**
	 * Tests the transformation of a full model configuration using external
	 * data-sources.
	 * 
	 * @throws TransformationFailedException
	 *             if the XSLT cannot be applied
	 */
	@Test
	public void testFullModelDataFromExternalTransformation()
			throws TransformationFailedException {
		transformer.transformFromClasspath(pathToFMDFE, outputStream);
		final String output = getOutput();
		// System.out.println(output);

		// check that we have the dataretriever as map
		final List<String> dr = new ArrayList<String>();

		// add the db_butRandom
		// @formatter:off
		dr.add("<bean id=\"dataretriever-\\E[a-z\\-0-9]+\\Q-db_butRandom\" class=\"" + RandomDataRetriever.class.getName() + "\">");
		dr.add("<constructor-arg type=\"" + IDataRetrieverConfiguration.class.getName() + "\">");
		dr.add("<bean class=\"" + RandomConnectionConfig.class.getName() + "\" xmlns:rnd=\"http://dev.meisen.net/xsd/dissertation/model/rnd\">");
		dr.add("<property name=\"amount\" value=\"100\"/>");
		dr.add("<property name=\"type\" value=\"java.lang.Integer\"/>");
		dr.add("</bean>");
		dr.add("</constructor-arg>");
		dr.add("</bean>");

		// add the myOwnId
		dr.add("<bean id=\"dataretriever-\\E[a-z\\-0-9]+\\Q-myOwnId\" class=\"" + RandomDataRetriever.class.getName() + "\">");
		dr.add("<constructor-arg type=\"" + IDataRetrieverConfiguration.class.getName() + "\">");
		dr.add("<bean class=\"" + RandomConnectionConfig.class.getName() + "\" xmlns:rnd=\"http://dev.meisen.net/xsd/dissertation/model/rnd\">");
		dr.add("<property name=\"amount\" value=\"500\"/>");
		dr.add("<property name=\"type\" value=\"java.lang.Double\"/>");
		dr.add("</bean>");
		dr.add("</constructor-arg>");
		dr.add("</bean>");

		// add the rnd_test
		dr.add("<bean id=\"dataretriever-\\E[a-z\\-0-9]+\\Q-rnd_test\" class=\"" + RandomDataRetriever.class.getName() + "\">");
		dr.add("<constructor-arg type=\"" + IDataRetrieverConfiguration.class.getName() + "\">");
		dr.add("<bean class=\"" + RandomConnectionConfig.class.getName() + "\" xmlns:rnd=\"http://dev.meisen.net/xsd/dissertation/model/rnd\">");
		dr.add("<property name=\"amount\" value=\"1000\"/>");
		dr.add("<property name=\"type\" value=\"java.lang.String\"/>");
		dr.add("</bean>");
		dr.add("</constructor-arg>");
		dr.add("</bean>");

		// add the db_test
		dr.add("<bean id=\"dataretriever-\\E[a-z\\-0-9]+\\Q-db_test\" class=\"" + DbDataRetriever.class.getName() + "\">");
		dr.add("<constructor-arg type=\"" + IDataRetrieverConfiguration.class.getName() + "\">");
		dr.add("<bean class=\"" + DbConnectionConfig.class.getName() + "\" xmlns:db=\"http://dev.meisen.net/xsd/dissertation/model/db\">");
		dr.add("<property name=\"type\" value=\"jdbc\"/>");
		dr.add("<property name=\"url\" value=\"jdbc:hsqldb:hsql://localhost:6666/tidaGhTasks\"/>");
		dr.add("<property name=\"driver\" value=\"org.hsqldb.jdbcDriver\"/>");
		dr.add("<property name=\"username\" value=\"SA\"/>");
		dr.add("<property name=\"password\" value=\"\"/>");
		dr.add("</bean>");
		dr.add("</constructor-arg>");
		dr.add("</bean>");
		// @formatter:on

		assertTrue(output, match(output, dr.toArray(new String[dr.size()])));

		// check that the resources with a dataretriever are not added somehow
		final Map<String, String> expectedResources = new LinkedHashMap<String, String>();
		expectedResources.put("R1", "Edison");
		expectedResources.put("R3", "NoValue");

		// get the lines
		// @formatter:off
		final List<String> rl = new ArrayList<String>();
		rl.add("<bean id=\"resources-\\E[a-z\\-0-9]+\\Q\" class=\"" + MergedCollection.class.getName() + "\">");
		rl.add("<property name=\"collections\">");
		rl.add("<list value-type=\"java.util.Collection\">");
		// @formatter:on

		// first we expect the resources directly defined within the xml
		// @formatter:off
		rl.add("<list value-type=\"net.meisen.dissertation.models.impl.data.Resource\">");
		for (final Entry<String, String> e : expectedResources.entrySet()) {
			rl.add("<bean factory-bean=\"resourcesFactory-\\E[a-z\\-0-9]+\\Q\" factory-method=\"createResource\">");
			rl.add("<constructor-arg type=\"" + ResourceModel.class.getName() + "\">");
			rl.add("<ref bean=\"resourcemodel-\\E[a-z\\-0-9]+\\Q-" + e.getKey() + "\"/>");
			rl.add("</constructor-arg>");
			rl.add("<constructor-arg type=\"java.lang.String\">");
			rl.add("<value>" + e.getValue() + "</value>");
			rl.add("</constructor-arg>");
			rl.add("</bean>");
		}
		rl.add("</list>");
		// @formatter:on

		// next the dataRetriever should appear
		// @formatter:off
		rl.add("<bean factory-bean=\"resourcesFactory-\\E[a-z\\-0-9]+\\Q\" factory-method=\"createResources\">");
		rl.add("<constructor-arg type=\"" + ResourceModel.class.getName() + "\">");
		rl.add("<ref bean=\"resourcemodel-\\E[a-z\\-0-9]+\\Q-R2\"/>");
		rl.add("</constructor-arg>");
		rl.add("<constructor-arg type=\"java.util.Collection\">");
		
		rl.add("<bean class=\"org.springframework.beans.factory.config.MethodInvokingFactoryBean\">");
		rl.add("<property name=\"targetMethod\" value=\"transform\"/>");
		rl.add("<property name=\"targetObject\">");
		
		rl.add("<bean factory-bean=\"dataretriever-\\E[a-z\\-0-9]+\\Q-db_test\" factory-method=\"retrieve\">");
		rl.add("<constructor-arg type=\"" + IQueryConfiguration.class.getName() + "\">");
		rl.add("<null xmlns:db=\"http://dev.meisen.net/xsd/dissertation/model/db\"/>");
		rl.add("</constructor-arg>");
		rl.add("</bean>");
		
		rl.add("</property>");
		rl.add("</bean>");
		
		rl.add("</constructor-arg>");
		rl.add("</bean>");
		// @formatter:on

		rl.add("</list>");
		rl.add("</property>");
		rl.add("</bean>");

		// check the lines
		assertTrue(output, match(output, rl.toArray(new String[rl.size()])));

		// check that the descriptors with a dataretriever are not added somehow
		final Map<String, String> expectedDescriptors = new LinkedHashMap<String, String>();
		expectedDescriptors.put("D2", "2");
		expectedDescriptors.put("D3", "Some Value");
		final Map<String, String> expectedRetrDescriptors = new LinkedHashMap<String, String>();
		expectedRetrDescriptors.put("D1", "db_test");
		expectedRetrDescriptors.put("D4", "rnd_test");

		// get the lines
		final List<String> dl = new ArrayList<String>();
		// @formatter:off
		dl.add("<bean id=\"descriptors-\\E[a-z\\-0-9]+\\Q\" class=\"" + MergedCollection.class.getName() + "\">");
		dl.add("<property name=\"collections\">");
		dl.add("<list value-type=\"java.util.Collection\">");
		// @formatter:on

		// first we expect the descriptors directly defined within the xml
		// @formatter:off
		dl.add("<list value-type=\"net.meisen.dissertation.models.impl.data.Descriptor\">");
		for (final Entry<String, String> e : expectedDescriptors.entrySet()) {
			dl.add("<bean factory-bean=\"descriptorsFactory-\\E[a-z\\-0-9]+\\Q\" factory-method=\"createDescriptor\">");
			dl.add("<constructor-arg type=\"" + DescriptorModel.class.getName()	+ "\">");
			dl.add("<ref bean=\"descriptormodel-\\E[a-z\\-0-9]+\\Q-" + e.getKey() + "\"/>");
			dl.add("</constructor-arg>");
			dl.add("<constructor-arg type=\"java.lang.Object\">");
			dl.add("<value>" + e.getValue() + "</value>");
			dl.add("</constructor-arg>");
			dl.add("</bean>");
		}
		dl.add("</list>");
		// @formatter:on

		// next the dataRetriever should appear
		// @formatter:off
		for (final Entry<String, String> e : expectedRetrDescriptors.entrySet()) {
			dl.add("<bean factory-bean=\"descriptorsFactory-\\E[a-z\\-0-9]+\\Q\" factory-method=\"createDescriptors\">");
			dl.add("<constructor-arg type=\"" + DescriptorModel.class.getName() + "\">");
			dl.add("<ref bean=\"descriptormodel-\\E[a-z\\-0-9]+\\Q-" + e.getKey() + "\"/>");
			dl.add("</constructor-arg>");
			dl.add("<constructor-arg type=\"java.util.Collection\">");
			
			dl.add("<bean class=\"org.springframework.beans.factory.config.MethodInvokingFactoryBean\">");
			dl.add("<property name=\"targetMethod\" value=\"transform\"/>");
			dl.add("<property name=\"targetObject\">");
			
			dl.add("<bean factory-bean=\"dataretriever-\\E[a-z\\-0-9]+\\Q-" + e.getValue() + "\" factory-method=\"retrieve\">");
			dl.add("<constructor-arg type=\"" + IQueryConfiguration.class.getName() + "\">");
			if (e.getValue().startsWith("db")) {
				dl.add("<null xmlns:db=\"http://dev.meisen.net/xsd/dissertation/model/db\"/>");
			} else {
				dl.add("<null/>");
			}
			dl.add("</constructor-arg>");
			dl.add("</bean>");
			
			dl.add("</property>");
			dl.add("</bean>");
			
			dl.add("</constructor-arg>");
			dl.add("</bean>");
		}
		// @formatter:on

		dl.add("</list>");
		dl.add("</property>");
		dl.add("</bean>");

		// check the lines
		assertTrue(output, match(output, dl.toArray(new String[dl.size()])));
	}

	/**
	 * Tests the created {@code MetaDataModel}.
	 */
	@Test
	public void testFullModelCreation() {
		final InputStream xml = getClass().getResourceAsStream(pathToFM);
		final Map<String, Object> modules = configuration.loadDelayed(
				"tidaModelBeans", xml);

		// get the model
		final MetaDataModel m = (MetaDataModel) modules.get(DefaultValues
				.getGeneratedModuleName());
		assertNotNull(m);
		assertEquals("myModel", m.getId());
		assertEquals("My wonderful Model", m.getName());

		// check the resources and descriptors
		final Collection<Resource<?>> res = m.getResources();
		final Collection<Descriptor<?, ?, ?>> des = m.getDescriptors();
		assertEquals(7, res.size());
		assertEquals(14, des.size());

		// check all the created identifiers of the resources
		final Set<UUID> uuids = new HashSet<UUID>();
		for (final Resource<?> r : res) {
			assertTrue(r.getId() instanceof UUID);
			assertTrue(uuids.add((UUID) r.getId()));
		}

		// check all the created identifiers of the descriptors
		final Set<String> expectedValues = new HashSet<String>(Arrays.asList(
				"A", "B", "C", "D", "E", "F", "G", "H", "I"));
		final Set<Long> longids = new HashSet<Long>();
		for (final Descriptor<?, ?, ?> d : des) {
			assertTrue(d.getId() instanceof Long);
			assertTrue(longids.add((Long) d.getId()));

			if (d instanceof MyOwnTestDescriptor) {

				@SuppressWarnings("unchecked")
				final MyOwnTestDescriptor<Long> myOwn = (MyOwnTestDescriptor<Long>) d;
				assertEquals(d.getModelName(), "D4");

				// check the values
				final List<String> values = myOwn.getValueList();
				assertTrue(expectedValues.removeAll(values));
			}
		}

		// check if all the expected values were retrieved
		assertEquals(0, expectedValues.size());
	}

	/**
	 * Tests if the full model configuration with external data-sources can be
	 * read.
	 */
	@Test
	public void testFullModelDataFromExternalCreation() {
		final InputStream xml = getClass().getResourceAsStream(pathToFMDFE);
		final Map<String, Object> modules = configuration.loadDelayed(
				"tidaModelBeans", xml);

		// get the model
		final MetaDataModel m = (MetaDataModel) modules.get(DefaultValues
				.getGeneratedModuleName());
		assertNotNull(m);
		assertEquals("modelWithExternalSources", m.getId());
		assertEquals("modelWithExternalSources", m.getName());

	}

	/**
	 * Helper method to retrieve the generated output as string.
	 * 
	 * @return the generated output as string
	 */
	protected String getOutput() {
		try {
			return new String(outputStream.toByteArray(), "UTF8");
		} catch (UnsupportedEncodingException e) {
			// ignore
			return null;
		}
	}

	/**
	 * Helper method to match documents against regular expressions lines.
	 * 
	 * @param output
	 *            the document to be matched
	 * @param lines
	 *            the lines (as regular expressions) which must be in the
	 *            sequence within the document
	 * @return {@code true} if the document matches, otherwise {@code false}
	 */
	protected boolean match(final String output, final String... lines) {

		// create the expected regular expression
		String regEx = "";
		regEx += "^.*";

		// append the lines
		for (final String line : lines) {
			regEx += "\\s*\\Q" + line + "\\E";
		}
		regEx += ".*$";

		final Pattern pattern = Pattern.compile(regEx, Pattern.DOTALL);
		final Matcher matcher = pattern.matcher(output);
		return matcher.matches();
	}
}
