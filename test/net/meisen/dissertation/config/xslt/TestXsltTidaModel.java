package net.meisen.dissertation.config.xslt;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.meisen.dissertation.config.TIDAConfig;
import net.meisen.dissertation.config.xslt.mock.MockIndexedCollectionFactory;
import net.meisen.dissertation.data.impl.descriptors.DoubleDescriptor;
import net.meisen.dissertation.data.impl.descriptors.GeneralDescriptor;
import net.meisen.dissertation.data.impl.descriptors.IntegerDescriptor;
import net.meisen.dissertation.data.impl.descriptors.ListDescriptor;
import net.meisen.dissertation.data.impl.descriptors.LongDescriptor;
import net.meisen.dissertation.data.impl.descriptors.ResourceDescriptor;
import net.meisen.dissertation.help.Db;
import net.meisen.dissertation.models.impl.data.Descriptor;
import net.meisen.dissertation.models.impl.data.DescriptorModel;
import net.meisen.dissertation.models.impl.data.MetaDataModel;
import net.meisen.dissertation.data.impl.indexes.IndexedCollectionFactory;
import net.meisen.general.sbconfigurator.api.IModuleHolder;
import net.meisen.general.sbconfigurator.config.DefaultConfiguration;
import net.meisen.general.sbconfigurator.config.exception.InvalidXsltException;
import net.meisen.general.sbconfigurator.config.exception.TransformationFailedException;
import net.meisen.general.sbconfigurator.config.transformer.DefaultXsltTransformer;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
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
@ContextFile("test-sbconfigurator-core.xml")
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

	private IModuleHolder modulesHolder;

	private Db db;

	/**
	 * Initializes the {@code transformer} to point to the correct {@code xslt}.
	 * 
	 * @throws InvalidXsltException
	 *             if the xslt is invalid
	 */
	@Before
	public void init() throws InvalidXsltException {
		transformer = (DefaultXsltTransformer) configuration
				.getXsltTransformer();
		transformer
				.setXsltTransformer("/net/meisen/dissertation/config/xslt/modelToSpring.xslt");

	}

	/**
	 * Helper method to load the specified database.
	 * 
	 * @param classpathDb
	 *            the path to the database
	 * 
	 * @return the created {@code DB}.
	 * 
	 * @throws IOException
	 *             if the path leads to an exception
	 */
	protected Db getDb(final String classpathDb) throws IOException {
		if (db != null) {
			db.shutDownDb();
		}

		db = new Db();
		db.addDb("tidaTestData", classpathDb);
		db.setUpDb();

		return db;
	}

	/**
	 * Gets the model created by the specified {@code xml}.
	 * 
	 * @param xml
	 *            the path to the xml with the {@code MetaDataModel} to be
	 *            loaded
	 * 
	 * @return the loaded {@code MetaDataModel}
	 */
	public MetaDataModel getModel(final String xml) {
		final InputStream res = getClass().getResourceAsStream(xml);
		if (modulesHolder != null) {
			modulesHolder.release();
		}
		modulesHolder = configuration.loadDelayed("tidaModelBeans", res);

		return modulesHolder.getModule(DefaultValues.METADATAMODEL_ID);
	}

	/**
	 * Tests the replacement of the default {@code IndexedCollectionFactory}.
	 * 
	 * @throws TransformationFailedException
	 *             if the file cannot be tansformed
	 */
	@Test
	public void testDefaultFactories() throws TransformationFailedException {
		final MetaDataModel model = getModel("/net/meisen/dissertation/config/xslt/configDefaultFactories.xml");
		final Class<?> res = model.getIndexedCollectionFactory().getClass();

		assertTrue("Instance of '" + res.getName() + "'",
				res.equals(IndexedCollectionFactory.class));
	}

	/**
	 * Tests the replacement of the default {@code IndexedCollectionFactory}.
	 * 
	 * @throws TransformationFailedException
	 *             if the file cannot be transformed
	 */
	@Test
	public void testChangedFactories() throws TransformationFailedException {
		final MetaDataModel model = getModel("/net/meisen/dissertation/config/xslt/configChangeFactories.xml");
		final Class<?> res = model.getIndexedCollectionFactory().getClass();

		assertTrue("Instance of '" + res.getName() + "'",
				res.equals(MockIndexedCollectionFactory.class));
	}

	/**
	 * Tests the implementation of {@link MetaDataModel#getDescriptorModels()}
	 * and {@link MetaDataModel#getDescriptorModel(String)}.
	 * 
	 * @throws TransformationFailedException
	 *             if the file cannot be transformed
	 */
	@Test
	public void testGetDescriptorModel() throws TransformationFailedException {
		final MetaDataModel model = getModel("/net/meisen/dissertation/config/xslt/descriptors.xml");

		assertEquals(2, model.getDescriptorModels().size());
		assertNotNull(model.getDescriptorModel("D1"));
		assertNotNull(model.getDescriptorModel("D2"));
		assertNull(model.getDescriptorModel("NOTKNOWN"));
		assertNull(model.getDescriptorModel("INVALID"));
		assertNull(model.getDescriptorModel(null));
		assertNull(model.getDescriptorModel(""));
	}

	/**
	 * Tests the implementation of
	 * {@link MetaDataModel#getDescriptor(String, Object)} and
	 * {@link MetaDataModel#getDescriptorByValue(String, Object)}.
	 * 
	 * @throws TransformationFailedException
	 *             if the file cannot be transformed
	 */
	@Test
	public void testGetDescriptor() throws TransformationFailedException {
		final MetaDataModel model = getModel("/net/meisen/dissertation/config/xslt/descriptors.xml");

		assertEquals(5, model.getDescriptors().size());

		// check the DescriptorModels
		DescriptorModel<?> desModel;
		desModel = model.getDescriptorModel("D1");
		assertNotNull(desModel);
		assertEquals(2, desModel.size());
		assertNotNull(model.getDescriptorByValue("D1", "IchHabeHunger"));
		assertNotNull(model.getDescriptorByValue("D1", "IchBinHungrig"));

		desModel = model.getDescriptorModel("D2");
		assertNotNull(desModel);
		assertEquals(3, desModel.size());
		assertNotNull(model.getDescriptor("D2", 1));
		assertNotNull(model.getDescriptor("D2", 2));
		assertNotNull(model.getDescriptor("D2", 3));
	}

	/**
	 * Tests the implementation of an extension.
	 * 
	 * @throws TransformationFailedException
	 *             if the file cannot be transformed
	 */
	@Test
	public void testDescriptorModelExtension()
			throws TransformationFailedException {
		final MetaDataModel model = getModel("/net/meisen/dissertation/config/xslt/extendedDescriptors.xml");

		assertNotNull(model.getDescriptorModel("D4"));
		assertEquals(3, model.getDescriptors().size());
	}

	/**
	 * Tests the created {@code MetaDataModel}.
	 */
	@Test
	public void testFullModelCreation() {

		// get the model
		final MetaDataModel m = getModel(pathToFM);
		assertNotNull(m);
		assertEquals("myModel", m.getId());
		assertEquals("My wonderful Model", m.getName());

		// check the resources and descriptors
		Collection<Descriptor<?, ?, ?>> des;
		des = m.getDescriptors();
		assertEquals(21, des.size());

		des = m.getDescriptorsByClass(Descriptor.class);
		assertEquals(21, des.size());
		des = m.getDescriptorsByClass(Object.class);
		assertEquals(21, des.size());
		des = m.getDescriptorsByClass(ResourceDescriptor.class);
		assertEquals(7, des.size());

		// check all the created identifiers of the resources
		final Set<UUID> uuids = new HashSet<UUID>();
		for (final Descriptor<?, ?, ?> r : des) {
			assertTrue(r.getId().getClass().getName(),
					r.getId() instanceof UUID);
			assertTrue(uuids.add((UUID) r.getId()));
		}

		// check all the created identifiers of the descriptors
		des = m.getDescriptorsByClass(ListDescriptor.class);
		assertEquals(3, des.size());
		final Set<String> expectedValues = new HashSet<String>(Arrays.asList(
				"A", "B", "C", "D", "E", "F", "G", "H", "I"));
		final Set<Long> longids = new HashSet<Long>();
		for (final Descriptor<?, ?, ?> d : des) {
			assertTrue(d.getId().getClass().getName(),
					d.getId() instanceof Long);
			assertTrue(longids.add((Long) d.getId()));

			@SuppressWarnings("unchecked")
			final ListDescriptor<Long> myOwn = (ListDescriptor<Long>) d;
			assertEquals(d.getModelName(), "D4");

			// check the values
			final List<String> values = myOwn.getValueList();
			assertTrue(expectedValues.removeAll(values));
		}

		// check if all the expected values were retrieved
		assertEquals(0, expectedValues.size());
	}

	/**
	 * Tests if the full model configuration with external data-sources can be
	 * read.
	 * 
	 * @throws IOException
	 *             if the file cannot be read
	 */
	@Test
	public void testFullModelDataFromExternalCreation() throws IOException {

		// we need a running database now
		getDb("/net/meisen/dissertation/data/impl/hsqldbs/tidaTestData.zip");

		// get the model
		final MetaDataModel m = getModel(pathToFMDFE);
		assertNotNull(m);
		assertEquals("modelWithExternalSources", m.getId());
		assertEquals("modelWithExternalSources", m.getName());

		// check the descriptors
		assertEquals(
				4,
				m.getDescriptorsByClass(GeneralDescriptor.class,
						IntegerDescriptor.class, LongDescriptor.class,
						DoubleDescriptor.class).size());
		assertNotNull(m.getDescriptorByValue("D1", "FIXED VALUE"));
		assertNotNull(m.getDescriptorByValue("D2", 2));
		assertNotNull(m.getDescriptorByValue("D3", "Some Value"));
		assertNotNull(m.getDescriptorByValue("D4", 1.0));

		// check the resources
		assertEquals(10002, m.getDescriptorsByClass(ResourceDescriptor.class)
				.size());
		for (int i = 1; i <= 10000; i++) {
			assertNotNull(m.getDescriptorByValue("R2", "" + i));
		}
		assertNotNull(m.getDescriptorByValue("R1", "Edison"));
		assertNotNull(m.getDescriptorByValue("R3", "NoValue"));
	}

	/**
	 * Helper method to match documents against regular expressions lines.
	 * 
	 * @param output
	 *            the document to be matched
	 * @param lines
	 *            the lines (as regular expressions) which must be in the
	 *            sequence within the document
	 * 
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

	/**
	 * Helper method which prints the passed xml to the console.
	 * 
	 * @param xml
	 *            the xml to be printed
	 * @throws TransformationFailedException
	 *             if the file cannot be transformed
	 */
	protected void printTransformation(final String xml)
			throws TransformationFailedException {
		transformer.transformFromClasspath(xml, outputStream);
		System.out.println(getOutput());
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
	 * CleansUp by releasing the {@code ModulesHolder} and shutting down the
	 * {@code Db}.
	 */
	@After
	public void cleanUp() {
		if (modulesHolder != null) {
			modulesHolder.release();
		}
		if (db != null) {
			db.shutDownDb();
		}
	}
}
