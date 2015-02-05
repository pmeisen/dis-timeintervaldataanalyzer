package net.meisen.dissertation.impl.dataretriever;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.IOException;

import net.meisen.dissertation.help.LoaderBasedTest;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.dataretriever.DataCollection;
import net.meisen.dissertation.model.dataretriever.DataIterator;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.general.genmisc.FileManager;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;
import net.meisen.general.genmisc.types.Streams;
import net.meisen.general.sbconfigurator.api.IConfiguration;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the {@code CsvDataRetriever} implementation.
 * 
 * @author pmeisen
 * 
 */
public class TestCsvDataRetriever extends LoaderBasedTest {

	/**
	 * The used {@code ExceptionRegistry}.
	 */
	@Autowired(required = false)
	@Qualifier(IConfiguration.coreExceptionRegistryId)
	protected IExceptionRegistry exceptionRegistry;

	private FileManager manager;

	/**
	 * Initializes the {@code FileManager}.
	 */
	@Before
	public void init() {
		manager = new FileManager();
	}

	/**
	 * Cleans the {@code FileManager}.
	 */
	@After
	public void cleanup() {
		manager.cleanUp();
	}

	/**
	 * Tests the exception to be thrown if the location is invalid.
	 */
	@Test
	public void testLoadFromModelWithInvalidLocation() {
		thrown.expect(CsvDataRetrieverException.class);
		thrown.expectMessage("could not be found");

		m("/net/meisen/dissertation/impl/dataretriever/csvDataModelWithInvalidLocation.xml",
				false);
	}

	/**
	 * Tests the exception to be thrown if the selector is invalid.
	 */
	@Test
	public void testLoadFromModelWithInvalidSelector() {
		thrown.expect(CsvDataRetrieverException.class);
		thrown.expectMessage("");

		m("/net/meisen/dissertation/impl/dataretriever/csvDataModelWithInvalidSelector.xml",
				false);
	}

	/**
	 * Tests the usage of a {@code CsvDataRetriever} within a {@code TidaModel}.
	 */
	@Test
	public void testLoadFromModel() {
		final TidaModel model = m(
				"/net/meisen/dissertation/impl/dataretriever/csvDataModel.xml",
				false);

		// check the created descriptors
		DescriptorModel<?> descModel;
		descModel = model.getMetaDataModel().getDescriptorModel("CALLER");
		assertEquals(77, descModel.sizeAll());
		descModel = model.getMetaDataModel().getDescriptorModel("ORIGIN");
		assertEquals(43, descModel.sizeAll());
		descModel = model.getMetaDataModel().getDescriptorModel("RECIPIENT");
		assertEquals(0, descModel.sizeAll());
		descModel = model.getMetaDataModel().getDescriptorModel("DESTINATION");
		assertEquals(0, descModel.sizeAll());

		// now load the data
		model.bulkLoadDataFromDataModel();

		// validate the result
		descModel = model.getMetaDataModel().getDescriptorModel("CALLER");
		assertEquals(77, descModel.sizeAll());
		descModel = model.getMetaDataModel().getDescriptorModel("ORIGIN");
		assertEquals(43, descModel.sizeAll());
		descModel = model.getMetaDataModel().getDescriptorModel("RECIPIENT");
		assertEquals(981, descModel.sizeAll());
		descModel = model.getMetaDataModel().getDescriptorModel("DESTINATION");
		assertEquals(214, descModel.sizeAll());
	}

	/**
	 * Tests the loading of a csv-file from the classpath.
	 */
	@Test
	public void testLoadClasspathWithoutModel() {
		final CsvDataConfig config = new CsvDataConfig();
		config.setClasspath(true);
		config.setSeparator(";");
		config.setFile("/net/meisen/dissertation/impl/csvdbs/calls.csv");

		final CsvDataRetriever dataRetriever = new CsvDataRetriever(
				"myTestRetriever", config);
		dataRetriever.setExceptionRegistry(exceptionRegistry);
		assertDataRetriever(dataRetriever);

		// cleanUp
		dataRetriever.release();
	}

	/**
	 * Tests the loading of a csv-file from the file-system.
	 * 
	 * @throws IOException
	 *             if an unexpected exception is thrown
	 */
	@Test
	public void testLoadFileWithoutModel() throws IOException {

		final File tmpFile = manager.createFile(System
				.getProperty("java.io.tmpdir"));
		Streams.copyStreamToFile(
				getClass().getResourceAsStream(
						"/net/meisen/dissertation/impl/csvdbs/calls.csv"),
				tmpFile);

		final CsvDataConfig config = new CsvDataConfig();
		config.setClasspath(false);
		config.setSeparator(";");
		config.setFile(tmpFile.getCanonicalPath());

		final CsvDataRetriever dataRetriever = new CsvDataRetriever(
				"myTestRetriever", config);
		dataRetriever.setExceptionRegistry(exceptionRegistry);
		assertDataRetriever(dataRetriever);

		// cleanUp
		dataRetriever.release();
		assertTrue(tmpFile.exists());
	}

	/**
	 * Assert of a {@code DataRetriever} instance.
	 * 
	 * @param dataRetriever
	 *            the retriever to be asserted
	 */
	protected void assertDataRetriever(final CsvDataRetriever dataRetriever) {
		CsvDataSelector selector;
		DataCollection<?> coll;

		// use a column selector
		selector = new CsvDataSelector();
		selector.setColumn("destination");

		coll = dataRetriever.retrieve(selector);
		assertEquals(1, coll.getNames().size());
		assertEquals("destination", coll.getNames().iterator().next());
		coll.release();

		// use a position selector
		selector = new CsvDataSelector();
		selector.setPosition(1);

		coll = dataRetriever.retrieve(selector);
		assertEquals(1, coll.getNames().size());
		assertEquals("caller", coll.getNames().iterator().next());
		coll.release();

		// use no selector at all
		coll = dataRetriever.retrieve(null);
		assertEquals(13, coll.getNames().size());

		// count the values
		final DataIterator<?> it = coll.iterator();
		int i = 0;
		while (it.hasNext()) {
			it.next();
			i++;
		}
		assertEquals(63824, i);

		coll.release();
	}
}
