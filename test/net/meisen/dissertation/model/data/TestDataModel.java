package net.meisen.dissertation.model.data;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.IOException;
import java.io.InputStream;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.help.Db;
import net.meisen.dissertation.impl.dataretriever.DbDataRetriever;
import net.meisen.dissertation.model.datasets.IClosableIterator;
import net.meisen.dissertation.model.datasets.IDataRecord;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.api.IModuleHolder;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of a {@code DataModel}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "?")
public class TestDataModel {

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private IConfiguration configuration;

	/**
	 * Helper method to load the {@code DataModel} specified by the passed
	 * {@code xml}.
	 * 
	 * @param xml
	 *            the {@code xml} which specifies the {@code DataModel} to be
	 *            loaded
	 * 
	 * @return the loaded {@code DataModel}
	 */
	protected IModuleHolder getModelHolder(final String xml) {
		final InputStream is = getClass().getResourceAsStream(xml);
		return configuration.loadDelayed("testloader", is);
	}

	/**
	 * Tests an empty, i.e. no {@code DataSets} defined, {@code DataModel}.
	 */
	@Test
	public void testEmptyDataModel() {
		final IModuleHolder moduleHolder = getModelHolder("/net/meisen/dissertation/model/data/testEmptyDataModel.xml");
		final DataModel model = moduleHolder.getModule("dataModelId");
		assertNotNull(model);

		// check the retrievers
		assertEquals(0, model.sizeOfRetrievers());

		// test some names
		assertFalse(model.hasNamedValue(null));
		assertFalse(model.hasNamedValue(""));
		assertFalse(model.hasNamedValue("TESTNAME"));

		// test the positions
		for (int i = -100; i < 100; i++) {
			assertFalse(model.isValidPosition(i));
		}

		// test the iteration
		final IClosableIterator<IDataRecord> it = model.iterator();
		int counter = 0;
		while (it.hasNext()) {
			counter++;
		}
		assertEquals(0, counter);

		// cleanUp
		it.close();
		moduleHolder.release();
	}

	/**
	 * Tests the retrieval of data from a single {@code DbDataRetriever}.
	 * 
	 * @throws IOException
	 *             if the database cannot be loaded
	 * 
	 * @see DbDataRetriever
	 */
	@Test
	public void testSingleDbRetrieval() throws IOException {
		final Db db = new Db();

		// start the test database
		db.addDb("tidaTestData",
				"/net/meisen/dissertation/impl/hsqldbs/tidaTestData.zip");
		db.setUpDb();

		// get the Model
		final IModuleHolder moduleHolder = getModelHolder("/net/meisen/dissertation/model/data/testDbDataModel.xml");
		final DataModel model = moduleHolder.getModule("dataModelId");
		assertNotNull(model);
		
		// check the retrievers
		assertEquals(0, model.sizeOfRetrievers());

		// test some names
		assertFalse(model.hasNamedValue(null));
		assertFalse(model.hasNamedValue(""));
		assertFalse(model.hasNamedValue("TESTNAME"));
		assertTrue(model.hasNamedValue("FIXED"));
		assertTrue(model.hasNamedValue("RANDOM"));
		assertTrue(model.hasNamedValue("COUNTER"));

		// test the positions
		for (int i = -100; i < 100; i++) {
			assertEquals("Invalid Position: " + i, i > 0 && i < 4,
					model.isValidPosition(i));
		}

		// check the values
		final IClosableIterator<IDataRecord> it = model.iterator();
		int i = 0;
		while (it.hasNext()) {
			i++;

			final IDataRecord data = it.next();
			assertEquals("FIXED VALUE", data.getValue("FIXED"));
			assertEquals(i, data.getValue("COUNTER"));
			assertNotNull(data.getValue("RANDOM"));
		}
		assertEquals(10000, i);

		// cleanUp
		it.close();
		moduleHolder.release();
		db.shutDownDb();
	}

	/**
	 * Tests a mixed {@code DataSet}, i.e. different positions and names in
	 * different {@code DataSets}.
	 * 
	 * @throws IOException
	 *             if the database cannot be loaded
	 */
	@Test
	public void testMixedRetrieval() throws IOException {
		final Db db = new Db();

		// start the test database
		db.addDb("tidaTestData",
				"/net/meisen/dissertation/impl/hsqldbs/tidaTestData.zip");
		db.setUpDb();

		// get the Model
		final IModuleHolder moduleHolder = getModelHolder("/net/meisen/dissertation/model/data/testMixedDataModel.xml");
		final DataModel model = moduleHolder.getModule("dataModelId");
		assertNotNull(model);
		
		// check the retrievers
		assertEquals(0, model.sizeOfRetrievers());

		// test some names
		assertFalse(model.hasNamedValue(null));
		assertFalse(model.hasNamedValue(""));
		assertFalse(model.hasNamedValue("TESTNAME"));
		assertFalse(model.hasNamedValue("FIXED"));
		assertFalse(model.hasNamedValue("RANDOM"));
		assertFalse(model.hasNamedValue("NVL"));
		assertFalse(model.hasNamedValue("LAST"));
		assertTrue(model.hasNamedValue("COUNTER"));
		assertTrue(model.hasNamedValueOnce("COUNTER"));
		assertTrue(model.hasNamedValueOnce("FIXED"));
		assertTrue(model.hasNamedValueOnce("RANDOM"));
		assertTrue(model.hasNamedValueOnce("NVL"));
		assertTrue(model.hasNamedValueOnce("LAST"));

		// test the positions
		for (int i = -100; i < 100; i++) {
			assertEquals("Invalid Position: " + i, i > 0 && i < 2,
					model.isValidPosition(i));
			assertEquals("Invalid Position: " + i, i > 0 && i < 5,
					model.isValidPositionOnce(i));
		}

		// check the values
		final IClosableIterator<IDataRecord> it = model.iterator();
		int i = 0;
		while (it.hasNext()) {
			i++;

			final IDataRecord data = it.next();
			if (i < 10001) {
				assertTrue(data.hasNamedValue("FIXED"));
				assertTrue(data.hasNamedValue("RANDOM"));
				assertTrue(data.hasNamedValue("COUNTER"));
				assertFalse(data.hasNamedValue("NVL"));
				assertFalse(data.hasNamedValue("LAST"));

				assertFalse(data.isValidPosition(0));
				assertTrue(data.isValidPosition(1));
				assertTrue(data.isValidPosition(2));
				assertTrue(data.isValidPosition(3));
				assertFalse(data.isValidPosition(4));
			} else if (i < 20001) {
				assertTrue(data.hasNamedValue("FIXED"));
				assertFalse(data.hasNamedValue("RANDOM"));
				assertTrue(data.hasNamedValue("COUNTER"));
				assertTrue(data.hasNamedValue("NVL"));
				assertTrue(data.hasNamedValue("LAST"));

				assertFalse(data.isValidPosition(0));
				assertTrue(data.isValidPosition(1));
				assertTrue(data.isValidPosition(2));
				assertTrue(data.isValidPosition(3));
				assertTrue(data.isValidPosition(4));
				assertFalse(data.isValidPosition(5));
			} else if (i == 20001) {
				assertFalse(data.hasNamedValue("FIXED"));
				assertFalse(data.hasNamedValue("RANDOM"));
				assertTrue(data.hasNamedValue("COUNTER"));
				assertFalse(data.hasNamedValue("NVL"));
				assertFalse(data.hasNamedValue("LAST"));

				assertFalse(data.isValidPosition(0));
				assertTrue(data.isValidPosition(1));
				assertFalse(data.isValidPosition(2));
			} else if (i == 20002) {
				assertTrue(data.hasNamedValue("FIXED"));
				assertFalse(data.hasNamedValue("RANDOM"));
				assertTrue(data.hasNamedValue("COUNTER"));
				assertFalse(data.hasNamedValue("NVL"));
				assertFalse(data.hasNamedValue("LAST"));

				assertFalse(data.isValidPosition(0));
				assertTrue(data.isValidPosition(1));
				assertTrue(data.isValidPosition(2));
				assertTrue(data.isValidPosition(3));
				assertFalse(data.isValidPosition(4));
			} else {
				fail("Invalid count " + i);
			}
		}
		assertEquals(20002, i);

		// cleanUp
		moduleHolder.release();
		db.shutDownDb();
	}
}
