package net.meisen.dissertation.impl.dataretriever;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.Locale;
import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.impl.dataretriever.FixedStructureDataCollection;
import net.meisen.dissertation.impl.dataretriever.FixedStructureDataRetriever;
import net.meisen.dissertation.impl.dataretriever.FixedStructureDataRetrieverConfig;
import net.meisen.dissertation.impl.dataretriever.FixedStructureDataRetrieverConfigEntry;
import net.meisen.dissertation.impl.dataretriever.FixedStructureQueryConfig;
import net.meisen.dissertation.model.dataretriever.DataRecord;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of a {@code FixedStructureDataRetriever}.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@SystemProperty(property = "testBeans.selector", value = "net/meisen/dissertation/config/tida-bean-exceptions.xml")
public class TestFixedStructureDataRetriever {

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private IConfiguration c;

	private Locale oldDefault;

	/**
	 * Helper class for the tests.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class Util {

		/**
		 * Creates a new instance of a {@code FixedStructureDataRetriever}.
		 * 
		 * @param c
		 *            the {@code Configuration} which can be used for wiring
		 * @param config
		 *            the {@code FixedStructureDataRetrieverConfig} of the
		 *            retriever
		 * @return the instance of the {@code FixedStructureDataRetriever}
		 */
		public static FixedStructureDataRetriever create(
				final IConfiguration c,
				final FixedStructureDataRetrieverConfig config) {
			final FixedStructureDataRetriever rnd = new FixedStructureDataRetriever(
					UUID.randomUUID().toString(), config);
			c.wireInstance(rnd);

			return rnd;
		}
	}

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
	 * Tests the retrieval of single, random integers.
	 */
	@Test
	public void testSingleRandomInteger() {
		final FixedStructureDataRetrieverConfig config = new FixedStructureDataRetrieverConfig();
		config.addEntry(new FixedStructureDataRetrieverConfigEntry(
				"RANDOM_INTEGER", Integer.class));
		final FixedStructureDataRetriever rndRetriever = Util.create(c, config);

		FixedStructureDataCollection data;

		// generate no data
		data = rndRetriever.retrieve(new FixedStructureQueryConfig(0));
		assertEquals(0, data.get().size());

		// generate exactly one data-set
		data = rndRetriever.retrieve(new FixedStructureQueryConfig(1));
		assertEquals(1, data.get().size());
		assertTrue(data.get().get(0).getData("RANDOM_INTEGER") instanceof Integer);
	}

	/**
	 * Tests the retrieval of single, fixed integers.
	 */
	@Test
	public void testSingleFixedInteger() {
		final FixedStructureDataRetrieverConfig config = new FixedStructureDataRetrieverConfig();
		config.addEntry(new FixedStructureDataRetrieverConfigEntry(
				"FIXED_INTEGER", Integer.class, 5000));
		final FixedStructureDataRetriever rndRetriever = Util.create(c, config);

		// generate data
		final FixedStructureDataCollection data = rndRetriever
				.retrieve(new FixedStructureQueryConfig(20));
		assertEquals(20, data.get().size());
		for (final DataRecord<String> rec : data.get()) {
			final Object val = rec.getData("FIXED_INTEGER");
			assertTrue(val instanceof Integer);
			assertEquals(5000, val);
		}
	}

	/**
	 * Tests multiple fields within the fixed structure.
	 */
	@Test
	public void testMultiple() {
		final FixedStructureDataRetrieverConfig config = new FixedStructureDataRetrieverConfig();
		config.addEntry(new FixedStructureDataRetrieverConfigEntry(
				"FIXED_INTEGER", Integer.class, 2001));
		config.addEntry(new FixedStructureDataRetrieverConfigEntry(
				"FIXED_STRING", String.class, "Hello World"));
		config.addEntry(new FixedStructureDataRetrieverConfigEntry(
				"RANDOM_STRING", String.class, "", true));
		config.addEntry(new FixedStructureDataRetrieverConfigEntry(
				"RANDOM_DOUBLE", Double.class));
		config.addEntry(new FixedStructureDataRetrieverConfigEntry(
				"FIXED_NULL_BOOLEAN", Boolean.class, null, false));
		final FixedStructureDataRetriever rndRetriever = Util.create(c, config);

		// generate data
		final FixedStructureDataCollection data = rndRetriever
				.retrieve(new FixedStructureQueryConfig(20));
		assertEquals(20, data.get().size());
		for (final DataRecord<String> rec : data.get()) {
			Object val;

			val = rec.getData("FIXED_INTEGER");
			assertTrue(val instanceof Integer);
			assertEquals(2001, val);

			val = rec.getData("FIXED_STRING");
			assertTrue(val instanceof String);
			assertEquals("Hello World", val);

			val = rec.getData("RANDOM_STRING");
			assertTrue(val instanceof String);

			val = rec.getData("RANDOM_DOUBLE");
			assertTrue(val instanceof Double);

			val = rec.getData("FIXED_NULL_BOOLEAN");
			assertNull(val);
		}
	}

	/**
	 * Reset the {@code Locale}
	 */
	@After
	public void cleanUp() {
		Locale.setDefault(oldDefault);
	}
}
