package net.meisen.dissertation.config;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.config.xslt.mock.MockIndexedCollectionFactory;
import net.meisen.dissertation.config.xslt.mock.MockMapperFactory;
import net.meisen.dissertation.config.xslt.mock.MockQueryFactory;
import net.meisen.dissertation.config.xslt.mock.MockTimeGranularityFactory;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.indexes.IndexedCollectionFactory;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.impl.time.granularity.TimeGranularityFactory;
import net.meisen.dissertation.impl.time.mapper.MapperFactory;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Class to show the path to the configuration, also adds a {@code Suite} which
 * is used to test different configurations for the {@code TidaServer}.
 * 
 * @author pmeisen
 * 
 */
public class TestConfig {

	/**
	 * Base implementation of a test to be tested by this {@code TestConfig}.
	 * 
	 * @author pmeisen
	 * 
	 */
	@ContextClass(TestConfig.class)
	@ContextFile("test-sbconfigurator-core.xml")
	public static abstract class BaseTest extends ModuleBasedTest {

		/**
		 * Initializes the simplest model.
		 */
		@Before
		public void init() {
			setModulesHolder("/net/meisen/dissertation/config/simplestModel.xml");
		}

		/**
		 * The test to run.
		 */
		@Test
		public abstract void test();
	}

	/**
	 * Tests the default configuration.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class TestDefaultConfiguration extends BaseTest {

		@Override
		public void test() {
			Object o;

			o = configuration.getModule(DefaultValues.QUERYFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof QueryFactory);
			
			o = modulesHolder.getModule(DefaultValues.GRANULARTYFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(),
					o instanceof TimeGranularityFactory);

			o = modulesHolder.getModule(DefaultValues.INDEXFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(),
					o instanceof IndexedCollectionFactory);

			o = modulesHolder.getModule(DefaultValues.MAPPERFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof MapperFactory);
		}
	}

	/**
	 * Tests the usage of the configuration to change the factories.
	 * 
	 * @author pmeisen
	 * 
	 */
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/config/tidaConfigSimpleLoadingTest.xml")
	public static class TestConfigurationOfFactories extends BaseTest {

		@Override
		public void test() {
			Object o;

			o = configuration.getModule(DefaultValues.QUERYFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof MockQueryFactory);
			
			o = modulesHolder.getModule(DefaultValues.GRANULARTYFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(),
					o instanceof MockTimeGranularityFactory);

			o = modulesHolder.getModule(DefaultValues.INDEXFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(),
					o instanceof MockIndexedCollectionFactory);

			o = modulesHolder.getModule(DefaultValues.MAPPERFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof MockMapperFactory);
		}
	}

	/**
	 * Suite which combines all the classes.
	 * 
	 * @author pmeisen
	 * 
	 */
	@RunWith(Suite.class)
	@Suite.SuiteClasses({ TestDefaultConfiguration.class,
			TestConfigurationOfFactories.class })
	public static class TestConfigSuite {
		// just the suite with all the tests defined here
	}
}
