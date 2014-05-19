package net.meisen.dissertation.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.config.xslt.mock.MockAggregationFunction;
import net.meisen.dissertation.config.xslt.mock.MockIndexFactory;
import net.meisen.dissertation.config.xslt.mock.MockMapperFactory;
import net.meisen.dissertation.config.xslt.mock.MockMinAggregationFunction;
import net.meisen.dissertation.config.xslt.mock.MockQueryFactory;
import net.meisen.dissertation.config.xslt.mock.MockTimeGranularityFactory;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.cache.FileCache;
import net.meisen.dissertation.impl.cache.MemoryCache;
import net.meisen.dissertation.impl.indexes.IndexFactory;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.impl.time.granularity.TimeGranularityFactory;
import net.meisen.dissertation.impl.time.mapper.MapperFactory;
import net.meisen.dissertation.model.cache.IBitmapCache;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.measures.AggregationFunctionHandler;
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

			o = configuration
					.getModule(DefaultValues.AGGREGATIONFUNCTIONHANDLER_ID);
			assertNotNull(o);
			assertTrue(o instanceof AggregationFunctionHandler);
			final AggregationFunctionHandler aggFuncHandler = (AggregationFunctionHandler) o;
			assertEquals(DefaultValues.getAggregationFunctions().size(),
					aggFuncHandler.getFunctions().size());

			o = modulesHolder.getModule(DefaultValues.GRANULARITYFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(),
					o instanceof TimeGranularityFactory);

			o = modulesHolder.getModule(DefaultValues.INDEXFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof IndexFactory);

			o = modulesHolder.getModule(DefaultValues.MAPPERFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof MapperFactory);

			o = modulesHolder.getModule(DefaultValues.CACHE_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof MemoryCache);

			// check folder configuration
			o = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof TidaModel);
			final TidaModel model = (TidaModel) o;
			assertEquals(new File(".", model.getId()), model.getLocation());
		}
	}

	/**
	 * Tests the usage of the configuration to change the factories.
	 * 
	 * @author pmeisen
	 * 
	 */
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/config/tidaConfigSimpleLoadingTest.xml")
	public static class TestCustomizedConfiguration extends BaseTest {

		@Override
		public void test() {
			Object o;

			o = configuration.getModule(DefaultValues.QUERYFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof MockQueryFactory);

			o = configuration
					.getModule(DefaultValues.AGGREGATIONFUNCTIONHANDLER_ID);
			assertNotNull(o);
			assertTrue(o instanceof AggregationFunctionHandler);
			final AggregationFunctionHandler aggFuncHandler = (AggregationFunctionHandler) o;
			assertEquals(DefaultValues.getAggregationFunctions().size() + 1,
					aggFuncHandler.getFunctions().size());
			assertTrue(aggFuncHandler.resolve("min") instanceof MockMinAggregationFunction);
			assertTrue(aggFuncHandler.resolve("mock") instanceof MockAggregationFunction);

			o = modulesHolder.getModule(DefaultValues.GRANULARITYFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(),
					o instanceof MockTimeGranularityFactory);

			o = modulesHolder.getModule(DefaultValues.INDEXFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof MockIndexFactory);

			o = modulesHolder.getModule(DefaultValues.MAPPERFACTORY_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof MockMapperFactory);

			// check folder configuration
			o = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof TidaModel);
			final TidaModel model = (TidaModel) o;
			assertEquals(new File("./testWorkingDirectory", model.getId()),
					model.getLocation());
		}
	}

	/**
	 * Tests the changing of the location for the model.
	 * 
	 * @author pmeisen
	 * 
	 */
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/config/tidaConfigSimpleLoadingTest.xml")
	public static class TestModelSpecificLocation extends BaseTest {

		@Override
		public void test() {
			setModulesHolder("/net/meisen/dissertation/config/tidaModelLocation.xml");

			final Object o = modulesHolder
					.getModule(DefaultValues.TIDAMODEL_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof TidaModel);
			final TidaModel model = (TidaModel) o;
			assertEquals(new File("helloWorld"), model.getLocation());
		}
	}

	/**
	 * Tests the usage of a {@code MemoryCache} defined via the global
	 * configuration.
	 * 
	 * @author pmeisen
	 */
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/config/tidaConfigCacheMemory.xml")
	public static class TestConfigMemoryCache extends BaseTest {

		@Override
		public void test() {
			final IBitmapCache cache = modulesHolder
					.getModule(DefaultValues.CACHE_ID);

			assertNotNull(cache);
			assertEquals(MemoryCache.class, cache.getClass());
		}
	}

	/**
	 * Tests the usage of a {@code FileCache} defined via the global
	 * configuration.
	 * 
	 * @author pmeisen
	 */
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/config/tidaConfigCacheFile.xml")
	public static class TestConfigFileCache extends BaseTest {

		@Override
		public void test() {
			final IBitmapCache cache = modulesHolder
					.getModule(DefaultValues.CACHE_ID);

			assertNotNull(cache);
			assertEquals(FileCache.class, cache.getClass());

			assertEquals(new File(System.getProperty("java.io.tmpdir")),
					((FileCache) cache).getLocation());
		}
	}

	/**
	 * Tests the usage of a {@code MemoryCache} defined specifically for the
	 * model.
	 * 
	 * @author pmeisen
	 */
	public static class TestModuleMemoryCache extends BaseTest {

		@Override
		public void test() {
			setModulesHolder("/net/meisen/dissertation/config/tidaModelCacheMemory.xml");

			final IBitmapCache cache = modulesHolder
					.getModule(DefaultValues.CACHE_ID);

			assertNotNull(cache);
			assertEquals(MemoryCache.class, cache.getClass());
		}
	}

	/**
	 * Tests the usage of a {@code FileCache} defined specifically for the
	 * model.
	 * 
	 * @author pmeisen
	 */
	public static class TestModuleFileCache extends BaseTest {

		@Override
		public void test() {
			setModulesHolder("/net/meisen/dissertation/config/tidaModelCacheFile.xml");

			final IBitmapCache cache = modulesHolder
					.getModule(DefaultValues.CACHE_ID);

			assertNotNull(cache);
			assertEquals(FileCache.class, cache.getClass());

			assertEquals(null, ((FileCache) cache).getLocation());
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
			TestCustomizedConfiguration.class, TestModelSpecificLocation.class,
			TestConfigMemoryCache.class, TestConfigFileCache.class,
			TestModuleMemoryCache.class, TestModuleFileCache.class })
	public static class TestConfigSuite {
		// just the suite with all the tests defined here
	}
}
