package net.meisen.dissertation.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
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
import net.meisen.dissertation.impl.cache.FileBitmapCache;
import net.meisen.dissertation.impl.cache.FileFactDescriptorModelSetCache;
import net.meisen.dissertation.impl.cache.FileIdentifierCache;
import net.meisen.dissertation.impl.cache.FileMetaDataCache;
import net.meisen.dissertation.impl.cache.IdsOnlyDataRecordCache;
import net.meisen.dissertation.impl.cache.MemoryBitmapCache;
import net.meisen.dissertation.impl.cache.MemoryFactDescriptorModelSetCache;
import net.meisen.dissertation.impl.cache.MemoryIdentifierCache;
import net.meisen.dissertation.impl.cache.MemoryMetaDataCache;
import net.meisen.dissertation.impl.dataintegration.ScriptPreProcessor;
import net.meisen.dissertation.impl.indexes.IndexFactory;
import net.meisen.dissertation.impl.parser.query.QueryFactory;
import net.meisen.dissertation.impl.time.granularity.TimeGranularityFactory;
import net.meisen.dissertation.impl.time.mapper.MapperFactory;
import net.meisen.dissertation.model.cache.IBitmapIdCache;
import net.meisen.dissertation.model.cache.IIdentifierCache;
import net.meisen.dissertation.model.cache.IMetaDataCache;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
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

			o = modulesHolder.getModule(DefaultValues.IDENTIFIERCACHE_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(),
					o instanceof MemoryIdentifierCache);

			o = modulesHolder.getModule(DefaultValues.BITMAPCACHE_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof MemoryBitmapCache);

			o = modulesHolder.getModule(DefaultValues.METADATACACHE_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof MemoryMetaDataCache);

			o = modulesHolder.getModule(DefaultValues.FACTSETSCACHE_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(),
					o instanceof MemoryFactDescriptorModelSetCache);

			o = modulesHolder.getModule(DefaultValues.DATARECORDCACHE_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(),
					o instanceof IdsOnlyDataRecordCache);

			o = modulesHolder.getModule(DefaultValues.PREPROCESSOR_ID);
			assertNull(o);

			// check folder configuration
			o = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
			assertNotNull(o);
			assertTrue(o.getClass().getName(), o instanceof TidaModel);
			final TidaModel model = (TidaModel) o;
			assertEquals(
					new File(DefaultValues.getDefaultLocation(), model.getId()),
					model.getLocation());
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
	 * Tests the loading of a fully configured file.
	 * 
	 * @author pmeisen
	 * 
	 */
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/config/fullConfig.xml")
	public static class TestFullConfig extends BaseTest {

		@Override
		public void test() {
			setModulesHolder("/net/meisen/dissertation/config/simplestModel.xml");
		}
	}

	/**
	 * Tests the loading of a fully configured model.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class TestFullModel extends BaseTest {

		@Override
		public void test() {
			setModulesHolder("/net/meisen/dissertation/config/fullModel.xml");

			// get the model and validate it
			final TidaModel model = modulesHolder
					.getModule(DefaultValues.TIDAMODEL_ID);
			assertNotNull(model);
			assertEquals("fullModel", model.getId());

			// check the pre-processor
			assertNotNull(model.getPreProcessor());
			assertEquals(ScriptPreProcessor.class, model.getPreProcessor()
					.getClass());

			// initialize the model
			model.initialize();

			// check the dimensions of the model
			assertEquals(2, model.getDimensionModel().getDimensions().size());
		}
	}

	/**
	 * Tests the loading of a fully configured configuration and model.
	 * 
	 * @author pmeisen
	 * 
	 */
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/config/fullConfig.xml")
	public static class TestFullConfigWithFullModel extends BaseTest {

		@Override
		public void test() {
			setModulesHolder("/net/meisen/dissertation/config/fullModel.xml");
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
	 * Tests the usage of a {@code MemoryBitmapCache} and
	 * {@code MemoryMetaDataCache} defined via the global configuration.
	 * 
	 * @author pmeisen
	 */
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/config/tidaConfigCacheMemory.xml")
	public static class TestConfigMemoryCache extends BaseTest {

		@Override
		public void test() {
			final IIdentifierCache idCache = modulesHolder
					.getModule(DefaultValues.IDENTIFIERCACHE_ID);
			assertNotNull(idCache);
			assertEquals(MemoryIdentifierCache.class, idCache.getClass());

			final IBitmapIdCache<Bitmap> bmpCache = modulesHolder
					.getModule(DefaultValues.BITMAPCACHE_ID);
			assertNotNull(bmpCache);
			assertEquals(MemoryBitmapCache.class, bmpCache.getClass());

			final IMetaDataCache mdCache = modulesHolder
					.getModule(DefaultValues.METADATACACHE_ID);
			assertNotNull(mdCache);
			assertEquals(MemoryMetaDataCache.class, mdCache.getClass());

			final IBitmapIdCache<FactDescriptorModelSet> fdmsCache = modulesHolder
					.getModule(DefaultValues.FACTSETSCACHE_ID);
			assertNotNull(fdmsCache);
			assertEquals(MemoryFactDescriptorModelSetCache.class,
					fdmsCache.getClass());
		}
	}

	/**
	 * Tests the usage of a {@code FileBitmapCache} defined via the global
	 * configuration.
	 * 
	 * @author pmeisen
	 */
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/config/tidaConfigCacheFile.xml")
	public static class TestConfigFileCache extends BaseTest {

		@Override
		public void test() {
			final IIdentifierCache idCache = modulesHolder
					.getModule(DefaultValues.IDENTIFIERCACHE_ID);
			assertNotNull(idCache);
			assertEquals(FileIdentifierCache.class, idCache.getClass());
			assertEquals(new File(System.getProperty("java.io.tmpdir"),
					"config-identifier"),
					((FileIdentifierCache) idCache).getLocation());

			final IBitmapIdCache<Bitmap> bmpCache = modulesHolder
					.getModule(DefaultValues.BITMAPCACHE_ID);
			assertNotNull(bmpCache);
			assertEquals(FileBitmapCache.class, bmpCache.getClass());
			assertEquals(new File(System.getProperty("java.io.tmpdir")),
					((FileBitmapCache) bmpCache).getLocation());

			final IMetaDataCache mdCache = modulesHolder
					.getModule(DefaultValues.METADATACACHE_ID);
			assertNotNull(mdCache);
			assertEquals(FileMetaDataCache.class, mdCache.getClass());
			assertEquals(new File(System.getProperty("java.io.tmpdir"),
					"config-metadata"),
					((FileMetaDataCache) mdCache).getLocation());

			final IBitmapIdCache<FactDescriptorModelSet> fdmsCache = modulesHolder
					.getModule(DefaultValues.FACTSETSCACHE_ID);
			assertNotNull(fdmsCache);
			assertEquals(FileFactDescriptorModelSetCache.class,
					fdmsCache.getClass());
			assertEquals(new File(System.getProperty("java.io.tmpdir"),
					"config-facts"),
					((FileFactDescriptorModelSetCache) fdmsCache).getLocation());
			assertEquals(1000 * 1024 * 1024,
					((FileFactDescriptorModelSetCache) fdmsCache)
							.getMaxFileSizeInByte());
		}
	}

	/**
	 * Tests the usage of a {@code MemoryBitmapCache} and
	 * {@code MemoryMetaDataCache} defined specifically for the model.
	 * 
	 * @author pmeisen
	 */
	public static class TestModuleMemoryCache extends BaseTest {

		@Override
		public void test() {
			setModulesHolder("/net/meisen/dissertation/config/tidaModelCacheMemory.xml");

			final IIdentifierCache idCache = modulesHolder
					.getModule(DefaultValues.IDENTIFIERCACHE_ID);
			assertNotNull(idCache);
			assertEquals(MemoryIdentifierCache.class, idCache.getClass());

			final IBitmapIdCache<Bitmap> bmpCache = modulesHolder
					.getModule(DefaultValues.BITMAPCACHE_ID);
			assertNotNull(bmpCache);
			assertEquals(MemoryBitmapCache.class, bmpCache.getClass());

			final IMetaDataCache mdCache = modulesHolder
					.getModule(DefaultValues.METADATACACHE_ID);
			assertNotNull(mdCache);
			assertEquals(MemoryMetaDataCache.class, mdCache.getClass());

			final IBitmapIdCache<FactDescriptorModelSet> fdmsCache = modulesHolder
					.getModule(DefaultValues.FACTSETSCACHE_ID);
			assertNotNull(fdmsCache);
			assertEquals(MemoryFactDescriptorModelSetCache.class,
					fdmsCache.getClass());
		}
	}

	/**
	 * Tests the usage of a {@code FileBitmapCache} defined specifically for the
	 * model.
	 * 
	 * @author pmeisen
	 */
	public static class TestModuleFileCache extends BaseTest {

		@Override
		public void test() {
			setModulesHolder("/net/meisen/dissertation/config/tidaModelCacheFile.xml");

			final IIdentifierCache idCache = modulesHolder
					.getModule(DefaultValues.IDENTIFIERCACHE_ID);
			assertNotNull(idCache);
			assertEquals(FileIdentifierCache.class, idCache.getClass());
			assertEquals(new File(System.getProperty("java.io.tmpdir"),
					"model-identifier"),
					((FileIdentifierCache) idCache).getLocation());

			final IBitmapIdCache<Bitmap> bmpCache = modulesHolder
					.getModule(DefaultValues.BITMAPCACHE_ID);
			assertNotNull(bmpCache);
			assertEquals(FileBitmapCache.class, bmpCache.getClass());
			assertEquals(new File(System.getProperty("java.io.tmpdir"),
					"model-bitmap"), ((FileBitmapCache) bmpCache).getLocation());

			final IMetaDataCache mdCache = modulesHolder
					.getModule(DefaultValues.METADATACACHE_ID);
			assertNotNull(mdCache);
			assertEquals(FileMetaDataCache.class, mdCache.getClass());
			assertEquals(new File(System.getProperty("java.io.tmpdir"),
					"model-metadata"),
					((FileMetaDataCache) mdCache).getLocation());

			final IBitmapIdCache<FactDescriptorModelSet> fdmsCache = modulesHolder
					.getModule(DefaultValues.FACTSETSCACHE_ID);
			assertNotNull(fdmsCache);
			assertEquals(FileFactDescriptorModelSetCache.class,
					fdmsCache.getClass());
			assertEquals(new File(System.getProperty("java.io.tmpdir"),
					"model-facts"),
					((FileFactDescriptorModelSetCache) fdmsCache).getLocation());
			assertEquals(10000,
					((FileFactDescriptorModelSetCache) fdmsCache)
							.getMaxCacheSize());
		}
	}

	/**
	 * Suite which combines all the classes.
	 * 
	 * @author pmeisen
	 * 
	 */
	@RunWith(Suite.class)
	@Suite.SuiteClasses({ TestDefaultConfiguration.class, TestFullConfig.class,
			TestFullModel.class, TestFullConfigWithFullModel.class,
			TestCustomizedConfiguration.class, TestModelSpecificLocation.class,
			TestConfigMemoryCache.class, TestConfigFileCache.class,
			TestModuleMemoryCache.class, TestModuleFileCache.class })
	public static class TestConfigSuite {
		// just the suite with all the tests defined here
	}
}
