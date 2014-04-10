package net.meisen.dissertation.impl.time.mapper;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.time.mapper.mock.MockMapper1;
import net.meisen.dissertation.impl.time.mapper.mock.MockMapper2;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Tests the implementation of a {@code MapperFactory}.
 * 
 * @author pmeisen
 * 
 */
public class TestMapperFactory {

	/**
	 * Tests the usage of a default configuration considering the used
	 * {@code MapperFactory}.
	 * 
	 * @author pmeisen
	 * 
	 */
	@ContextClass(TestConfig.class)
	@ContextFile("test-sbconfigurator-core.xml")
	public static class TestDefaultMapperConfig extends ModuleBasedTest {

		/**
		 * Checks the default usage.
		 */
		@Test
		public void test() {
			setModulesHolder("/net/meisen/dissertation/config/simplestModel.xml");
			final MapperFactory factory = modulesHolder
					.getModule(DefaultValues.MAPPERFACTORY_ID);
			assertNotNull(factory);

			// get the configuration
			final MapperFactoryConfig config = factory.getConfiguration();
			assertEquals(2, config.getMapper().size());
			assertTrue(config.getMapper().contains(DateMapper.class));
			assertTrue(config.getMapper().contains(LongMapper.class));
		}
	}

	/**
	 * Tests the usage of a globally changed configuration considering the used
	 * {@code MapperFactory}.
	 * 
	 * @author pmeisen
	 * 
	 */
	@ContextClass(TestConfig.class)
	@ContextFile("test-sbconfigurator-core.xml")
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/impl/time/mapper/config/globallyChangedMapperConfig.xml")
	public static class TestGloballyChangedMapperConfig extends ModuleBasedTest {

		/**
		 * Tests the application of the globally changed configuration.
		 */
		@Test
		public void test() {
			setModulesHolder("/net/meisen/dissertation/config/simplestModel.xml");
			final MapperFactory factory = modulesHolder
					.getModule(DefaultValues.MAPPERFACTORY_ID);
			assertNotNull(factory);

			// get the configuration
			final MapperFactoryConfig config = factory.getConfiguration();
			assertEquals(3, config.getMapper().size());
			assertTrue(config.getMapper().contains(DateMapper.class));
			assertTrue(config.getMapper().contains(LongMapper.class));
			assertTrue(config.getMapper().contains(MockMapper1.class));
		}
	}

	/**
	 * Tests the usage of a model-based configuration considering the used
	 * {@code MapperFactory}.
	 * 
	 * @author pmeisen
	 * 
	 */
	@ContextClass(TestConfig.class)
	@ContextFile("test-sbconfigurator-core.xml")
	public static class TestModelBasedChangedMapperConfig extends
			ModuleBasedTest {

		/**
		 * Tests the application of the changed model-based configuration.
		 */
		@Test
		public void test() {
			setModulesHolder("/net/meisen/dissertation/impl/time/mapper/config/modelBasedChangedMapperConfig.xml");
			final MapperFactory factory = modulesHolder
					.getModule(DefaultValues.MAPPERFACTORY_ID);
			assertNotNull(factory);

			// get the configuration
			final MapperFactoryConfig config = factory.getConfiguration();
			assertEquals(3, config.getMapper().size());
			assertTrue(config.getMapper().contains(DateMapper.class));
			assertTrue(config.getMapper().contains(LongMapper.class));
			assertTrue(config.getMapper().contains(MockMapper2.class));
		}
	}

	/**
	 * Tests a configuration with enabled inheritance.
	 * 
	 * @author pmeisen
	 * 
	 */
	@ContextClass(TestConfig.class)
	@ContextFile("test-sbconfigurator-core.xml")
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/impl/time/mapper/config/globallyChangedMapperConfig.xml")
	public static class TestHybridBasedChangedMapperConfigWithInheritance
			extends ModuleBasedTest {

		/**
		 * Tests the application of the changed model-based configuration.
		 */
		@Test
		public void test() {
			setModulesHolder("/net/meisen/dissertation/impl/time/mapper/config/modelBasedInheritedChangedMapperConfig.xml");
			final MapperFactory factory = modulesHolder
					.getModule(DefaultValues.MAPPERFACTORY_ID);
			assertNotNull(factory);

			// get the configuration
			final MapperFactoryConfig config = factory.getConfiguration();
			assertEquals(4, config.getMapper().size());
			assertTrue(config.getMapper().contains(DateMapper.class));
			assertTrue(config.getMapper().contains(LongMapper.class));
			assertTrue(config.getMapper().contains(MockMapper1.class));
			assertTrue(config.getMapper().contains(MockMapper2.class));
		}
	}

	/**
	 * Tests the configuration with explicitly disabled inheritance.
	 * 
	 * @author pmeisen
	 * 
	 */
	@ContextClass(TestConfig.class)
	@ContextFile("test-sbconfigurator-core.xml")
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/impl/time/mapper/config/globallyChangedMapperConfig.xml")
	public static class TestHybridBasedChangedMapperConfigWithoutInheritance
			extends ModuleBasedTest {

		/**
		 * Tests the application of the changed model-based configuration.
		 */
		@Test
		public void test() {
			setModulesHolder("/net/meisen/dissertation/impl/time/mapper/config/modelBasedDisableInheritanceChangedMapperConfig.xml");
			final MapperFactory factory = modulesHolder
					.getModule(DefaultValues.MAPPERFACTORY_ID);
			assertNotNull(factory);

			// get the configuration
			final MapperFactoryConfig config = factory.getConfiguration();
			assertEquals(3, config.getMapper().size());
			assertTrue(config.getMapper().contains(DateMapper.class));
			assertTrue(config.getMapper().contains(LongMapper.class));
			assertTrue(config.getMapper().contains(MockMapper2.class));
		}
	}

	/**
	 * Suite which combines all the classes.
	 * 
	 * @author pmeisen
	 * 
	 */
	@RunWith(Suite.class)
	@Suite.SuiteClasses({ TestDefaultMapperConfig.class,
			TestGloballyChangedMapperConfig.class,
			TestModelBasedChangedMapperConfig.class,
			TestHybridBasedChangedMapperConfigWithInheritance.class,
			TestHybridBasedChangedMapperConfigWithoutInheritance.class })
	public static class TestMapperFactorySuite {
		// just the suite with all the tests defined here
	}
}
