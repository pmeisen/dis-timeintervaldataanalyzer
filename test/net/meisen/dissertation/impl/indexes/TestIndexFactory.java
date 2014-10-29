package net.meisen.dissertation.impl.indexes;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.indexes.datarecord.slices.RoaringBitmap;
import net.meisen.dissertation.impl.indexes.mock.BitmapMock;
import net.meisen.dissertation.impl.indexes.mock.IndexedCollectionMock;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.datarecord.slices.Bitmap;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Tests the implementation of {@code IndexFactory}.
 * 
 * @author pmeisen
 * 
 */
public class TestIndexFactory {

	/**
	 * Tests the general functionality offered by the {@code IndexFactory}.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class TestGenerallyIndexFactory {

		/**
		 * Tests the creation of an index based on a single key-type.
		 */
		@Test
		public void testFromTypeCreation() {
			IndexKeyDefinition keyDef;
			final IndexFactory factory = new IndexFactory();

			// Test a byte key
			keyDef = new IndexKeyDefinition(byte.class);
			assertEquals(TroveByteIndexedCollection.class,
					factory.create(keyDef).getClass());
			keyDef = new IndexKeyDefinition(Byte.class);
			assertEquals(TroveByteIndexedCollection.class,
					factory.create(keyDef).getClass());

			// Test a short key
			keyDef = new IndexKeyDefinition(short.class);
			assertEquals(TroveShortIndexedCollection.class,
					factory.create(keyDef).getClass());
			keyDef = new IndexKeyDefinition(Short.class);
			assertEquals(TroveShortIndexedCollection.class,
					factory.create(keyDef).getClass());

			// Test an integer key
			keyDef = new IndexKeyDefinition(int.class);
			assertEquals(TroveIntIndexedCollection.class, factory
					.create(keyDef).getClass());
			keyDef = new IndexKeyDefinition(Integer.class);
			assertEquals(TroveIntIndexedCollection.class, factory
					.create(keyDef).getClass());

			// Test a long key
			keyDef = new IndexKeyDefinition(long.class);
			assertEquals(TroveLongIndexedCollection.class,
					factory.create(keyDef).getClass());
			keyDef = new IndexKeyDefinition(Long.class);
			assertEquals(TroveLongIndexedCollection.class,
					factory.create(keyDef).getClass());

			// Test a UUID key
			keyDef = new IndexKeyDefinition(UUID.class);
			assertEquals(MapIndexedCollection.class, factory.create(keyDef)
					.getClass());
		}

		/**
		 * Tests the creation of an empty bitmap.
		 */
		@Test
		public void testBitmapCreation() {
			IndexFactory factory;
			IndexFactoryConfig config;
			Bitmap bitmap;

			// create a simple bitmap using the default configuration and test
			// it
			factory = new IndexFactory();
			bitmap = factory.createBitmap();
			assertNotNull(bitmap);
			assertEquals(RoaringBitmap.class, bitmap.getClass());

			// create a user-defined bitmap
			config = new IndexFactoryConfig();
			config.setBitmapClass(BitmapMock.class);
			factory = new IndexFactory();
			factory.setConfig(config);
			bitmap = factory.createBitmap();
			assertNotNull(bitmap);
			assertEquals(BitmapMock.class, bitmap.getClass());
		}
	}

	/**
	 * Tests the usage of a default configuration considering the used
	 * {@code IndexFactory}.
	 * 
	 * @author pmeisen
	 * 
	 */
	@ContextClass(TestConfig.class)
	@ContextFile("test-sbconfigurator-core.xml")
	public static class TestDefaultIndexConfig extends ModuleBasedTest {

		/**
		 * Checks the default usage.
		 */
		@Test
		public void test() {
			setModulesHolder("/net/meisen/dissertation/config/simplestModel.xml");
			final IndexFactory factory = modulesHolder
					.getModule(DefaultValues.INDEXFACTORY_ID);
			assertNotNull(factory);

			// get the configuration
			final IndexFactoryConfig config = factory.getConfiguration();
			assertEquals(RoaringBitmap.class, config.getBitmapClass());
			assertEquals(TroveByteIndexedCollection.class,
					config.getByteClass());
			assertEquals(TroveShortIndexedCollection.class,
					config.getShortClass());
			assertEquals(TroveIntIndexedCollection.class, config.getIntClass());
			assertEquals(TroveLongIndexedCollection.class,
					config.getLongClass());
		}
	}

	/**
	 * Tests the usage of a globally changed configuration considering the used
	 * {@code IndexFactory}.
	 * 
	 * @author pmeisen
	 * 
	 */
	@ContextClass(TestConfig.class)
	@ContextFile("test-sbconfigurator-core.xml")
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/impl/indexes/config/globallyChangedIndexConfig.xml")
	public static class TestGloballyChangedIndexConfig extends ModuleBasedTest {

		/**
		 * Tests the application of the globally changed configuration.
		 */
		@Test
		public void test() {
			setModulesHolder("/net/meisen/dissertation/config/simplestModel.xml");
			final IndexFactory factory = modulesHolder
					.getModule(DefaultValues.INDEXFACTORY_ID);
			assertNotNull(factory);

			// get the configuration
			final IndexFactoryConfig config = factory.getConfiguration();
			assertEquals(BitmapMock.class, config.getBitmapClass());
			assertEquals(TroveByteIndexedCollection.class,
					config.getByteClass());
			assertEquals(TroveShortIndexedCollection.class,
					config.getShortClass());
			assertEquals(IndexedCollectionMock.class, config.getIntClass());
			assertEquals(IndexedCollectionMock.class, config.getLongClass());
		}
	}

	/**
	 * Tests the usage of a model-based configuration considering the used
	 * {@code IndexFactory}.
	 * 
	 * @author pmeisen
	 * 
	 */
	@ContextClass(TestConfig.class)
	@ContextFile("test-sbconfigurator-core.xml")
	public static class TestModelBasedChangedIndexConfig extends
			ModuleBasedTest {

		/**
		 * Tests the application of the changed model-based configuration.
		 */
		@Test
		public void test() {
			setModulesHolder("/net/meisen/dissertation/impl/indexes/config/modelBasedChangedIndexConfig.xml");
			final IndexFactory factory = modulesHolder
					.getModule(DefaultValues.INDEXFACTORY_ID);
			assertNotNull(factory);

			// get the configuration
			final IndexFactoryConfig config = factory.getConfiguration();
			assertEquals(BitmapMock.class, config.getBitmapClass());
			assertEquals(IndexedCollectionMock.class, config.getByteClass());
			assertEquals(IndexedCollectionMock.class, config.getShortClass());
			assertEquals(TroveIntIndexedCollection.class, config.getIntClass());
			assertEquals(TroveLongIndexedCollection.class,
					config.getLongClass());
		}
	}

	/**
	 * Suite which combines all the classes.
	 * 
	 * @author pmeisen
	 * 
	 */
	@RunWith(Suite.class)
	@Suite.SuiteClasses({ TestGenerallyIndexFactory.class,
			TestDefaultIndexConfig.class, TestGloballyChangedIndexConfig.class,
			TestModelBasedChangedIndexConfig.class })
	public static class TestIndexFactorySuite {
		// just the suite with all the tests defined here
	}
}
