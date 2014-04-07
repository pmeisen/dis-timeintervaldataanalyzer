package net.meisen.dissertation.impl.indexes;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.UUID;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.impl.indexes.mock.BitmapMock;
import net.meisen.dissertation.impl.indexes.mock.IndexedCollectionMock;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.indexes.IndexKeyDefinition;
import net.meisen.dissertation.model.indexes.datarecord.IntervalIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.EWAHBitmap;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * Tests the implementation of {@code IndexedCollectionFactory}.
 * 
 * @author pmeisen
 * 
 */
public class TestIndexedCollectionFactory {

	public static class TestGenerallyIndexCollectionFactory {

		/**
		 * Tests the creation of an index based on a single key-type.
		 */
		@Test
		public void testFromTypeCreation() {
			IndexKeyDefinition keyDef;
			final IndexedCollectionFactory factory = new IndexedCollectionFactory();

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

		@Test
		public void testBitmapCreation() {
			IndexedCollectionFactory factory;
			IndexedCollectionFactoryConfig config;
			Bitmap bitmap;

			// create a simple bitmap using the default configuration and test
			// it
			factory = new IndexedCollectionFactory();
			bitmap = factory.createBitmap();
			assertNotNull(bitmap);
			assertEquals(EWAHBitmap.class, bitmap.getClass());

			// create a user-defined bitmap
			config = new IndexedCollectionFactoryConfig();
			config.setBitmapClass(BitmapMock.class);
			factory = new IndexedCollectionFactory();
			factory.setConfig(config);
			bitmap = factory.createBitmap();
			assertNotNull(bitmap);
			assertEquals(BitmapMock.class, bitmap.getClass());
		}
	}

	@ContextClass(TestConfig.class)
	@ContextFile("test-sbconfigurator-core.xml")
	public static class TestDefaultIndexConfig extends ModuleBasedTest {

		@Test
		public void test() {
			setModulesHolder("/net/meisen/dissertation/config/simplestModel.xml");
			final IndexedCollectionFactory factory = modulesHolder
					.getModule(DefaultValues.INDEXFACTORY_ID);
			assertNotNull(factory);

			// get the configuration
			final IndexedCollectionFactoryConfig config = factory.getConfig();
			assertEquals(Bitmap.class, config.getBitmapClass());
			assertEquals(TroveByteIndexedCollection.class,
					config.getByteClass());
			assertEquals(TroveShortIndexedCollection.class,
					config.getShortClass());
			assertEquals(TroveIntIndexedCollection.class, config.getIntClass());
			assertEquals(TroveLongIndexedCollection.class,
					config.getLongClass());
		}
	}

	@ContextClass(TestConfig.class)
	@ContextFile("test-sbconfigurator-core.xml")
	@SystemProperty(property = "tida.config.selector", value = "net/meisen/dissertation/impl/indexes/config/globallyChangedIndexConfig.xml")
	public static class TestGloballyChangedIndexConfig extends ModuleBasedTest {

		@Test
		public void test() {
			setModulesHolder("/net/meisen/dissertation/config/simplestModel.xml");
			final IndexedCollectionFactory factory = modulesHolder
					.getModule(DefaultValues.INDEXFACTORY_ID);
			assertNotNull(factory);

			// get the configuration
			final IndexedCollectionFactoryConfig config = factory.getConfig();
			assertEquals(BitmapMock.class, config.getBitmapClass());
			assertEquals(TroveByteIndexedCollection.class,
					config.getByteClass());
			assertEquals(TroveShortIndexedCollection.class,
					config.getShortClass());
			assertEquals(IndexedCollectionMock.class, config.getIntClass());
			assertEquals(IndexedCollectionMock.class, config.getLongClass());
		}
	}

	@ContextClass(TestConfig.class)
	@ContextFile("test-sbconfigurator-core.xml")
	public static class TestModelBasedChangedIndexConfig extends
			ModuleBasedTest {

		@Test
		public void test() {
			setModulesHolder("/net/meisen/dissertation/impl/indexes/config/modelBasedChangedIndexConfig.xml");
			final IndexedCollectionFactory factory = modulesHolder
					.getModule(DefaultValues.INDEXFACTORY_ID);
			assertNotNull(factory);

			// get the configuration
			final IndexedCollectionFactoryConfig config = factory.getConfig();
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
	@Suite.SuiteClasses({ TestGenerallyIndexCollectionFactory.class,
			TestGloballyChangedIndexConfig.class,
			TestModelBasedChangedIndexConfig.class })
	public static class TestIndexedCollectionFactorySuite {
		// just the suite with all the tests defined here
	}
}
