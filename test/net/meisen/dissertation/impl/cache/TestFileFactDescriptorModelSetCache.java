package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.File;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.FactDescriptor;
import net.meisen.dissertation.model.indexes.datarecord.MetaIndex;
import net.meisen.dissertation.model.indexes.datarecord.slices.BitmapId;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorModelSet;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the implementation of the {@code FileFactDescriptorModelSetCache}
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
@RunWith(JUnitConfigurationRunner.class)
public class TestFileFactDescriptorModelSetCache {

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private IConfiguration configuration;

	private File dir;
	private TidaModel model;

	/**
	 * Initializes a directory for testing and a {@code TidaModel}.
	 */
	@Before
	public void init() {
		dir = new File(System.getProperty("java.io.tmpdir"),
				"testFileFactDescriptorModelSetCache");
		assertTrue(Files.deleteDir(dir));
		assertTrue(dir.mkdirs());

		model = Mockito.mock(TidaModel.class);
		Mockito.when(model.getId()).thenReturn("testModel");
		Mockito.when(model.getLocation()).thenReturn(dir);
	}

	/**
	 * Tests the caching of an empty set.
	 */
	@Test
	public void testEmptySet() {
		final FactDescriptorModelSet set = new FactDescriptorModelSet();

		final FileFactDescriptorModelSetCache cache = configuration
				.createInstance(FileFactDescriptorModelSetCache.class);
		cache.initialize(model);

		// try to cache a specific instance
		final BitmapId<Integer> bitmapId = new BitmapId<Integer>(1,
				MetaIndex.class);
		cache.cache(bitmapId, set);

		// compare the retrieved and the cached one
		assertEquals(set, cache.getCacheable(bitmapId));

		// the data should still be available
		cache.clearCache();
		assertEquals(set, cache.getCacheable(bitmapId));

		// release the cache
		cache.release();
	}

	/**
	 * Tests the caching of a set with elements.
	 */
	@Test
	public void testFullSet() {
		final FactDescriptorModelSet set = new FactDescriptorModelSet();
		assertTrue(set.add(new FactDescriptor<Integer>("ModelId", 5)));
		assertTrue(set.add(new FactDescriptor<Integer>("ModelId", 15)));
		assertTrue(set.add(new FactDescriptor<Integer>("ModelId", 25)));
		assertTrue(set.add(new FactDescriptor<Integer>("ModelId", 25, 3.0)));
		assertFalse(set.add(new FactDescriptor<Integer>("ModelId", 25, 3.0)));

		final FileFactDescriptorModelSetCache cache = configuration
				.createInstance(FileFactDescriptorModelSetCache.class);
		cache.initialize(model);

		// try to cache a specific instance
		final BitmapId<Integer> bitmapId = new BitmapId<Integer>(1,
				MetaIndex.class);
		cache.cache(bitmapId, set);

		// compare the retrieved and the cached one
		assertEquals(set, cache.getCacheable(bitmapId));

		// the data should still be available
		cache.clearCache();
		assertEquals(set, cache.getCacheable(bitmapId));

		cache.release();
	}

	/**
	 * CleanUp afterwards
	 */
	@After
	public void cleanUp() {
		model.release(true);
		assertTrue(dir.toString(), Files.deleteDir(dir));
	}
}
