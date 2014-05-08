package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.model.cache.IBitmapCacheConfig;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests the implementation of a {@code FileCache}.
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestFileCache extends ModuleBasedTest {

	private FileCache fc;

	/**
	 * Create a test instance of a {@code FileCache}.
	 */
	@Before
	public void create() {
		setModulesHolder("/net/meisen/dissertation/impl/cache/fileCacheModel.xml");

		// create the instance of the cache
		fc = modulesHolder.getModule(DefaultValues.CACHE_ID);
		assertNotNull(fc);
	}

	/**
	 * Tests the exception to be thrown if the configuration is changed after
	 * initialization.
	 */
	@Test
	public void testConfigurationChangeException() {
		thrown.expect(FileCacheException.class);
		thrown.expectMessage("configuration cannot be changed");

		// initialize and change the configuration
		fc.initialize();
		fc.setConfig(null);
	}

	/**
	 * Tests the exception to be thrown if an invalid configuration is used.
	 */
	@Test
	public void testInvalidConfigurationException() {

		thrown.expect(FileCacheException.class);
		thrown.expectMessage("cache does not support a configuration of type");
		
		// initialize and change the configuration
		fc.setConfig(new IBitmapCacheConfig() {
		});
	}

	/**
	 * CleanUp afterwards
	 */
	@After
	public void cleanUp() {

	}
}
