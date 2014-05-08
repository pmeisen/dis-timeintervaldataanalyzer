package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.UUID;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.google.common.io.Files;

import net.meisen.dissertation.exceptions.FileCacheException;
import net.meisen.dissertation.help.ExceptionBasedTest;
import net.meisen.dissertation.model.cache.IBitmapCacheConfig;

/**
 * Tests the implementation of a {@code FileCache}.
 * 
 * @author pmeisen
 * 
 */
public class TestFileCache extends ExceptionBasedTest {

	private FileCache fc;

	@Before
	public void create() {

		// create the location
		final File tmpDir = new File(System.getProperty("java.io.tmpdir"));
		final File tmpSubDir = new File(tmpDir, UUID.randomUUID().toString());
		assertTrue(tmpSubDir.mkdirs());

		// create the instance of the cache
		fc = new FileCache();
		fc.setConfig(new FileCacheConfig(tmpSubDir));
	}

	@Test
	public void testConfigurationChangeException() {

		thrown.expect(FileCacheException.class);
		thrown.expectMessage("configuration cannot be changed");

		// initialize and change the configuration
		fc.initialize();
		fc.setConfig(null);
	}

	@Test
	public void testInvalidConfigurationException() {

		thrown.expect(FileCacheException.class);
		thrown.expectMessage("configuration cannot be changed");

		// initialize and change the configuration
		fc.setConfig(new IBitmapCacheConfig() {
		});
	}

	@After
	public void cleanUp() {

	}
}
