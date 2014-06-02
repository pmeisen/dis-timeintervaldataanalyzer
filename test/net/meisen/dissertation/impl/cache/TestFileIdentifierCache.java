package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.util.Arrays;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.general.genmisc.types.Files;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Tests the implementation of a {@code MemoryIdentifierCache}
 * 
 * @author pmeisen
 * 
 */
@ContextClass(TestConfig.class)
@ContextFile("test-sbconfigurator-core.xml")
public class TestFileIdentifierCache extends ModuleBasedTest {

	private TidaModel model = null;
	private FileIdentifierCache fileIdCache = null;

	/**
	 * Helper to load a {@code FileIdentifierCache}.
	 */
	@Before
	public void loadCache() {
		assertTrue(Files.deleteDir(new File(".", "fileIdentifierCacheModel")));

		// load the model and get the cache
		setModulesHolder("/net/meisen/dissertation/impl/cache/fileIdentifierCacheModel.xml");
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
		fileIdCache = modulesHolder.getModule(DefaultValues.IDENTIFIERCACHE_ID);
		fileIdCache.initialize(model);
	}

	/**
	 * Tests the initialization of the cache
	 */
	@Test
	public void testInit() {
		assertEquals(0, fileIdCache.getValidIdentifiers().getIds().length);
	}

	/**
	 * Tests the marking of valid values.
	 */
	@Test
	public void testValidationOfValues() {
		int[] valid;

		// check the adding of some values
		fileIdCache.markIdentifierAsUsed(10);
		fileIdCache.markIdentifierAsValid(2, 3, 4);

		valid = fileIdCache.getValidIdentifiers().getIds();
		assertEquals(3, valid.length);
		assertEquals(0, Arrays.binarySearch(valid, 2));
		assertEquals(1, Arrays.binarySearch(valid, 3));
		assertEquals(2, Arrays.binarySearch(valid, 4));

		// add some more (also once already validated)
		fileIdCache.markIdentifierAsValid(2, 6, 10);

		valid = fileIdCache.getValidIdentifiers().getIds();
		assertEquals(5, valid.length);
		assertEquals(0, Arrays.binarySearch(valid, 2));
		assertEquals(1, Arrays.binarySearch(valid, 3));
		assertEquals(2, Arrays.binarySearch(valid, 4));
		assertEquals(3, Arrays.binarySearch(valid, 6));
		assertEquals(4, Arrays.binarySearch(valid, 10));
	}

	/**
	 * Tests the marking of invalid values.
	 */
	@Test
	public void testInvalidationOfValues() {
		int[] valid;

		// invalidate some values
		fileIdCache.markIdentifierAsUsed(2040);
		fileIdCache.markIdentifierAsValid(2, 3, 4, 120, 1000, 2040, 204);
		fileIdCache.markIdentifierAsInvalid(2, 5, 100, 204, 2000);

		valid = fileIdCache.getValidIdentifiers().getIds();
		assertEquals(5, valid.length);
		assertEquals(0, Arrays.binarySearch(valid, 3));
		assertEquals(1, Arrays.binarySearch(valid, 4));
		assertEquals(2, Arrays.binarySearch(valid, 120));
		assertEquals(3, Arrays.binarySearch(valid, 1000));
		assertEquals(4, Arrays.binarySearch(valid, 2040));
	}

	/**
	 * Test persistency setting.
	 */
	@Test
	public void testPersistency() {
		int[] valid;

		assertEquals(-1, fileIdCache.getLastUsedIdentifier());
		fileIdCache.setPersistency(true);

		fileIdCache.markIdentifierAsUsed(100);
		assertEquals(100, fileIdCache.getLastUsedIdentifier());
		assertEquals(0, fileIdCache.getValidIdentifiers().getIds().length);

		fileIdCache.markIdentifierAsUsed(200);
		assertEquals(200, fileIdCache.getLastUsedIdentifier());
		assertEquals(0, fileIdCache.getValidIdentifiers().getIds().length);

		fileIdCache.markIdentifierAsValid(100, 104, 105);
		valid = fileIdCache.getValidIdentifiers().getIds();
		assertEquals(200, fileIdCache.getLastUsedIdentifier());
		assertEquals(3, valid.length);
		assertEquals(0, Arrays.binarySearch(valid, 100));
		assertEquals(1, Arrays.binarySearch(valid, 104));
		assertEquals(2, Arrays.binarySearch(valid, 105));

		fileIdCache.setPersistency(false);

		fileIdCache.markIdentifierAsUsed(300);
		fileIdCache.markIdentifierAsInvalid(100);
		valid = fileIdCache.getValidIdentifiers().getIds();
		assertEquals(300, fileIdCache.getLastUsedIdentifier());
		assertEquals(2, valid.length);
		assertEquals(0, Arrays.binarySearch(valid, 104));
		assertEquals(1, Arrays.binarySearch(valid, 105));
	}

	/**
	 * Tests the reloading of an empty cache.
	 */
	@Test
	public void testEmptyReloading() {

		// release the cache
		fileIdCache.release();
		assertNull(fileIdCache.getValidIdentifiers());

		// initialize it again and check if everything is loaded correctly
		fileIdCache.initialize(model);
		assertNotNull(fileIdCache.getValidIdentifiers());
	}

	/**
	 * Tests the reloading of an not empty cache.
	 */
	@Test
	public void testReloading() {
		fileIdCache.markIdentifierAsUsed(100);
		fileIdCache.markIdentifierAsValid(2, 4, 5, 6, 100);

		// release the cache
		fileIdCache.release();
		assertNull(fileIdCache.getValidIdentifiers());

		// initialize it again and check if everything is loaded correctly
		fileIdCache.initialize(model);
		final int[] valid = fileIdCache.getValidIdentifiers().getIds();
		assertEquals(100, fileIdCache.getLastUsedIdentifier());
		assertEquals(5, valid.length);
		assertEquals(0, Arrays.binarySearch(valid, 2));
		assertEquals(1, Arrays.binarySearch(valid, 4));
		assertEquals(2, Arrays.binarySearch(valid, 5));
		assertEquals(3, Arrays.binarySearch(valid, 6));
		assertEquals(4, Arrays.binarySearch(valid, 100));
	}

	/**
	 * Tests the none-persting setting.
	 */
	@Test
	public void testReloadingWithBulk() {
		fileIdCache.setPersistency(false);
		fileIdCache.markIdentifierAsUsed(50);
		fileIdCache.markIdentifierAsValid(2, 4, 5);
		fileIdCache.markIdentifierAsUsed(100);
		fileIdCache.markIdentifierAsValid(6, 100);
		fileIdCache.setPersistency(true);

		fileIdCache.setPersistency(false);
		fileIdCache.markIdentifierAsUsed(1000);
		fileIdCache.markIdentifierAsValid(300, 550, 400);

		// release the cache
		fileIdCache.release();
		assertNull(fileIdCache.getValidIdentifiers());

		// initialize it again and check if everything is loaded correctly
		fileIdCache.initialize(model);
		final int[] valid = fileIdCache.getValidIdentifiers().getIds();
		assertEquals(1000, fileIdCache.getLastUsedIdentifier());
		assertEquals(fileIdCache.toString(), 8, valid.length);
		assertEquals(0, Arrays.binarySearch(valid, 2));
		assertEquals(1, Arrays.binarySearch(valid, 4));
		assertEquals(2, Arrays.binarySearch(valid, 5));
		assertEquals(3, Arrays.binarySearch(valid, 6));
		assertEquals(4, Arrays.binarySearch(valid, 100));
		assertEquals(5, Arrays.binarySearch(valid, 300));
		assertEquals(6, Arrays.binarySearch(valid, 400));
		assertEquals(7, Arrays.binarySearch(valid, 550));
	}

	/**
	 * Tests the exception to be thrown if the array contains an invalid value.
	 */
	@Test
	public void testInvalidValidation() {
		thrown.expect(BaseIdentifierCacheException.class);
		thrown.expectMessage("array contains an identifier which is not allowed");

		fileIdCache.markIdentifierAsUsed(1000);
		fileIdCache.markIdentifierAsValid(4, 5, 1001, 2, 8, 99, 100, 201);
	}

	/**
	 * Tests the exception to be thrown if the array contains an invalid value.
	 */
	@Test
	public void testInvalidInvalidation() {
		thrown.expect(BaseIdentifierCacheException.class);
		thrown.expectMessage("array contains an identifier which is not allowed");

		fileIdCache.markIdentifierAsUsed(1000);
		fileIdCache.markIdentifierAsValid(4, 5, 1001, 2, 8, 99, 100, 201);
	}

	/**
	 * If a module was loaded any created folder is deleted.
	 */
	@After
	public void cleanUp() {
		if (fileIdCache != null) {
			fileIdCache.release();
		}
		if (model != null) {
			model.release(true);
		}
	}
}
