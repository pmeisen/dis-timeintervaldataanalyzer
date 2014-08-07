package net.meisen.dissertation.impl.cache;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import net.meisen.dissertation.config.TestConfig;
import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.help.ModuleBasedTest;
import net.meisen.dissertation.model.data.TidaModel;
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
public class TestMemoryIdentifierCache extends ModuleBasedTest {

	private TidaModel model = null;
	private MemoryIdentifierCache memIdCache = null;

	/**
	 * Helper to load a {@code MemoryIdentifierCache}.
	 */
	@Before
	public void loadCache() {

		// load the model and get the cache
		setModulesHolder("/net/meisen/dissertation/impl/cache/memoryIdentifierCacheModel.xml");
		model = modulesHolder.getModule(DefaultValues.TIDAMODEL_ID);
		memIdCache = modulesHolder.getModule(DefaultValues.IDENTIFIERCACHE_ID);
		memIdCache.initialize(model);
	}

	/**
	 * Tests the initialization of the cache
	 */
	@Test
	public void testInit() {
		assertEquals(0, memIdCache.getValidIdentifiers().getIds().length);
	}

	/**
	 * Tests the marking of valid values.
	 */
	@Test
	public void testValidationOfValues() {
		int[] valid;

		// check the adding of some values
		memIdCache.markIdentifierAsUsed(10);
		memIdCache.markIdentifierAsValid(2, 3, 4);

		valid = memIdCache.getValidIdentifiers().getIds();
		assertEquals(3, valid.length);
		assertEquals(0, Arrays.binarySearch(valid, 2));
		assertEquals(1, Arrays.binarySearch(valid, 3));
		assertEquals(2, Arrays.binarySearch(valid, 4));

		// add some more (also once already validated)
		memIdCache.markIdentifierAsValid(2, 6, 10);

		valid = memIdCache.getValidIdentifiers().getIds();
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
		memIdCache.markIdentifierAsUsed(2040);
		memIdCache.markIdentifierAsValid(2, 3, 4, 120, 1000, 2040, 204);
		memIdCache.markIdentifierAsInvalid(2, 5, 100, 204, 2000);

		valid = memIdCache.getValidIdentifiers().getIds();
		assertEquals(5, valid.length);
		assertEquals(0, Arrays.binarySearch(valid, 3));
		assertEquals(1, Arrays.binarySearch(valid, 4));
		assertEquals(2, Arrays.binarySearch(valid, 120));
		assertEquals(3, Arrays.binarySearch(valid, 1000));
		assertEquals(4, Arrays.binarySearch(valid, 2040));
	}

	/**
	 * The memory cache should ignore the persistency setting.
	 */
	@Test
	public void testIgnoringOfPersistency() {
		int[] valid;

		assertEquals(-1, memIdCache.getLastUsedIdentifier());
		memIdCache.setPersistency(true);

		memIdCache.markIdentifierAsUsed(100);
		assertEquals(100, memIdCache.getLastUsedIdentifier());
		assertEquals(0, memIdCache.getValidIdentifiers().getIds().length);

		memIdCache.markIdentifierAsUsed(200);
		assertEquals(200, memIdCache.getLastUsedIdentifier());
		assertEquals(0, memIdCache.getValidIdentifiers().getIds().length);

		memIdCache.markIdentifierAsValid(100, 104, 105);
		valid = memIdCache.getValidIdentifiers().getIds();
		assertEquals(200, memIdCache.getLastUsedIdentifier());
		assertEquals(3, valid.length);
		assertEquals(0, Arrays.binarySearch(valid, 100));
		assertEquals(1, Arrays.binarySearch(valid, 104));
		assertEquals(2, Arrays.binarySearch(valid, 105));

		memIdCache.setPersistency(false);

		memIdCache.markIdentifierAsUsed(300);
		memIdCache.markIdentifierAsInvalid(100);
		valid = memIdCache.getValidIdentifiers().getIds();
		assertEquals(300, memIdCache.getLastUsedIdentifier());
		assertEquals(2, valid.length);
		assertEquals(0, Arrays.binarySearch(valid, 104));
		assertEquals(1, Arrays.binarySearch(valid, 105));
	}

	/**
	 * Tests the exception to be thrown if the array contains an invalid value.
	 */
	@Test
	public void testInvalidValidation() {
		thrown.expect(BaseIdentifierCacheException.class);
		thrown.expectMessage("array contains an identifier which is not allowed");

		memIdCache.markIdentifierAsUsed(1000);
		memIdCache.markIdentifierAsValid(4, 5, 1001, 2, 8, 99, 100, 201);
	}

	/**
	 * Tests the exception to be thrown if the array contains an invalid value.
	 */
	@Test
	public void testInvalidInvalidation() {
		thrown.expect(BaseIdentifierCacheException.class);
		thrown.expectMessage("array contains an identifier which is not allowed");

		memIdCache.markIdentifierAsUsed(1000);
		memIdCache.markIdentifierAsValid(4, 5, 1001, 2, 8, 99, 100, 201);
	}

	/**
	 * If a module was loaded any created folder is deleted.
	 */
	@After
	public void cleanUp() {
		if (memIdCache != null) {
			memIdCache.release();
		}
		if (model != null) {
			model.release(true);
		}
	}
}
