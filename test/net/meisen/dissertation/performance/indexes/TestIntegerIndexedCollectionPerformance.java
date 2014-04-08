package net.meisen.dissertation.performance.indexes;

import net.meisen.dissertation.performance.PerformanceConfig;
import net.meisen.general.sbconfigurator.api.IConfiguration;
import net.meisen.general.sbconfigurator.runners.JUnitConfigurationRunner;
import net.meisen.general.sbconfigurator.runners.annotations.ContextClass;
import net.meisen.general.sbconfigurator.runners.annotations.ContextFile;
import net.meisen.general.sbconfigurator.runners.annotations.SystemProperty;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Tests the performance of the {@code BaseIndexedCollection} with different
 * {@code IndexKeyDefinition}. It is also possible to add {@code Map}
 * implementations.
 * 
 * @author pmeisen
 * 
 */
@RunWith(JUnitConfigurationRunner.class)
@ContextClass(PerformanceConfig.class)
@ContextFile("performance-sbconfigurator-core.xml")
@SystemProperty(property = "performanceBeans.selector", value = "net/meisen/dissertation/performance/indexes/integerIndexedCollectionBeans.xml")
public class TestIntegerIndexedCollectionPerformance {

	@Autowired(required = true)
	@Qualifier("coreConfiguration")
	private IConfiguration configuration;

	/**
	 * Generator which creates an integer for the specified data.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class IntegerIdGenerator implements IIdGenerator {

		@Override
		public Object generateId(final Object data) {
			return ((Integer) data).intValue();
		}
	}

	/**
	 * The used data generator.
	 * 
	 * @author pmeisen
	 * 
	 */
	public static class IntegerDataGenerator implements IDataGenerator {

		private int i = 0;

		@Override
		public Object generateData() {
			i++;
			return new Integer(i);
		}
	}

	/**
	 * Tests the performance of the different {@code IndexKeyDefinition} types
	 * available.
	 */
	@Test
	public void testAddPerformace() {
		final IndexedCollectionAddPerformance addTest = configuration
				.createInstance(IndexedCollectionAddPerformance.class);

		addTest.runAddPerformanceTest();
		addTest.printResult();
	}

	/**
	 * Tests the performance of contains
	 */
	@Test
	public void testContainsPerformace() {
		final IndexedCollectionContainsPerformance containsTest = configuration
				.createInstance(IndexedCollectionContainsPerformance.class);

		containsTest.runContainsPerformanceTest();
		containsTest.printResult();
	}

	/**
	 * Tests the performance of contains
	 */
	@Test
	public void testGetPerformace() {
		final IndexedCollectionGetPerformance getTest = configuration
				.createInstance(IndexedCollectionGetPerformance.class);

		getTest.runGetPerformanceTest();
		getTest.printResult();
	}
}
