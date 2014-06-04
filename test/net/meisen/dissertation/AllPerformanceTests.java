package net.meisen.dissertation;

import net.meisen.dissertation.performance.TestLargeDescriptor;
import net.meisen.dissertation.performance.indexes.TestIntegerIndexedCollectionPerformance;
import net.meisen.dissertation.performance.indexes.TestLongIndexedCollectionPerformance;
import net.meisen.dissertation.performance.indexes.TestNestedIndexedCollectionPerformance;
import net.meisen.dissertation.performance.indexes.TestStringIndexedCollectionPerformance;
import net.meisen.dissertation.performance.iteration.TestIteration;
import net.meisen.dissertation.performance.loading.TestLoadingPerformance;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * All tests together as a {@link Suite}
 * 
 * @author pmeisen
 * 
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({

		// index performance
		TestIntegerIndexedCollectionPerformance.class,
		TestLongIndexedCollectionPerformance.class,
		TestStringIndexedCollectionPerformance.class,
		TestNestedIndexedCollectionPerformance.class,

		// descriptorModels performance
		TestLargeDescriptor.class,
		
		// insertion performance
		TestLoadingPerformance.class,
		
		// some miscellaneous performances
		TestIteration.class, })
public class AllPerformanceTests {

}
