package net.meisen.dissertation;

import net.meisen.dissertation.performance.indexes.TestIntegerIndexedCollectionPerformance;
import net.meisen.dissertation.performance.indexes.TestLongIndexedCollectionPerformance;
import net.meisen.dissertation.performance.indexes.TestNestedIndexedCollectionPerformance;
import net.meisen.dissertation.performance.indexes.TestStringIndexedCollectionPerformance;

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

		// do the performance tests
		TestIntegerIndexedCollectionPerformance.class,
		TestLongIndexedCollectionPerformance.class,
		TestStringIndexedCollectionPerformance.class,
		TestNestedIndexedCollectionPerformance.class })
public class AllPerformanceTests {

}
