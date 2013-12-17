package net.meisen.dissertation;

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

		// do the unit tests
		AllUnitTests.class,

		// do the performance tests
		AllPerformanceTests.class })
public class AllTests {
	// nothing more to do here
}
