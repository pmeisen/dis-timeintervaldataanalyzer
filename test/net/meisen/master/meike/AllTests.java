package net.meisen.master.meike;

import net.meisen.master.meike.impl.distances.datasets.TestBestShiftDistance;
import net.meisen.master.meike.impl.distances.datasets.TestDatasetFactory;
import net.meisen.master.meike.impl.distances.datasets.TestIterativeShiftDistance;
import net.meisen.master.meike.impl.distances.intervals.TestBasicDistances;
import net.meisen.master.meike.impl.distances.intervals.TestWeightedSumDistance;
import net.meisen.master.meike.impl.knnSearch.TestNearestNeighbors;
import net.meisen.master.meike.impl.matching.hungarian.TestCostMatrix;
import net.meisen.master.meike.impl.matching.hungarian.TestKuhnMunkres;
import net.meisen.master.meike.performance.TestPerformanceCostMatrix;
import net.meisen.master.meike.performance.TestPerformanceDatasetFactory;
import net.meisen.master.meike.performance.TestPerformanceFlughafen;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;

/**
 * All tests together as a {@link Suite}
 */
@RunWith(Suite.class)
@Suite.SuiteClasses({
        TestBestShiftDistance.class,
        TestIterativeShiftDistance.class,
        TestDatasetFactory.class,
        TestBasicDistances.class,
        TestWeightedSumDistance.class,
        TestCostMatrix.class,
        TestKuhnMunkres.class,
        TestNearestNeighbors.class,
        TestPerformanceDatasetFactory.class,
        TestPerformanceCostMatrix.class,
        TestPerformanceFlughafen.class
})
public class AllTests {

}
