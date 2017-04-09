package net.meisen.master.meike.impl.matching;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;

/**
 * Interface for algorithms that calculate a min-cost matching between the
 * {@link Interval}s of two {@link Dataset}s.
 */
public interface IDatasetMinCostMatcher {

    /**
     * Calculates a minimum cost maximum cardinality matching between the
     * {@link Interval}s of the given {@link Dataset}s.
     *
     * @param firstDataset
     *           one of the two datasets of intervals to match
     * @param secondDataset
     *           the other dataset
     *
     * @return the minimum cost of matching all intervals of the shorter
     * dataset to exactly one interval of the longer dataset
     */
    double calculateMinimumCost(Dataset firstDataset, Dataset secondDataset);
}
