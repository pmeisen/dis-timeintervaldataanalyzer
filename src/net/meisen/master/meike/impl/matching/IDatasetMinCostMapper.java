package net.meisen.master.meike.impl.matching;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;

/**
 * Interface for algorithms that calculate a min-cost mapping (usually a
 * matching) between the {@link Interval}s of two {@link Dataset}s.
 */
public interface IDatasetMinCostMapper {
    /**
     * Calculates a minimum cost mapping between the {@link Interval}s of the
     * given {@link Dataset}s. Usually this is a maximum cardinality matching
     *
     * @param original
     *          the original dataset which the other is being compared to
     * @param other
     *          a candidate dataset to compare to the original
     * @return a mapping between the two datasets' intervals with minimum cost
     */
    Mapping calculateMinimumCostMapping(Dataset original, Dataset other);
}
