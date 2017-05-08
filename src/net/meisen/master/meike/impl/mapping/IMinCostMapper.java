package net.meisen.master.meike.impl.mapping;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;

/**
 * Interface for algorithms that calculate a min-cost mapping (usually a
 * matching) between the {@link Interval}s of two {@link Dataset}s.
 */
public interface IMinCostMapper {
    /**
     * Calculates a minimum cost mapping of rows to columns based on the mapping
     * costs given by the {@link CostMatrix}. Usually this is a maximum
     * cardinality matching, but it may also be just a mapping.
     *
     * @param costMatrix
     *          entry [i,j] specifies the cost of mapping row i to column j
     * @return a mapping from rows to columns with minimum cost
     */
    Mapping calculateMinimumCostMapping(CostMatrix costMatrix);
}
