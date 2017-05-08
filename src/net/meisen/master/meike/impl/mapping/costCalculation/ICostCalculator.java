package net.meisen.master.meike.impl.mapping.costCalculation;

import net.meisen.master.meike.impl.mapping.CostMatrix;

import java.util.List;

/**
 * Interface for classes that allow calculating the cost of a mapping.
 */
public interface ICostCalculator {
    /**
     * Calculates the cost of the mapping given by the {@code mappingIndices}
     * from the given cost matrix.
     *
     * @param costMatrix
     *          {@link CostMatrix} specifying the costs of interval pairs
     * @param mappingIndices
     *          mapping from the intervals of the first dataset to intervals of
     *          the second dataset from which the cost matrix was built; to
     *          indicate that an interval has no counterpart in the second
     *          dataset, the mapped index is at least as large as the number of
     *          intervals in the second dataset.
     * @return the cost of the given mapping and cost matrix
     */
    double calculateCost(CostMatrix costMatrix, List<Integer> mappingIndices);
}
