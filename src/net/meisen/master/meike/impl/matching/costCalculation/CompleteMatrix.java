package net.meisen.master.meike.impl.matching.costCalculation;

import net.meisen.master.meike.impl.matching.CostMatrix;

import java.util.List;

/**
 * Considers all entries in the matrix that are used for the mapping when
 * calculating the mapping cost, independently of whether those entries
 * correspond to real entries or dummy entries only used to make the matrix
 * quadratic.
 */
public class CompleteMatrix implements ICostCalculator {

    @Override
    public double calculateCost(final CostMatrix costMatrix,
                                final List<Integer> mappingIndices) {
        double matchingCost = 0;
        for (int i = 0; i < costMatrix.getCosts().length; i++) {
            matchingCost += costMatrix.getCosts()[i][mappingIndices.get(i)];
        }
        return matchingCost;
    }
}
