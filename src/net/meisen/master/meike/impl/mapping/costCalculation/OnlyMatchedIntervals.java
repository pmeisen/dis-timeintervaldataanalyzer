package net.meisen.master.meike.impl.mapping.costCalculation;

import net.meisen.master.meike.impl.mapping.CostMatrix;

import java.util.List;

/**
 * Assigns zero cost to any interval that is not mapped to a corresponding real
 * interval but to a dummy used only to make the cost matrix quadratic.
 */
public class OnlyMatchedIntervals implements ICostCalculator {

    @Override
    public double calculateCost(final CostMatrix costMatrix,
                                final List<Integer> mappingIndices) {
        double matchingCost = 0;
        for (int i = 0; i < costMatrix.getFirstDatasetLength(); i++) {
            final int mappedIntervalIndex = mappingIndices.get(i);
            if (mappedIntervalIndex < costMatrix.getSecondDatasetLength()) {
                matchingCost += costMatrix.getCosts()[i][mappedIntervalIndex];
            }
        }
        return matchingCost;
    }
}
