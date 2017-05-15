package net.meisen.master.meike.impl.mapping.lowerBounds;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;

/**
 * Lower bound that uses a {@link DoubleMatcher}.
 */
public class DoubleMatchingBound implements ILowerBound {
    private final DoubleMatcher doubleMatcher = DoubleMatcher.create();
    private final IIntervalDistance intervalDistance;
    private final ICostCalculator costCalculator;

    private DoubleMatchingBound(final IIntervalDistance intervalDistance,
                                final ICostCalculator costCalculator) {
        this.intervalDistance = intervalDistance;
        this.costCalculator = costCalculator;
    }

    public static DoubleMatchingBound from(final IIntervalDistance intervalDistance,
                                           final ICostCalculator costCalculator) {
        assert null != intervalDistance;
        assert null != costCalculator;
        return new DoubleMatchingBound(intervalDistance, costCalculator);
    }

    @Override
    public double calculate(final Dataset original, final Dataset other) {
        final CostMatrix costMatrix =
                new CostMatrix(this.intervalDistance, original, other);
        final Mapping mapping =
                this.doubleMatcher.calculateMinimumCostMapping(costMatrix);
        return this.costCalculator.calculateCost(mapping);
    }
}
