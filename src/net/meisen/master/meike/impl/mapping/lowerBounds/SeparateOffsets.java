package net.meisen.master.meike.impl.mapping.lowerBounds;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.BestShiftDistance;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;

/**
 * Lower bound obtained by allowing every interval to choose a separate offset.
 */
public class SeparateOffsets implements ILowerBound {
    private final IMinCostMapper minCostMapper = KuhnMunkres.create();
    private final IIntervalDistance intervalDistance;
    private final ICostCalculator costCalculator;

    private SeparateOffsets(final IIntervalDistance intervalDistance,
                            final ICostCalculator costCalculator) {
        this.intervalDistance = BestShiftDistance.from(intervalDistance);
        this.costCalculator = costCalculator;
    }

    public static SeparateOffsets from(final IIntervalDistance intervalDistance,
                                           final ICostCalculator costCalculator) {
        assert null != intervalDistance;
        assert null != costCalculator;
        return new SeparateOffsets(intervalDistance, costCalculator);
    }

    @Override
    public double calculate(final Dataset original, final Dataset other) {
        final CostMatrix costMatrix =
                new CostMatrix(this.intervalDistance, original, other);
        final Mapping mapping =
                this.minCostMapper.calculateMinimumCostMapping(costMatrix);
        return this.costCalculator.calculateCost(mapping);
    }
}
