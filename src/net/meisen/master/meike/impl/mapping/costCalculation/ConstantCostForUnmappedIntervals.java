package net.meisen.master.meike.impl.mapping.costCalculation;

import net.meisen.master.meike.impl.mapping.Mapping;

/**
 * Assigns constant costs to any unmapped intervals.
 */
public class ConstantCostForUnmappedIntervals implements ICostCalculator {
    private final double costForUnmappedIntervals;

    private ConstantCostForUnmappedIntervals(final double costForUnmappedIntervals) {
        this.costForUnmappedIntervals = costForUnmappedIntervals;
    }

    /**
     * Creates an instance of this class that uses the given cost value for
     * any of the unmapped intervals.
     *
     * @param costForUnmappedIntervals
     *          cost value to use for unmapped intervals
     * @return an instance of this class
     */
    public static ConstantCostForUnmappedIntervals fromCost(
            final double costForUnmappedIntervals) {
        return new ConstantCostForUnmappedIntervals(costForUnmappedIntervals);
    }

    @Override
    public double calculateCost(Mapping mapping) {
        return mapping.getMappingCosts().stream()
                .mapToDouble(opt -> opt.orElse(this.costForUnmappedIntervals))
                .sum();
    }
}
