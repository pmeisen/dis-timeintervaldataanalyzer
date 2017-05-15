package net.meisen.master.meike.impl.mapping.costCalculation;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;

import java.util.List;

/**
 * Calculates the cost for previously unmapped intervals of the longer dataset
 * by calculating another mapping from them to the intervals of the shorter
 * dataset.
 */
public class MapAgain implements ICostCalculator {
    private final IMinCostMapper minCostMapper = KuhnMunkres.create();
    private final IIntervalDistance distance;
    private final double costForFinallyUnmappedIntervals;

    private MapAgain(final IIntervalDistance distance,
                     final double costForFinallyUnmappedIntervals) {
        this.distance = distance;
        this.costForFinallyUnmappedIntervals = costForFinallyUnmappedIntervals;
    }

    public static MapAgain from(final IIntervalDistance distance,
                                final double costForFinallyUnmappedIntervals) {
        assert null != distance;

        return new MapAgain(distance, costForFinallyUnmappedIntervals);
    }

    @Override
    public double calculateCost(final Mapping mapping) {
        double mappingCost = mapping.getMappingCosts().stream()
                .mapToDouble(opt -> opt.orElse(0.0))
                .sum();

        final List<Interval> unmappedIntervals =
                mapping.getUnmappedIntervalsOfLargerDataset();
        final List<Interval> intervalsOfSmallerDataset =
                mapping.getIntervalsOfSmallerDataset();

        mappingCost += this.getCostOfSecondMatching(unmappedIntervals,
                intervalsOfSmallerDataset);

        return mappingCost;
    }

    private double getCostOfSecondMatching(final List<Interval> unmapped,
                                           final List<Interval> smaller) {
        final CostMatrix costMatrix =
                new CostMatrix(distance, new Dataset(unmapped), new Dataset(smaller));
        final Mapping mapping =
                this.minCostMapper.calculateMinimumCostMapping(costMatrix);
        final ICostCalculator costCalculator =
                this.getCostCalculator(unmapped, smaller);
        return costCalculator.calculateCost(mapping);
    }

    private ICostCalculator getCostCalculator(final List<Interval> unmapped,
                                              final List<Interval> smaller) {
        final double costForUnmapped = unmapped.size() < smaller.size()
                ? 0
                : this.costForFinallyUnmappedIntervals;
        return ConstantCostForUnmappedIntervals.fromCost(costForUnmapped);
    }
}
