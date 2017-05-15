package net.meisen.master.meike.impl.mapping.costCalculation;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.MappingFactory;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * Calculates the cost for previously unmapped intervals of the longer dataset
 * by calculating another mapping from them to the intervals of the shorter
 * dataset.
 */
public class MapAgain implements ICostCalculator {
    private final IMinCostMapper minCostMapper =
            KuhnMunkres.from(MappingFactory.from(new CompleteMatrix()));

    private final IIntervalDistance distance;

    private MapAgain(final IIntervalDistance distance) {
        this.distance = distance;
    }

    public static MapAgain fromDistance(final IIntervalDistance distance) {
        return new MapAgain(distance);
    }

    @Override
    public double calculateCost(final CostMatrix costMatrix,
                                final List<Integer> mappingIndices) {
        final List<Integer> unmappedIntervals = new LinkedList<>();
        final List<Integer> mappedIntervals = new LinkedList<>();

        double matchingCost = 0;
        for (int i = 0; i < costMatrix.getFirstDatasetLength(); i++) {
            final int mappedIntervalIndex = mappingIndices.get(i);
            if (mappedIntervalIndex != Mapping.NOT_MAPPED) {
                matchingCost += costMatrix.getCosts()[i][mappedIntervalIndex];
                mappedIntervals.add(mappedIntervalIndex);
            } else {
                unmappedIntervals.add(i);
            }
        }

        matchingCost += this.getCostOfSecondMatching(
                this.reduceFirstDataset(unmappedIntervals, costMatrix),
                this.reduceSecondDataset(mappedIntervals, costMatrix));

        return matchingCost;
    }

    private List<Interval> reduceFirstDataset(final List<Integer> unmappedIntervals,
                                              final CostMatrix costMatrix) {
        final List<Interval> firstIntervals = costMatrix.getFirstDataset();
        final ArrayList<Interval> firstDataset = new ArrayList<>();
        for (final int i : unmappedIntervals) {
            firstDataset.add(firstIntervals.get(i));
        }
        return firstDataset;
    }

    private List<Interval> reduceSecondDataset(final List<Integer> mappedIntervals,
                                               final CostMatrix costMatrix) {
        final List<Interval> secondIntervals = costMatrix.getSecondDataset();
        final List<Interval> secondDataset = new LinkedList<>(secondIntervals);
        for (final int i : mappedIntervals) {
            secondDataset.remove(secondIntervals.get(i));
        }
        return secondDataset;
    }

    private double getCostOfSecondMatching(final List<Interval> first,
                                           final List<Interval> second) {
        final CostMatrix costMatrix =
                new CostMatrix(distance, new Dataset(first), new Dataset(second));
        return this.minCostMapper.calculateMinimumCostMapping(costMatrix).getCost();
    }
}
