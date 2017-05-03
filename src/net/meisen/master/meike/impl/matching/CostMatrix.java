package net.meisen.master.meike.impl.matching;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.OptionalDouble;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

/**
 * Utility class for creating and manipulating the cost matrix for two given
 * sets of {@link Interval}s and a distance measure.
 */
public class CostMatrix {
    private final double[][] costs;
    private final int firstDatasetLength;
    private final int secondDatasetLength;

    public CostMatrix(final IIntervalDistance distanceMeasure,
                      final Dataset firstDataset,
                      final Dataset secondDataset) {
        final Interval[] firstIntervals = firstDataset.getIntervals().toArray(new Interval[0]);
        this.firstDatasetLength = firstIntervals.length;
        final Interval[] secondIntervals = secondDataset.getIntervals().toArray(new Interval[0]);
        this.secondDatasetLength = secondIntervals.length;
        this.costs = this.createEmptyCostMatrix(Math.max(firstIntervals.length, secondIntervals.length));
        this.fillCostMatrix(distanceMeasure, firstIntervals, secondIntervals);
    }

    public double[][] getCosts() {
        return this.costs;
    }

    public int getFirstDatasetLength() {
        return this.firstDatasetLength;
    }

    public int getSecondDatasetLength() {
        return this.secondDatasetLength;
    }

    private double[][] createEmptyCostMatrix(final int dimension) {
        final double[][] costs = new double[dimension][];
        for (int i = 0; i < dimension; i++) {
            costs[i] = new double[dimension];
        }
        return costs;
    }

    private List<Callable<Double>> getCallables(final IIntervalDistance distance,
                                                final Interval[] first,
                                                final Interval[] second) {
        final List<Callable<Double>> callables = new LinkedList<>();

        return callables;
    }

    private void fillCostMatrix(final IIntervalDistance distance,
                                final Interval[] first,
                                final Interval[] second) {
        double maximum = 0;

        for (int i = 0; i < first.length; i++) {
            for (int j = 0; j < second.length; j++) {
                final double cost = distance.calculate(first[i], second[j]);
                this.costs[i][j] = cost;
                maximum = Math.max(maximum, cost);
            }
        }

        this.fillDummyValuesWith(maximum, first.length, second.length);
    }

    private void fillDummyValuesWith(final double value, final int firstLength,
                                     final int secondLength) {
        final int dimension = Math.max(firstLength, secondLength);

        if (firstLength < dimension) {
            for (int i = firstLength; i < dimension; i++) {
                for (int j = 0; j < dimension; j++) {
                    costs[i][j] = value;
                }
            }
        } else if (secondLength < dimension) {
            for (int i = 0; i < dimension; i++) {
                for (int j = secondLength; j < dimension; j++) {
                    costs[i][j] = value;
                }
            }
        }
    }
}