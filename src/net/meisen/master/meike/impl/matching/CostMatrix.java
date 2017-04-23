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
    private final ExecutorService executorService = Executors.newWorkStealingPool();

    public CostMatrix(final IIntervalDistance distanceMeasure,
                      final Dataset firstDataset,
                      final Dataset secondDataset) {
        final Interval[] firstIntervals = firstDataset.getIntervals().toArray(new Interval[0]);
        final Interval[] secondIntervals = secondDataset.getIntervals().toArray(new Interval[0]);
        this.costs = this.createEmptyCostMatrix(Math.max(firstIntervals.length, secondIntervals.length));
        this.fillCostMatrix(distanceMeasure, firstIntervals, secondIntervals);
    }

    public double[][] getCosts() {
        return this.costs;
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
        for (int i = 0; i < first.length; i++) {
            final int index = i;
            callables.add(() -> {
                double maximum = 0;
                for (int j = 0; j < second.length; j++) {
                    final double cost = distance.calculate(first[index], second[j]);
                    this.costs[index][j] = cost;
                    maximum = Math.max(maximum, cost);
                }
                return maximum;
            });
        }
        return callables;
    }

    private void fillCostMatrix(final IIntervalDistance distance,
                                final Interval[] first,
                                final Interval[] second) {
        final double maximum =
                this.runAndGetMaximum(this.getCallables(distance, first, second));
        this.fillDummyValuesWith(maximum, first.length, second.length);

        executorService.shutdown();
        try {
            executorService.awaitTermination(3, TimeUnit.SECONDS);
        } catch (final InterruptedException e) {
            throw new IllegalStateException(e);
        }
    }

    private double runAndGetMaximum(final Collection<Callable<Double>> callables) {
        try {
            final OptionalDouble optionalMaximum = executorService.invokeAll(callables)
                    .stream()
                    .mapToDouble(future -> {
                        try {
                            return future.get();
                        } catch (Exception exception) {
                            throw new IllegalStateException(exception);
                        }
                    }).max();
            if (optionalMaximum.isPresent()) {
                return optionalMaximum.getAsDouble();
            }
        } catch (final InterruptedException exception) {
            throw new IllegalStateException(exception);
        }
        return Integer.MAX_VALUE;
    }

    private void fillDummyValuesWith(final double value, final int firstLength,
                                     final int secondLength) {
        final int dimension = Math.max(firstLength, secondLength);

        if (firstLength < dimension) {
            for (int i = firstLength; i < dimension; i++) {
                final int index = i;
                executorService.submit(() -> {
                    for (int j = 0; j < dimension; j++) {
                        costs[index][j] = value;
                    }
                });
            }
        } else if (secondLength < dimension) {
            for (int i = 0; i < dimension; i++) {
                final int index = i;
                executorService.submit(() -> {
                    for (int j = secondLength; j < dimension; j++) {
                        costs[index][j] = value;
                    }
                });
            }
        }
    }
}