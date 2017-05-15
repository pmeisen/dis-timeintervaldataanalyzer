package net.meisen.master.meike.impl.mapping;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;

import java.util.List;

/**
 * Utility class for creating and manipulating the cost matrix for two given
 * sets of {@link Interval}s and a distance measure.
 */
public class CostMatrix {
    private final double[][] costs;
    private final Interval[] firstDataset;
    private final Interval[] secondDataset;

    public CostMatrix(final IIntervalDistance distanceMeasure,
                      final Dataset firstDataset,
                      final Dataset secondDataset) {
        this.firstDataset = firstDataset.getIntervals().toArray(new Interval[0]);
        this.secondDataset = secondDataset.getIntervals().toArray(new Interval[0]);
        this.costs = this.createEmptyCostMatrix(
                Math.max(firstDataset.getNumberOfIntervals(),
                        secondDataset.getNumberOfIntervals()));
        this.fillCostMatrix(distanceMeasure);
    }

    public double[][] getCosts() {
        return this.costs;
    }

    public int getFirstDatasetLength() {
        return this.firstDataset.length;
    }

    public List<Interval> getFirstDataset() {
        return ImmutableList.copyOf(this.firstDataset);
    }

    public int getSecondDatasetLength() {
        return this.secondDataset.length;
    }

    public List<Interval> getSecondDataset() {
        return ImmutableList.copyOf(this.secondDataset);
    }

    private double[][] createEmptyCostMatrix(final int dimension) {
        final double[][] costs = new double[dimension][];
        for (int i = 0; i < dimension; i++) {
            costs[i] = new double[dimension];
        }
        return costs;
    }

    private void fillCostMatrix(final IIntervalDistance distance) {
        double maximum = 0;

        for (int i = 0; i < this.firstDataset.length; i++) {
            for (int j = 0; j < this.secondDataset.length; j++) {
                final double cost = distance.calculate(this.firstDataset[i], this.secondDataset[j]);
                this.costs[i][j] = cost;
                maximum = Math.max(maximum, cost);
            }
        }

        this.fillDummyValuesWith(maximum, this.firstDataset.length, this.secondDataset.length);
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