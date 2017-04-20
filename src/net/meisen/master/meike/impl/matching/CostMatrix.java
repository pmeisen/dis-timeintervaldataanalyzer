package net.meisen.master.meike.impl.matching;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;

import java.util.Arrays;

/**
 * Utility class for creating and manipulating the cost matrix for two given
 * sets of {@link Interval}s and a distance measure.
 */
public class CostMatrix {
    private final double[][] costs;

    public CostMatrix(final IIntervalDistance distanceMeasure,
                      final Dataset firstDataset,
                      final Dataset secondDataset) {
        this.costs = this.createCostMatrix(distanceMeasure,
                firstDataset.getIntervals().toArray(new Interval[0]),
                secondDataset.getIntervals().toArray(new Interval[0]));
    }

    public double[][] getCosts() {
        return this.costs;
    }

    public void subtractRowMinima() {
        for (int i = 0; i < this.costs.length; i++) {
            final double minimum =
                    Arrays.stream(this.costs[i]).min().getAsDouble();
            for (int j = 0; j < this.costs.length; j++) {
                costs[i][j] -= minimum;
            }
        }
    }

    public void subtractColumnMinima() {
        for (int j = 0; j < this.costs.length; j++) {
            final int jf = j;
            final double minimum =
                    Arrays.stream(this.costs).mapToDouble(row -> row[jf]).min().getAsDouble();
            for (int i = 0; i < this.costs.length; i++) {
                costs[i][j] -= minimum;
            }
        }
    }

    private double[][] createCostMatrix(final IIntervalDistance distance,
                                        final Interval[] first,
                                        final Interval[] second) {
        final int dimension =
                Math.max(first.length, second.length);

        final double[][] costs = new double[dimension][];
        double maximum = 0;
        for (int i = 0; i < first.length; i++) {
            costs[i] = new double[dimension];
            for (int j = 0; j < second.length; j++) {
                costs[i][j] = distance.calculate(first[i], second[j]);
                if(Double.isNaN(costs[i][j])) {
                    throw new IllegalStateException("Cost must be a number!");
                }
                maximum = Math.max(maximum, costs[i][j]);
            }
        }

        if (first.length < dimension) {
            for (int i = first.length; i < dimension; i++) {
                costs[i] = new double[dimension];
                for (int j = 0; j < dimension; j++) {
                    costs[i][j] = maximum;
                }
            }
        } else if (second.length < dimension) {
            for (int i = 0; i < dimension; i++) {
                for (int j = second.length; j < dimension; j++) {
                    costs[i][j] = maximum;
                }
            }
        }

        return costs;
    }
}