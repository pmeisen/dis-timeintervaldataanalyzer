package net.meisen.master.meike.impl.matching.lowerBounds;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.matching.CostMatrix;
import net.meisen.master.meike.impl.matching.IDatasetMinCostMatcher;

import java.util.Arrays;

/**
 * Lower bound obtained by ignoring the constraint that we need a matching.
 */
public class IgnoreMatching implements IDatasetMinCostMatcher {
    private final IIntervalDistance distanceMeasure;

    private IgnoreMatching(final IIntervalDistance distanceMeasure) {
        this.distanceMeasure = distanceMeasure;
    }

    /**
     * Creates a new instance of the lower bound implementation using the given
     * distance measure as a cost function between intervals.
     *
     * @param distanceMeasure
     *           the distance measure to determine the cost of matching one
     *           interval to another; must not be {@code null}.
     *
     * @return an instance of the "ignore matching" lower bound implementation
     */
    public static IgnoreMatching from(final IIntervalDistance distanceMeasure) {
        assert null != distanceMeasure;

        return new IgnoreMatching(distanceMeasure);
    }
    @Override
    public double calculateMinimumCost(final Dataset firstDataset,
                                       final Dataset secondDataset) {
        final long startTime = System.currentTimeMillis();
        final CostMatrix costMatrix = new CostMatrix(this.distanceMeasure,
                firstDataset, secondDataset);
        final long costMatrixFinishTime = System.currentTimeMillis();
        final long costMatrixTime = costMatrixFinishTime - startTime;
        System.out.println("Cost matrix: \t" + costMatrixTime);

        final double lowerBoundValue = this.sumOfRowMinima(costMatrix);
        final long algorithmFinishTime = System.currentTimeMillis();
        final long algorithmTime = algorithmFinishTime - costMatrixFinishTime;
        System.out.println("Algorithm:\t" + algorithmTime);

        System.out.println("Ratio: \t" + algorithmTime / (double) costMatrixTime);

        return lowerBoundValue;
    }

    private double sumOfRowMinima(final CostMatrix costMatrix) {
        double result = 0;
        for (final double[] row : costMatrix.getCosts()) {
            result += Arrays.stream(row).min().getAsDouble();
        }
        return result;
    }
}
