package net.meisen.master.meike.impl.matching.hungarian;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.matching.IDatasetMinCostMatcher;

/**
 * Implementation of the Kuhn-Munkres algorithm.
 */
public class KuhnMunkres implements IDatasetMinCostMatcher {
    private final IIntervalDistance distanceMeasure;

    private KuhnMunkres(final IIntervalDistance distanceMeasure) {
        this.distanceMeasure = distanceMeasure;
    }

    /**
     * Creates a new instance of the Kuhn-Munkres implementation using the given
     * distance measure as a cost function between intervals.
     *
     * @param distanceMeasure
     *           the distance measure to determine the cost of matching one
     *           interval to another; must not be {@code null}.
     *
     * @return an instance of the Kuhn-Munkres implementation
     */
    public static KuhnMunkres from(final IIntervalDistance distanceMeasure) {
        assert null != distanceMeasure;

        return new KuhnMunkres(distanceMeasure);
    }

    @Override
    public double calculateMinimumCost(final Dataset firstDataset,
                                       final Dataset secondDataset) {
        final double[][] costs = new CostMatrix(this.distanceMeasure,
                firstDataset, secondDataset).getCosts();
        final HungarianAlgorithm algorithm = new HungarianAlgorithm(costs);
        final int[] matching = algorithm.execute();

        double matchingCost = 0;
        for (int i = 0; i < costs.length; i++) {
            matchingCost += costs[i][matching[i]];
        }
        return matchingCost;
    }
}
