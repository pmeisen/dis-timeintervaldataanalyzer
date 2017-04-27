package net.meisen.master.meike.impl.matching.lowerBounds;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.matching.CostMatrix;
import net.meisen.master.meike.impl.matching.IDatasetMinCostMapper;
import net.meisen.master.meike.impl.matching.Mapping;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.OptionalDouble;
import java.util.OptionalInt;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;

import static java.util.stream.Collectors.toList;

/**
 * Lower bound obtained by ignoring the constraint that we need a matching.
 */
public class IgnoreMatching implements IDatasetMinCostMapper {
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
    public Mapping calculateMinimumCostMapping(final Dataset firstDataset,
                                               final Dataset secondDataset) {
        final CostMatrix costMatrix = new CostMatrix(this.distanceMeasure,
                firstDataset, secondDataset);

        return this.getRowMinimaMapping(costMatrix);
    }

    private Mapping getRowMinimaMapping(final CostMatrix costMatrix) {
        double[][] costs = costMatrix.getCosts();
        int[] mappingIndices = new int[costs.length];
        double mappingCost = 0;
        for (int i = 0; i < costs.length; i++) {
            final OptionalDouble minimumEntry = DoubleStream.of(costs[i]).min();
            assert minimumEntry.isPresent();

            mappingCost += minimumEntry.getAsDouble();
            mappingIndices[i] = DoubleStream.of(costs[i]).boxed()
                    .collect(toList()).indexOf(minimumEntry.getAsDouble());
        }
        return Mapping.create(mappingCost, mappingIndices);
    }
}
