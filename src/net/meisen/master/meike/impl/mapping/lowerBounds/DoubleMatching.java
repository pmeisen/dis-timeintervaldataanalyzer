package net.meisen.master.meike.impl.mapping.lowerBounds;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.MappingFactory;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Lower bound obtained by ignoring the constraint that we need a matching.
 */
public class DoubleMatching implements ILowerBound {
    private final MappingFactory mappingFactory;
    private final IIntervalDistance intervalDistance;

    private DoubleMatching(final MappingFactory mappingFactory,
                           final IIntervalDistance intervalDistance) {
        this.mappingFactory = mappingFactory;
        this.intervalDistance = intervalDistance;
    }

    /**
     * Creates a new instance of the lower bound implementation.
     *
     * @return an instance of the double matching lower bound implementation
     */
    public static DoubleMatching from(final MappingFactory mappingFactory,
                                      final IIntervalDistance intervalDistance) {
        assert null != mappingFactory;
        assert null != intervalDistance;

        return new DoubleMatching(mappingFactory, intervalDistance);
    }

    @Override
    public Mapping calculateMinimumCostMapping(final CostMatrix costMatrix) {
        assert null != costMatrix;

        return this.getRowMinimaMapping(costMatrix);
    }

    private Mapping getRowMinimaMapping(final CostMatrix costMatrix) {
        final double[][] costs = costMatrix.getCosts();
        final List<Integer> mappingIndices = Arrays.stream(costs)
                .map(row -> IntStream.range(0, row.length)
                        .reduce((i,j) -> row[i] > row[j] ? i : j)
                        .orElse(Mapping.NOT_MAPPED))
                .collect(Collectors.toList());

        return this.mappingFactory.create(costMatrix, mappingIndices);
    }

    @Override
    public double calculate(final Dataset original, final Dataset other) {
        final CostMatrix costMatrix =
                new CostMatrix(this.intervalDistance, original, other);

        return this.calculateMinimumCostMapping(costMatrix).getCost();
    }
}
