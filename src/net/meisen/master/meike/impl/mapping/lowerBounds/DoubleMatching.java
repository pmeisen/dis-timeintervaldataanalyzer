package net.meisen.master.meike.impl.mapping.lowerBounds;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IDatasetMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.MappingFactory;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Lower bound obtained by ignoring the constraint that we need a matching.
 */
public class DoubleMatching implements IDatasetMinCostMapper {
    private final IIntervalDistance distanceMeasure;
    private final MappingFactory mappingFactory;

    private DoubleMatching(final IIntervalDistance distanceMeasure,
                           final MappingFactory mappingFactory) {
        this.distanceMeasure = distanceMeasure;
        this.mappingFactory = mappingFactory;
    }

    /**
     * Creates a new instance of the lower bound implementation using the given
     * distance measure as a cost function between intervals.
     *
     * @param distanceMeasure
     *           the distance measure to determine the cost of mapping one
     *           interval to another; must not be {@code null}.
     *
     * @return an instance of the double matching lower bound implementation
     */
    public static DoubleMatching from(final IIntervalDistance distanceMeasure,
                                      final MappingFactory mappingFactory) {
        assert null != distanceMeasure;
        assert null != mappingFactory;

        return new DoubleMatching(distanceMeasure, mappingFactory);
    }

    @Override
    public Mapping calculateMinimumCostMapping(final Dataset firstDataset,
                                               final Dataset secondDataset) {
        final CostMatrix costMatrix = new CostMatrix(this.distanceMeasure,
                firstDataset, secondDataset);

        return this.calculateMinimumCostMapping(costMatrix);
    }

    @Override
    public Mapping calculateMinimumCostMapping(final CostMatrix costMatrix) {
        assert null != costMatrix;

        return this.getRowMinimaMapping(costMatrix);
    }

    private Mapping getRowMinimaMapping(final CostMatrix costMatrix) throws IllegalStateException{
        final double[][] costs = costMatrix.getCosts();
        final List<Integer> mappingIndices = Arrays.stream(costs)
                .map(row -> IntStream.range(0, row.length)
                        .reduce((i,j) -> row[i] > row[j] ? i : j)
                        .orElse(Mapping.NOT_MAPPED))
                .collect(Collectors.toList());

        return this.mappingFactory.create(costMatrix, mappingIndices);
    }
}
