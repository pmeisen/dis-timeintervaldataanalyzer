package net.meisen.master.meike.impl.matching.lowerBounds;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.matching.CostMatrix;
import net.meisen.master.meike.impl.matching.IDatasetMinCostMapper;
import net.meisen.master.meike.impl.matching.costCalculation.ICostCalculator;
import net.meisen.master.meike.impl.matching.mapping.Mapping;
import net.meisen.master.meike.impl.matching.mapping.MappingFactory;

import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.OptionalDouble;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;

import static java.util.stream.Collectors.toList;

/**
 * Lower bound obtained by ignoring the constraint that we need a matching.
 */
public class IgnoreMatching implements IDatasetMinCostMapper {
    private final IIntervalDistance distanceMeasure;
    private final MappingFactory mappingFactory;

    private IgnoreMatching(final IIntervalDistance distanceMeasure,
                           final MappingFactory mappingFactory) {
        this.distanceMeasure = distanceMeasure;
        this.mappingFactory = mappingFactory;
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
    public static IgnoreMatching from(final IIntervalDistance distanceMeasure,
                                      final MappingFactory mappingFactory) {
        assert null != distanceMeasure;
        assert null != mappingFactory;

        return new IgnoreMatching(distanceMeasure, mappingFactory);
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

    private Mapping getRowMinimaMapping(final CostMatrix costMatrix) {
        final double[][] costs = costMatrix.getCosts();
        final List<Integer> mappingIndices = Arrays.stream(costs)
                .map(row -> IntStream.range(0, row.length)
                        .reduce((i,j) -> row[i] > row[j] ? i : j)
                        .getAsInt())
                .collect(Collectors.toList());

        return this.mappingFactory.create(costMatrix, mappingIndices);
    }
}
