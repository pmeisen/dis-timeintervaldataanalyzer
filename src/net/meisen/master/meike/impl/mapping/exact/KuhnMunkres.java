package net.meisen.master.meike.impl.mapping.exact;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IDatasetMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.MappingFactory;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementation of the Kuhn-Munkres algorithm.
 */
public class KuhnMunkres implements IDatasetMinCostMapper {
    private final IIntervalDistance distanceMeasure;
    private final MappingFactory mappingFactory;

    private KuhnMunkres(final IIntervalDistance distanceMeasure,
                        final MappingFactory mappingFactory) {
        this.distanceMeasure = distanceMeasure;
        this.mappingFactory = mappingFactory;
    }

    /**
     * Creates a new instance of the Kuhn-Munkres implementation using the given
     * distance measure as a cost function between intervals.
     *
     * @param distanceMeasure
     *           the distance measure to determine the cost of mapping one
     *           interval to another; must not be {@code null}.
     *
     * @return an instance of the Kuhn-Munkres implementation
     */
    public static KuhnMunkres from(final IIntervalDistance distanceMeasure,
                                   final MappingFactory mappingFactory) {
        assert null != distanceMeasure;
        assert null != mappingFactory;

        return new KuhnMunkres(distanceMeasure, mappingFactory);
    }

    @Override
    public Mapping calculateMinimumCostMapping(final Dataset firstDataset,
                                               final Dataset secondDataset) {
        final CostMatrix costMatrix = new CostMatrix(this.distanceMeasure,
                firstDataset, secondDataset);

        return this.calculateMinimumCostMapping(costMatrix);
    }

    @Override
    public Mapping calculateMinimumCostMapping(CostMatrix costMatrix) {
        final double[][] costs = costMatrix.getCosts();
        final List<Integer> matchingIndices = Arrays
                .stream(new HungarianAlgorithm(costs).execute())
                .boxed()
                .collect(Collectors.toList());

        return this.mappingFactory.create(costMatrix, matchingIndices);
    }
}
