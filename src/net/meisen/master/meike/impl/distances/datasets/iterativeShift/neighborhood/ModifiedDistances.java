package net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Defines a neighborhood by applying a set of different distance measures:
 * For each such distance measure, the best {@link Mapping} for the given
 * {@link Mapping}'s current offset is included in the neighborhood.
 */
public class ModifiedDistances implements INeighborhood {
    private final List<IIntervalDistance> distanceMeasures;
    private final IMinCostMapper mapper;

    private ModifiedDistances(final List<IIntervalDistance> distanceMeasures,
                              final IMinCostMapper mapper) {
        this.distanceMeasures = distanceMeasures;
        this.mapper = mapper;
    }

    public static ModifiedDistances using(
            final List<IIntervalDistance> distanceMeasures,
            final IMinCostMapper mapper) {
        return new ModifiedDistances(distanceMeasures, mapper);
    }

    @Override
    public List<Mapping> getNeighbors(final Mapping mapping,
                                      final Dataset original,
                                      final Dataset other) {
        other.setOffset(mapping.getOffset());
        return this.distanceMeasures.stream()
                .map(distanceMeasure -> mapper
                        .calculateMinimumCostMapping(new CostMatrix(distanceMeasure, original, other))
                        .withOffset(mapping.getOffset()))
                .collect(Collectors.toList());
    }
}
