package net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Combines the results of several neighborhoods.
 */
public class CombinedNeighborhood implements INeighborhood {
    private final List<INeighborhood> originalNeighborhoods;

    private CombinedNeighborhood(final List<INeighborhood> neighborhoods) {
        this.originalNeighborhoods = neighborhoods;
    }

    public static CombinedNeighborhood from(final List<INeighborhood> neighborhoods) {
        return new CombinedNeighborhood(neighborhoods);
    }

    @Override
    public List<Mapping> getNeighbors(Mapping mapping, Dataset original, Dataset other) {
        return this.originalNeighborhoods.stream()
                .flatMap(n -> n.getNeighbors(mapping, original, other).stream())
                .collect(Collectors.toList());
    }
}
