package net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.List;

/**
 * Always returns an empty neighborhood.
 */
public class EmptyNeighborhood implements INeighborhood {
    @Override
    public List<Mapping> getNeighbors(final Mapping mapping,
                                      final Dataset original,
                                      final Dataset other) {
        return ImmutableList.of();
    }
}
