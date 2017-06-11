package net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.List;

/**
 * Calculates the neighbors of a permutation by applying local perturbations.
 */
public class LocalPerturbation implements INeighborhood {
    @Override
    public List<Mapping> getNeighbors(final Mapping mapping,
                                      final Dataset original,
                                      final Dataset other) {
        // Todo
        return ImmutableList.of();
    }
}
