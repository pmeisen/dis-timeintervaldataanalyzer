package net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.List;

/**
 * Interface that defines a neighborhood of a {@link Mapping}, i.e. a set of
 * similar {@link Mapping}s.
 */
public interface INeighborhood {
    /**
     * Returns a list of {@link Mapping}s between the intervals of the given
     * {@link Dataset}s that are some kind of neighbors to the given mapping.
     *
     * @param mapping
     *          the current, original mapping
     * @param original
     *          the original dataset whose intervals are mapped to the other
     * @param other
     *          the other dataset whose intervals the original is mapped to
     * @return a list of neighboring mappings
     */
    List<Mapping> getNeighbors(Mapping mapping, Dataset original, Dataset other);
}
