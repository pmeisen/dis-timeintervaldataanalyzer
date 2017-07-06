package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.Optional;

/**
 * Allows calculating the distance between two data sets based on the distances
 * between their respective {@link Interval}s.
 */
public interface IDatasetDistanceCalculator {

    /**
     * Calculates the next best mapping for this instance's datasets.
     *
     * @return a next mapping between the two datasets, or nothing if the
     * previous mapping was already the best.
     */
    Optional<Mapping> nextMapping();

    Mapping finalMapping();
}

