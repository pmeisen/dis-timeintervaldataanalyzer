package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.matching.mapping.Mapping;

/**
 * Allows calculating the distance between two data sets based on the distances
 * between their respective {@link Interval}s.
 */
@FunctionalInterface
public interface IDatasetDistance {

    /**
     * Calculates the distance value for the two given datasets. Note that this
     * is not necessarily symmetric.
     *
     * @param original
     *            The original {@link Dataset}
     * @param other
     *            Another {@link Dataset} to be compared to the original
     * @return the distance value of the two datasets
     */
    Mapping calculate(final Dataset original, final Dataset other);
}

