package net.meisen.master.meike.impl.mapping.lowerBounds;

import net.meisen.master.meike.impl.distances.datasets.Dataset;

/**
 * Interface for lower bounding mappers.
 */
public interface ILowerBound {
    /**
     * Calculates the lower bounding value for the minimum cost maximum
     * cardinality matching between the intervals of the given {@link Dataset}s.
     *
     * @param original
     *          the original dataset
     * @param other
     *          a candidate dataset to be compared to the original dataset
     * @return a lower bounding value for the minimum cost matching between
     * the intervals of {@code original} and {@code other}
     */
    double calculate(Dataset original, Dataset other);
}
