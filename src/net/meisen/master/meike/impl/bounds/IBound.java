package net.meisen.master.meike.impl.bounds;

import net.meisen.master.meike.impl.distances.datasets.Dataset;

/**
 * Interface for any bounding measures for {@link Dataset}s.
 */
public interface IBound {
    /**
     * Calculates the bounding value for the distance from a candidate dataset
     * to the original model dataset
     *
     * @param original
     *          the original dataset to which all others are being compared
     * @param candidate
     *          the candidate dataset whose bound is to be calculated
     * @return a bounding value for the distance between the two given datasets
     */
    double calculate(Dataset original, Dataset candidate);
}
