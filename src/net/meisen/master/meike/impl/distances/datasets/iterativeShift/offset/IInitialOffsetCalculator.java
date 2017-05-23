package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import net.meisen.master.meike.impl.distances.datasets.Dataset;

import java.util.List;

/**
 * Interface for calculating offsets from two {@link Dataset}s.
 */
public interface IInitialOffsetCalculator {
    /**
     * Calculate good offset values for the intervals of the {@code other}
     * dataset so that they are better aligned with the intervals of the
     * {@code original} dataset.
     *
     * @param original
     *          the original dataset
     * @param other
     *          the other dataset
     * @return a list of initial offset values to be used for the other dataset
     */
    List<Long> calculate(Dataset original, Dataset other);
}
