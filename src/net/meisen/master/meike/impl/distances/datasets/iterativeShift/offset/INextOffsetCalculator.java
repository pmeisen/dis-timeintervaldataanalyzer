package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.List;

/**
 * Interface for calculating the next offset from the current {@link Mapping}.
 */
public interface INextOffsetCalculator {
    /**
     * Calculate good offset values based on the given {@code mapping} so that
     * the mapped intervals are better aligned.
     *
     * @param mapping
     *          the current mapping
     * @return a list of next offset values to be used for the "other" dataset
     */
    List<Long> calculate(Mapping mapping);
}
