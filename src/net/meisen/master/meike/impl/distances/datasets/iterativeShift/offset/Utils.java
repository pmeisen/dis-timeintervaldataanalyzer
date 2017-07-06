package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Utility methods for calculating offsets.
 */
public enum Utils {
    ;
    /**
     * Each pair of intervals (p, q) from original x other creates four
     * candidates for the best offset. This method calculates all these
     * candidates.
     *
     * @return all values that might be the best offset, sorted in ascending order.
     */
    public static List<Long> getPossibleOffsets(final Dataset original,
                                                final Dataset candidate) {
        final Set<Long> offsets = new HashSet<>();
        for (final Interval first : original.getIntervals()) {
            for (final Interval second: candidate.getIntervals()) {
                offsets.add(first.getStart() - second.getEnd() + second.getOffset());
                offsets.add(first.getEnd() - second.getEnd() + second.getOffset());
                offsets.add(first.getStart() - second.getStart() + second.getOffset());
                offsets.add(first.getEnd() - second.getStart() + second.getOffset());
            }
        }
        return offsets.stream().sorted().collect(Collectors.toList());
    }
}
