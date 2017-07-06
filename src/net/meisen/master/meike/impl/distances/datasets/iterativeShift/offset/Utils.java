package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;

import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.LongStream;

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

    /**
     * Each pair of intervals (p, q) from original x other creates four
     * candidates for the best offset. This method calculates all these
     * candidates.
     *
     * @param mapping
     *          the chosen mapping from intervals of the first dataset to
     *          intervals of the second dataset
     * @return all values that might be the best offset, ordered by ascending
     * absolute value.
     */
    public static List<Long> getPossibleOffsets(final List<Pair<Interval, Interval>> mapping) {
        return mapping.stream()
                .flatMapToLong(pair -> {
                    final Interval first = pair.getKey();
                    final Interval second = pair.getValue();
                    return LongStream.of(
                            first.getStart() - second.getEnd() + second.getOffset(),
                            first.getEnd() - second.getEnd() + second.getOffset(),
                            first.getStart() - second.getStart() + second.getOffset(),
                            first.getEnd() - second.getStart() + second.getOffset());
                })
                .boxed()
                .sorted(Comparator.comparingLong(v -> Math.abs(0 - v)))
                .collect(Collectors.toList());
    }
}
