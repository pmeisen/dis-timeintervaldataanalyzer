package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.matching.IDatasetMinCostMatcher;

import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Allows calculating the distance between two {@link Dataset}s considering
 * the possibility to shift all {@link Interval}s of one of them by a constant
 * offset.
 */
public class BestShiftDistance implements IDatasetDistance {

    private final IDatasetMinCostMatcher matcher;
    private long maxOffset = -1;

    private BestShiftDistance(final IDatasetMinCostMatcher matcher) {
        this.matcher = matcher;
    }

    /**
     * Creates a new instance using the given matcher for distance calculation.
     *
     * @param matcher
     *           the matcher to calculate the distance between two datasets;
     *           must not be {@code null}.
     * @return an instance of this class that uses the given matcher
     */
    public static BestShiftDistance from(final IDatasetMinCostMatcher matcher) {
        assert null != matcher;

        return new BestShiftDistance(matcher);
    }

    /**
     * Limits the offsets to try for finding the best shift to those within a
     * range of {@code maxOffset} from zero. Negative values mean that there is
     * no limit - all possible offsets will be tried.
     *
     * @param maxOffset
     *            the new offset range to use
     */
    public void setMaxOffset(final long maxOffset) {
        this.maxOffset = maxOffset;
    }

    @Override
    public double calculate(final Dataset original, final Dataset other) {
        assert null != original;
        assert null != other;

        double bestDistance = Double.MAX_VALUE;
        long bestOffset = 0;

        for (final long offset : this.getPossibleOffsets(original, other)) {
            final double distance =
                    this.calculateWithOffset(offset, original, other);
            if (distance < bestDistance) {
                bestDistance = distance;
                bestOffset = offset;
            }
        }

        System.out.println("Best offset: " + bestOffset);

        return bestDistance;
    }

    private double calculateWithOffset(final long offset,
                                       final Dataset original,
                                       final Dataset other) {
        original.setOffset(offset);
        return this.matcher.calculateMinimumCost(original, other);
    }

    /**
     * Each pair of intervals (p, q) from original x other creates four
     * candidates for the best offset. This method calculates all these
     * candidates.
     *
     * @param original
     *           the original dataset
     * @param other
     *           the dataset that is being compared to the original one
     * @return all values that might be the best offset, ordered ascendingly.
     */
    private List<Long> getPossibleOffsets(final Dataset original,
                                          final Dataset other) {
        final Set<Long> offsets = new HashSet<>();
        for (final Interval first : original.getIntervals()) {
            for (final Interval second: other.getIntervals()) {
                offsets.add(first.getStart() - second.getEnd());
                offsets.add(first.getEnd() - second.getEnd());
                offsets.add(first.getStart() - second.getStart());
                offsets.add(first.getEnd() - second.getStart());
            }
        }

        if (this.maxOffset > -1) {
            return offsets.stream()
                    .filter(v -> -this.maxOffset <= v && v <= this.maxOffset)
                    .sorted(Comparator.comparingLong(v -> Math.abs(0 - v)))
                    .collect(Collectors.toList());
        } else {
            return offsets.stream()
                    .sorted(Comparator.comparingLong(v -> Math.abs(0 - v)))
                    .collect(Collectors.toList());
        }
    }
}
