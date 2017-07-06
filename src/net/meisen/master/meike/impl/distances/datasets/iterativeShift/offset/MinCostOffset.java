package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import com.google.common.collect.ImmutableList;
import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.LongStream;

/**
 * Calculates an offset that yields some kind of minimal cost.
 */
public class MinCostOffset implements IInitialOffsetCalculator,
        INextOffsetCalculator {
    private final IIntervalDistance intervalDistance;

    private MinCostOffset(final IIntervalDistance intervalDistance) {
        this.intervalDistance = intervalDistance;
    }

    public static MinCostOffset fromIntervalDistance(
            final IIntervalDistance intervalDistance) {
        return new MinCostOffset(intervalDistance);
    }

    @Override
    public List<Long> calculate(final Dataset original, final Dataset other) {
        final List<Interval> first = original.getIntervals().stream()
                .sorted(Comparator.comparing(Interval::getCentroid))
                .collect(Collectors.toList());
        final List<Interval> second = other.getIntervals().stream()
                .sorted(Comparator.comparing(Interval::getCentroid))
                .collect(Collectors.toList());
        return ImmutableList.of(this.calculate(this.zip(first, second)));
    }

    @Override
    public List<Long> calculate(final Mapping mapping) {
        return ImmutableList.of(this.calculate(mapping.getPairs()));
    }

    private long calculate(final List<Pair<Interval, Interval>> pairs) {
        return this.getPossibleOffsets(pairs).stream()
                .min(Comparator.comparingDouble(offset ->
                        this.calculateCost(pairs, offset)))
                .orElse(0L);
    }

    private double calculateCost(final List<Pair<Interval, Interval>> mapping,
                                 final long offset) {
        return mapping.stream().mapToDouble(pair -> {
            pair.getValue().setOffset(offset);
            return this.intervalDistance.calculate(pair.getKey(), pair.getValue());
        }).sum();
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
    private Set<Long> getPossibleOffsets(
            final List<Pair<Interval, Interval>> mapping) {
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
                .collect(Collectors.toSet());
    }

    private List<Pair<Interval, Interval>> zip(final List<Interval> as,
                                               final List<Interval> bs) {
        return IntStream.range(0, Math.min(as.size(), bs.size()))
                .mapToObj(i -> new Pair<>(as.get(i), bs.get(i)))
                .collect(Collectors.toList());
    }
}
