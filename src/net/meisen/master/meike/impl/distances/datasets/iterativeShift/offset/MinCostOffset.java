package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import com.google.common.collect.ImmutableList;
import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Calculates an offset that yields some kind of minimal cost: For the initial
 * offset this means to sort all intervals by their centroids, zipping them and
 * returning the offset value for which the sum of the cost of those pairs is
 * minimized. For a next offset starting from a given {@link Mapping} this means
 * returning the offset value for which the sum of the cost of the pairs defined
 * by the mapping is minimized.
 */
public class MinCostOffset implements IInitialOffsetCalculator, INextOffsetCalculator {
    private final IIntervalDistance intervalDistance;

    private MinCostOffset(final IIntervalDistance intervalDistance) {
        this.intervalDistance = intervalDistance;
    }

    public static MinCostOffset fromIntervalDistance(final IIntervalDistance intervalDistance) {
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
        return Utils.getPossibleOffsets(pairs).stream()
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

    private List<Pair<Interval, Interval>> zip(final List<Interval> as,
                                               final List<Interval> bs) {
        return IntStream.range(0, Math.min(as.size(), bs.size()))
                .mapToObj(i -> new Pair<>(as.get(i), bs.get(i)))
                .collect(Collectors.toList());
    }
}
