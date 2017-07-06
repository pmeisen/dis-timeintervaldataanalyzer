package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.Mapping;
import org.assertj.core.util.VisibleForTesting;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Calculates initial offset candidates by considering some monotone
 * {@link Mapping}s between the {@link Interval}s of the given {@link Dataset}s:
 * For each such monotone mapping, this returns the offset value for which the
 * interval distance between the pairs defined by the mapping is minimized.
 */
public class MonotoneMapping implements IInitialOffsetCalculator {
    /**
     * Defines how many intervals of the shorter {@link Dataset} may be unmapped.
     */
    private final int maxOverShift;
    private final IIntervalDistance intervalDistance;

    private MonotoneMapping(final IIntervalDistance intervalDistance, final int maxOverShift) {
        this.intervalDistance = intervalDistance;
        this.maxOverShift = maxOverShift;
    }

    public static MonotoneMapping fromDistance(
            final IIntervalDistance intervalDistance, final int maxOverShift) {
        return new MonotoneMapping(intervalDistance, maxOverShift);
    }

    @Override
    public List<Long> calculate(final Dataset original, final Dataset other) {
        final List<Long> possibleOffsets = Utils.getPossibleOffsets(original, other);

        return this.getIntervalPairLists(original, other).stream()
                .map(pairList -> this.getBestOffset(pairList, possibleOffsets))
                .distinct()
                .collect(Collectors.toList());
    }

    private List<List<Pair<Interval, Interval>>> getIntervalPairLists(
            final Dataset original, final Dataset other) {
        final Interval[] originalIntervals = original.getIntervals().toArray(new Interval[0]);
        final Interval[] otherIntervals = other.getIntervals().toArray(new Interval[0]);

        return getIndexPairs(originalIntervals.length, otherIntervals.length).stream()
                .map(pairs -> this.getIntervalPairs(originalIntervals, otherIntervals, pairs))
                .collect(Collectors.toList());
    }

    private List<Pair<Interval, Interval>> getIntervalPairs(
            final Interval[] original, final Interval[] other,
            final List<Pair<Integer, Integer>> indexPairs) {
        return indexPairs.stream()
                .map(pair -> new Pair<>(original[pair.getKey()], other[pair.getValue()]))
                .collect(Collectors.toList());
    }

    @VisibleForTesting
    List<List<Pair<Integer, Integer>>> getIndexPairs(final int firstLength, final int secondLength) {
        final List<List<Pair<Integer, Integer>>> result = new LinkedList<>();
        for (int i = Math.max(0, secondLength - firstLength) + maxOverShift; i >= 0; i--) {
            final List<Pair<Integer, Integer>> newList = new LinkedList<>();
            for (int j = 0; j < firstLength && i + j < secondLength; j++) {
                newList.add(new Pair<>(j, j+i));
            }
            result.add(newList);
        }
        for (int i = 1; i <= Math.max(0, firstLength - secondLength) + maxOverShift; i++) {
            final List<Pair<Integer, Integer>> newList = new LinkedList<>();
            for (int j = 0; j < secondLength && i + j < firstLength; j++) {
                newList.add(new Pair<>(j+i, j));
            }
            result.add(newList);
        }
        return result;
    }

    private Long getBestOffset(final List<Pair<Interval, Interval>> pairs, List<Long> offsets) {
        return offsets.stream()
                .min(Comparator.comparing(offset -> getCost(pairs, offset)))
                .orElse(0L);
    }

    private double getCost(final List<Pair<Interval, Interval>> pairs, long offset) {
        return pairs.stream().mapToDouble(pair -> {
            pair.getValue().setOffset(offset);
            return intervalDistance.calculate(pair.getKey(), pair.getValue());
        }).sum();
    }
}
