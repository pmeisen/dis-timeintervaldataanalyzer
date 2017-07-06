package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Calculates initial offset candidates by picking some monotone mapping.
 */
public class MonotoneMapping implements IInitialOffsetCalculator {
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
        final List<Long> chosenOffsets = new LinkedList<>();

        for (final List<Pair<Integer, Integer>> indexPairs : getIndexPairs(original.getNumberOfIntervals(), other.getNumberOfIntervals())) {
            chosenOffsets.add(getBestOffset(indexPairs.stream()
                    .map(pair -> new Pair<>(original.getIntervals().get(pair.getKey()), other.getIntervals().get(pair.getValue())))
                    .collect(Collectors.toList()), possibleOffsets));
        }

        return chosenOffsets.stream().distinct().collect(Collectors.toList());
    }

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
