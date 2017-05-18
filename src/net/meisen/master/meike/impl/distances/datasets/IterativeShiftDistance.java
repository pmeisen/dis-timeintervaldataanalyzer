package net.meisen.master.meike.impl.distances.datasets;

import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.logging.ILogger;
import net.meisen.master.meike.impl.logging.SilentLogger;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Implementation of the iterative search for the best shift value and matching.
 */
public class IterativeShiftDistance implements IDatasetDistance {
    private final IMinCostMapper mapper;
    private final IIntervalDistance distanceMeasure;
    private final ILogger logger;

    private IterativeShiftDistance(final IMinCostMapper mapper,
                                   final IIntervalDistance distanceMeasure) {
        this.mapper = mapper;
        this.distanceMeasure = distanceMeasure;
        this.logger = new SilentLogger();
    }

    /**
     * Creates a new instance of this class using the given mapper.
     *
     * @param mapper
     *          the mapper to use for mapping intervals from the original
     *          dataset to the intervals of the other dataset; must not be
     *          {@code null}.
     * @param distanceMeasure
     *          the distance measure used to calculate the distance between two
     *          intervals; must not be {@code null}.
     * @return a new instance of {@link IterativeShiftDistance}
     */
    public static IterativeShiftDistance from(
            final IMinCostMapper mapper,
            final IIntervalDistance distanceMeasure) {
        assert null != mapper;
        assert null != distanceMeasure;

        return new IterativeShiftDistance(mapper, distanceMeasure);
    }

    @Override
    public Mapping calculate(final Dataset original, final Dataset other) {
        assert null != original;
        assert null != other;

        original.setOffset(0);
        other.setOffset(0);

        long nextOffset = this.calculateInitialOffset(original, other);
        this.logger.log("ISD - Initial offset: \t" + nextOffset);
        long previousOffset;
        Mapping mapping;
        do {
            other.setOffset(nextOffset);
            final CostMatrix costMatrix =
                    new CostMatrix(this.distanceMeasure, original, other);
            mapping = this.mapper.calculateMinimumCostMapping(costMatrix).withOffset(nextOffset);
            previousOffset = nextOffset;
            nextOffset = this.calculateBestOffset(mapping);
        } while (nextOffset != previousOffset);
        return mapping;
    }

    /**
     * Aligns the "centroids" (means of all interval start and end points) of
     * the two datasets and calculates the value by which the {@code other}
     * dataset has to be shifted to achieve this alignment.
     *
     * @param original
     *          the original dataset
     * @param other
     *          a dataset that is being shifted and compared to the dataset
     * @return the value by which other has to be moved so that the two
     * datasets' "centroids" are aligned
     */
    private long calculateInitialOffset(final Dataset original,
                                        final Dataset other) {
        return Math.round(this.calculateCentroid(original) -
                this.calculateCentroid(other));
    }

    private double calculateCentroid(final Dataset dataset) {
        long sumOfIntervalBounds = dataset.getIntervals().stream()
                .mapToLong(interval -> interval.getStart() + interval.getEnd())
                .sum();
        long numberOfIntervals = dataset.getIntervals().size();
        return sumOfIntervalBounds / (2.0 * numberOfIntervals);
    }

    private long calculateBestOffset(final Mapping mapping) {
        final List<Pair<Interval, Interval>> pairs = mapping.getPairs();
        long bestOffset = 0;
        double minimumCost = Double.MAX_VALUE;
        for (final long offset : this.getPossibleOffsets(pairs)) {
            final double cost = this.calculateCost(pairs, offset);
            if (cost < minimumCost) {
                bestOffset = offset;
                minimumCost = cost;
            }
        }
        this.logger.log("ISD - Best offset: \t" + bestOffset);
        return bestOffset;
    }

    private double calculateCost(final List<Pair<Interval, Interval>> mapping,
                                 final long offset) {
        double cost = 0;
        for (final Pair<Interval, Interval> pair : mapping) {
            pair.getValue().setOffset(offset);
            cost += this.distanceMeasure.calculate(pair.getKey(), pair.getValue());
        }
        return cost;
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
        final Set<Long> offsets = new HashSet<>();

        for (final Pair<Interval, Interval> pair : mapping) {
            final Interval first = pair.getKey();
            final Interval second = pair.getValue();
            second.setOffset(0);
            offsets.add(first.getStart() - second.getEnd());
            offsets.add(first.getEnd() - second.getEnd());
            offsets.add(first.getStart() - second.getStart());
            offsets.add(first.getEnd() - second.getStart());
        }

        return offsets.stream()
                .sorted(Comparator.comparingLong(v -> Math.abs(0 - v)))
                .collect(Collectors.toSet());
    }
}
