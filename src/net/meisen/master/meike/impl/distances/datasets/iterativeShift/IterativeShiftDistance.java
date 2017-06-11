package net.meisen.master.meike.impl.distances.datasets.iterativeShift;

import com.google.common.collect.ImmutableList;
import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.IDatasetDistance;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.INeighborhood;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.IInitialOffsetCalculator;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.INextOffsetCalculator;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.MinCostOffset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.logging.ILogger;
import net.meisen.master.meike.impl.logging.MappingLogger;
import net.meisen.master.meike.impl.logging.SimpleConsoleLogger;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;


/**
 * Implementation of the iterative search for the best shift value and matching.
 */
public class IterativeShiftDistance implements IDatasetDistance {
    private final IMinCostMapper mapper;
    private final IIntervalDistance distanceMeasure;
    private final IInitialOffsetCalculator initialOffsetCalculator;
    private final INextOffsetCalculator nextOffsetCalculator;
    private final INeighborhood neighborhood;
    private final MinCostOffset minCostOffset;
    private final ILogger logger;
    private MappingLogger mappingLogger = MappingLogger.createFor(0, "01.01.2017", "wrong");

    private IterativeShiftDistance(final IMinCostMapper mapper,
                                   final IIntervalDistance distanceMeasure,
                                   final IInitialOffsetCalculator initialOffsetCalculator,
                                   final INextOffsetCalculator nextOffsetCalculator,
                                   final INeighborhood neighborhood) {
        this.mapper = mapper;
        this.distanceMeasure = distanceMeasure;
        this.initialOffsetCalculator = initialOffsetCalculator;
        this.nextOffsetCalculator = nextOffsetCalculator;
        this.neighborhood = neighborhood;
        this.minCostOffset = MinCostOffset.fromIntervalDistance(distanceMeasure);
        this.logger = new SimpleConsoleLogger();
        this.mappingLogger.enabled = false;
    }

    /**
     * Creates a new instance of this class using the given mapper.
     *
     * @param mapper
     *          the mapper used for mapping intervals from the original
     *          dataset to the intervals of the other dataset given an offset;
     *          must not be {@code null}.
     * @param distanceMeasure
     *          the distance measure used to calculate the distance between two
     *          intervals when creating a cost matrix; must not be {@code null}.
     * @param initialOffsetCalculator
     *          used for calculating the initial offsets to start with
     * @param nextOffsetCalculator
     *          used for calculating the next offset when a mapping is already
     *          given
     * @param neighborhood
     *          used for calculating a neighborhood of a mapping, that will be
     *          searched for better solutions
     * @return a new instance of {@link IterativeShiftDistance}
     */
    public static IterativeShiftDistance from(
            final IMinCostMapper mapper,
            final IIntervalDistance distanceMeasure,
            final IInitialOffsetCalculator initialOffsetCalculator,
            final INextOffsetCalculator nextOffsetCalculator,
            final INeighborhood neighborhood) {
        assert null != mapper;
        assert null != distanceMeasure;
        assert null != initialOffsetCalculator;
        assert null != nextOffsetCalculator;
        assert null != neighborhood;

        return new IterativeShiftDistance(mapper, distanceMeasure,
                initialOffsetCalculator, nextOffsetCalculator, neighborhood);
    }

    public void setMappingLogger(final MappingLogger mLogger) {
        this.mappingLogger = mLogger;
    }

    @Override
    public Mapping calculate(final Dataset original, final Dataset other) {
        assert null != original;
        assert null != other;

        original.setOffset(0);
        other.setOffset(0);

        return this.getBestMappingStartingFrom(
                this.initialOffsetCalculator.calculate(original, other).stream()
                        .distinct()
                        .collect(Collectors.toList()),
                original, other).getKey();
    }

    private Pair<Mapping, Double> getBestMappingStartingFrom(
            List<Long> offsets, final Dataset original, final Dataset other) {
        final List<Long> allowedOffsets = this.getPossibleOffsets(original, other);
        final Set<Long> alreadyUsedOffsets = new HashSet<>();
        Pair<Mapping, Double> bestMapping = new Pair<>(null, Double.MAX_VALUE);

        offsets = offsets.stream()
                .flatMap(o -> this.getClosest(allowedOffsets, o).stream())
                .distinct()
                .collect(Collectors.toList());
        mappingLogger.log("Initial offsets:");
        mappingLogger.log(offsets);

        while (!offsets.isEmpty()) {
            alreadyUsedOffsets.addAll(offsets);
            final List<Mapping> newMappings = offsets.stream()
                    .map(off -> this.getMappingForOffset(off, original, other))
                    .collect(Collectors.toList());
            mappingLogger.log("New mappings:");
            for (final Mapping mapping : newMappings) {
                mappingLogger.log(mapping);
            }

            final Pair<Mapping, Double> bestNewMapping = newMappings.stream()
                    .map(m -> new Pair<>(m, this.getCost(m)))
                    .sorted(Comparator.comparing(Pair::getValue))
                    .findFirst()
                    .orElseThrow(() -> new IllegalStateException("Cannot happen"));
            if (bestNewMapping.getValue() < bestMapping.getValue()) {
                bestMapping = bestNewMapping;
                mappingLogger.log("Best new mapping (" + bestNewMapping.getValue() + "):");
                mappingLogger.log(bestNewMapping.getKey());
            } else {
                break;
            }

            final List<Mapping> neighbors =
                    this.neighborhood.getNeighbors(bestNewMapping.getKey(), original, other);
            mappingLogger.log("Neighbors:");
            for (final Mapping mapping : neighbors) {
                mappingLogger.log(mapping);
            }
            offsets = Stream.concat(Stream.of(bestNewMapping.getKey()), neighbors.stream().distinct())
                    .flatMap(m -> this.nextOffsetCalculator.calculate(m).stream())
                    .flatMap(o -> this.getClosest(allowedOffsets, o).stream())
                    .distinct()
                    .filter(o -> !alreadyUsedOffsets.contains(o))
                    .collect(Collectors.toList());
            mappingLogger.log(offsets);
        }
        bestMapping.getKey().withOffset(this.getBestOffset(bestMapping.getKey()));
        return bestMapping;
    }

    private long getBestOffset(final Mapping mapping) {
        return this.minCostOffset.calculate(mapping).get(0);
    }

    private Mapping getMappingForOffset(final long offset, final Dataset original, final Dataset other) {
        other.setOffset(offset);
        final CostMatrix costMatrix =
                new CostMatrix(this.distanceMeasure, original, other);
        return this.mapper.calculateMinimumCostMapping(costMatrix)
                .withOffset(offset);
    }

    private double getCost(final Mapping mapping) {
        return mapping.getMappingCosts().stream()
                .mapToDouble(opt -> opt.orElse(1.0)).sum();
    }

    private List<Long> getClosest(final List<Long> candidates, final long target) {
        for (int i = 0; i < candidates.size(); i++) {
            if (candidates.get(i) >= target) {
                if (i > 0) {
                    return ImmutableList.of(candidates.get(i - 1), candidates.get(i));
                } else {
                    return ImmutableList.of(candidates.get(i));
                }
            }
        }
        return ImmutableList.of(candidates.get(candidates.size() - 1));
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
     * @return all values that might be the best offset, sorted in ascending order.
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

        return offsets.stream()
                .sorted()
                .collect(Collectors.toList());
    }
}
