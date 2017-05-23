package net.meisen.master.meike.impl.distances.datasets.iterativeShift;

import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.IDatasetDistance;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.INeighborhood;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.IInitialOffsetCalculator;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.INextOffsetCalculator;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.logging.ILogger;
import net.meisen.master.meike.impl.logging.SilentLogger;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static java.util.stream.Stream.of;

/**
 * Implementation of the iterative search for the best shift value and matching.
 */
public class IterativeShiftDistance implements IDatasetDistance {
    private final IMinCostMapper mapper;
    private final IIntervalDistance distanceMeasure;
    private final IInitialOffsetCalculator initialOffsetCalculator;
    private final INextOffsetCalculator nextOffsetCalculator;
    private final INeighborhood neighborhood;
    private final ILogger logger;

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
        this.logger = new SilentLogger();
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

    @Override
    public Mapping calculate(final Dataset original, final Dataset other) {
        assert null != original;
        assert null != other;

        original.setOffset(0);
        other.setOffset(0);

        return this.initialOffsetCalculator.calculate(original, other).stream()
                .map(offset -> getBestMappingStartingFrom(offset, original, other))
                .sorted(Comparator.comparing(Pair::getValue))
                .map(Pair::getKey)
                .findFirst()
                .orElseThrow(() -> new IllegalStateException("Cannot happen"));
    }

    private Pair<Mapping, Double> getBestMappingStartingFrom(
            final long offset, final Dataset original, final Dataset other) {
        this.logger.log("ISD - Initial offset: \t" + offset);

        final Set<Long> alreadyUsedOffsets = new HashSet<>();
        List<Long> offsets = Arrays.asList(offset);
        Pair<Mapping, Double> bestMapping = new Pair<>(null, Double.MAX_VALUE);

        while (!offsets.isEmpty()) {
            alreadyUsedOffsets.addAll(offsets);
            final Pair<Mapping, Double> newMapping = offsets.stream()
                    .map(off -> this.getMappingForOffset(off, original, other))
                    .map(m -> new Pair<>(m, this.getCost(m)))
                    .sorted(Comparator.comparing(Pair::getValue))
                    .findFirst()
                    .orElseThrow(() -> new IllegalStateException("Cannot happen"));
            if (newMapping.getValue() < bestMapping.getValue()) {
                bestMapping = newMapping;
            }
            final List<Mapping> neighbors =
                    this.neighborhood.getNeighbors(newMapping.getKey(), original, other);
            offsets = Stream.concat(of(newMapping.getKey()), neighbors.stream())
                    .flatMap(m -> this.nextOffsetCalculator.calculate(m).stream())
                    .filter(o -> !alreadyUsedOffsets.contains(o))
                    .distinct()
                    .collect(Collectors.toList());
        }
        return bestMapping;
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
                .mapToDouble(opt -> opt.orElse(0.0)).sum();
    }
}
