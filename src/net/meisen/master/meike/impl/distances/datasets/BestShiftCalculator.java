package net.meisen.master.meike.impl.distances.datasets;

import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.Utils;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Queue;
import java.util.stream.Collectors;

/**
 * Allows calculating the distance between two {@link Dataset}s considering
 * the possibility to shift all {@link Interval}s of one of them by a constant
 * offset. The algorithm considers all 4n^2 possible offset values in order of
 * increasing absolute value; if the application allows limiting the range of
 * possible offsets, a maximum valid offset can be configured beforehand.
 */
public class BestShiftCalculator implements IDatasetDistanceCalculator {
    private final IMinCostMapper mapper;
    private final IIntervalDistance intervalDistance;
    private final ICostCalculator costCalculator;
    private final Dataset originalDataset;
    private final Dataset candidateDataset;
    public final Queue<Long> possibleOffsets;
    private Pair<Mapping, Double> bestMapping = new Pair<>(null, Double.MAX_VALUE);

    private BestShiftCalculator(final Dataset originalDataset,
                                final Dataset candidateDataset,
                                final IMinCostMapper mapper,
                                final IIntervalDistance intervalDistance,
                                final ICostCalculator costCalculator,
                                final long maxOffset) {
        this.mapper = mapper;
        this.intervalDistance = intervalDistance;
        this.costCalculator = costCalculator;
        this.originalDataset = originalDataset;
        this.candidateDataset = candidateDataset;
        this.possibleOffsets = this.getPossibleOffsets(maxOffset);
    }

    /**
     * Creates a new instance using the given mapper for distance calculation.
     *
     * @param mapper
     *          the mapper to calculate the distance between two datasets;
     *          must not be {@code null}.
     * @param intervalDistance
     *          a distance measure for pairs of intervals; must not be
     *          {@code null}.
     * @param costCalculator
     *          a cost calculator for determining the cost of a mapping
     * @return an instance of this class that uses the given mapper and distance
     */
    static BestShiftCalculator createFor(final Dataset originalDataset,
                                         final Dataset candidateDataset,
                                         final IMinCostMapper mapper,
                                         final IIntervalDistance intervalDistance,
                                         final ICostCalculator costCalculator,
                                         final long maxOffset) {
        assert null != originalDataset;
        assert null != candidateDataset;
        assert null != mapper;
        assert null != intervalDistance;
        assert null != costCalculator;

        return new BestShiftCalculator(originalDataset, candidateDataset,
                mapper, intervalDistance, costCalculator, maxOffset);
    }

    @Override
    public Optional<Mapping> nextMapping() {
        if (this.possibleOffsets.isEmpty()) {
            return Optional.empty();
        } else {
            final long offset = this.possibleOffsets.remove();
            final Mapping mapping = this.calculateWithOffset(offset);
            return Optional.of(mapping);
        }
    }

    @Override
    public Mapping finalMapping() {
        Optional<Mapping> mapping = this.nextMapping();
        while (mapping.isPresent()) {
            final double cost = this.costCalculator.calculateCost(mapping.get());
            if (cost < this.bestMapping.getValue()) {
                this.bestMapping = new Pair<>(mapping.get(), cost);
            }
            mapping = this.nextMapping();
        }
        return this.bestMapping.getKey();
    }

    private Mapping calculateWithOffset(final long offset) {
        this.candidateDataset.setOffset(offset);
        final CostMatrix costMatrix = new CostMatrix(
                this.intervalDistance, this.originalDataset, this.candidateDataset);
        return this.mapper.calculateMinimumCostMapping(costMatrix).withOffset(offset);
    }

    /**
     * Each pair of intervals (p, q) from original x other creates four
     * candidates for the best offset. This method calculates all these
     * candidates.
     *
     * @return all values that might be the best offset, sorted in ascending order.
     */
    private Queue<Long> getPossibleOffsets(final long maxOffset) {
        this.candidateDataset.setOffset(0);
        final List<Long> offsets = Utils.getPossibleOffsets(
                this.originalDataset, this.candidateDataset);

        if (BestShiftFactory.UNLIMITED_OFFSET == maxOffset) {
            return new LinkedList<>(offsets.stream()
                    .sorted(Comparator.comparingLong(v -> Math.abs(0 - v)))
                    .collect(Collectors.toList()));
        } else {
            return new LinkedList<>(offsets.stream()
                    .filter(v -> Math.abs(v) <= maxOffset)
                    .sorted(Comparator.comparingLong(v -> Math.abs(0 - v)))
                    .collect(Collectors.toList()));
        }
    }
}
