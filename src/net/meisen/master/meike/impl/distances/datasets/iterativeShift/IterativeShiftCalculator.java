package net.meisen.master.meike.impl.distances.datasets.iterativeShift;

import com.google.common.collect.ImmutableList;
import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.IDatasetDistanceCalculator;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.INeighborhood;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.IInitialOffsetCalculator;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.INextOffsetCalculator;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.MinCostOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.Utils;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.logging.MappingLogger;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;

import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;


/**
 * Implementation of the iterative search for the best shift value and matching.
 */
public class IterativeShiftCalculator implements IDatasetDistanceCalculator {
    private final IMinCostMapper mapper;
    private final ICostCalculator costCalculator;
    private final IIntervalDistance distanceMeasure;
    private final INextOffsetCalculator nextOffsetCalculator;
    private final INeighborhood neighborhood;
    private final MinCostOffset minCostOffset;
    private final Dataset originalDataset;
    private final Dataset candidateDataset;
    private final List<Long> allowedOffsets;
    private final Set<Long> alreadyUsedOffsets = new HashSet<>();
    private final List<Long> nextOffsetCandidates;
    private Pair<Mapping, Double> bestMapping = new Pair<>(null, Double.MAX_VALUE);
    private int numberOfKuhnMunkres = 0;

    private final MappingLogger mappingLogger;

    private IterativeShiftCalculator(final Dataset originalDataset,
                                     final Dataset candidateDataset,
                                     final IMinCostMapper mapper,
                                     final ICostCalculator costCalculator,
                                     final IIntervalDistance distanceMeasure,
                                     final IInitialOffsetCalculator initialOffsetCalculator,
                                     final INextOffsetCalculator nextOffsetCalculator,
                                     final INeighborhood neighborhood,
                                     final MappingLogger mappingLogger) {
        this.originalDataset = originalDataset;
        this.candidateDataset = candidateDataset;
        this.mapper = mapper;
        this.costCalculator = costCalculator;
        this.distanceMeasure = distanceMeasure;
        this.nextOffsetCalculator = nextOffsetCalculator;
        this.neighborhood = neighborhood;
        this.minCostOffset = MinCostOffset.fromIntervalDistance(distanceMeasure);
        this.mappingLogger = mappingLogger;
        this.allowedOffsets = this.getPossibleOffsets();
        this.nextOffsetCandidates = initialOffsetCalculator
                .calculate(originalDataset, candidateDataset)
                .stream()
                .distinct()
                .collect(Collectors.toList());
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
     * @return a new instance of {@link IterativeShiftCalculator}
     */
    static IterativeShiftCalculator createFor(
            final Dataset originalDataset,
            final Dataset candidateDataset,
            final IMinCostMapper mapper,
            final ICostCalculator costCalculator,
            final IIntervalDistance distanceMeasure,
            final IInitialOffsetCalculator initialOffsetCalculator,
            final INextOffsetCalculator nextOffsetCalculator,
            final INeighborhood neighborhood,
            final MappingLogger mappingLogger) {
        assert null != originalDataset;
        assert null != candidateDataset;
        assert null != mapper;
        assert null != costCalculator;
        assert null != distanceMeasure;
        assert null != initialOffsetCalculator;
        assert null != nextOffsetCalculator;
        assert null != neighborhood;
        assert null != mappingLogger;

        return new IterativeShiftCalculator(originalDataset, candidateDataset,
                mapper, costCalculator, distanceMeasure, initialOffsetCalculator,
                nextOffsetCalculator, neighborhood, mappingLogger);
    }

    private class IterationResult {
        public final Mapping nextMapping;
        public final double costOfNextMapping;
        public final List<Long> nextOffsets;

        public IterationResult(final Mapping nextMapping,
                               final double costOfNextMapping,
                               final List<Long> nextOffsets) {
            this.nextMapping = nextMapping;
            this.costOfNextMapping = costOfNextMapping;
            this.nextOffsets = nextOffsets;
        }
    }

    @Override
    public Optional<Mapping> nextMapping() {
        if (this.nextOffsetCandidates.isEmpty()) {
            return Optional.empty();
        } else {
            this.alreadyUsedOffsets.addAll(this.nextOffsetCandidates);
            this.candidateDataset.setOffset(0);

            final IterationResult iterationResult =
                    this.getBestMappingForOffsets(this.nextOffsetCandidates);
            if (iterationResult.costOfNextMapping < this.bestMapping.getValue()) {
                this.bestMapping = new Pair<>(iterationResult.nextMapping, iterationResult.costOfNextMapping);
            }

            this.nextOffsetCandidates.clear();
            this.nextOffsetCandidates.addAll(iterationResult.nextOffsets);

            mappingLogger.log(iterationResult.nextMapping);
            return Optional.of(iterationResult.nextMapping);
        }
    }

    @Override
    public Mapping finalMapping() {
        Optional<Mapping> mapping = this.nextMapping();
        while (mapping.isPresent()) {
            mapping = this.nextMapping();
        }

        this.mappingLogger.log("Offsets: \t" + this.alreadyUsedOffsets.size()
                + "\t/\t" + this.allowedOffsets.size() + "\t\t"
                + (this.alreadyUsedOffsets.size() * 100.0 / this.allowedOffsets.size()));

        final Mapping result = this.getMappingForOffset(this.bestMapping.getKey().getOffset());
        mappingLogger.log(result);
        return result;
    }

    public class MappingStatistics {
        public final int initialOffsets;
        public final int usedOffsets;
        public final int possibleOffsets;

        public MappingStatistics(final int initialOffsets, final int usedOffsets, final int possibleOffsets) {
            this.initialOffsets = initialOffsets;
            this.usedOffsets = usedOffsets;
            this.possibleOffsets = possibleOffsets;
        }
    }

    /**
     * Returns more details than the usual method.
     */
    public Pair<Mapping, MappingStatistics> getFinalMapping() {
        final int initialOffsetsSize = this.nextOffsetCandidates.size();

        final List<Long> allStartOffsets = new LinkedList<>(this.nextOffsetCandidates);

        Optional<Mapping> mapping = this.nextMapping();
        while (mapping.isPresent()) {
            mapping = this.nextMapping();
        }

        final Mapping result = this.getMappingForOffset(this.bestMapping.getKey().getOffset());
        mappingLogger.log(result);
        return new Pair<>(result, new MappingStatistics(initialOffsetsSize, this.numberOfKuhnMunkres, this.allowedOffsets.size()));
    }

    private IterationResult getBestMappingForOffsets(final List<Long> initialOffsets) {
        List<Long> offsets = new LinkedList<>(initialOffsets);

        final Mapping bestOffsetMapping = offsets.stream()
                .map(this::getMappingForOffset)
                .map(m -> new Pair<>(m, this.getCost(m)))
                .min(Comparator.comparing(Pair::getValue))
                .map(Pair::getKey)
                .orElseThrow(() -> new IllegalStateException("Cannot happen"));

        final List<Mapping> permutations = this.neighborhood.getNeighbors(
                bestOffsetMapping, this.originalDataset, this.candidateDataset);

        final Pair<Mapping, Double> bestPermutation = Stream
                .concat(Stream.of(bestOffsetMapping), permutations.stream())
                .distinct()
                .map(m -> new Pair<>(m, this.getCostSafely(m)))
                .min(Comparator.comparing(Pair::getValue))
                .orElseThrow(() -> new IllegalStateException("Cannot happen"));

        // Todo: Neighborhood of best permutation!

        final List<Long> nextOffsets = this.nextOffsetCalculator.calculate(bestPermutation.getKey())
                .stream()
                .flatMap(o -> this.getClosest(o).stream())
                .distinct()
                .filter(o -> !alreadyUsedOffsets.contains(o))
                .collect(Collectors.toList());
        if (!alreadyUsedOffsets.contains(bestPermutation.getKey().getOffset())) {
            nextOffsets.add(bestPermutation.getKey().getOffset());
        }

        bestPermutation.getKey().withOffset(this.getBestOffset(bestPermutation.getKey()));
        return new IterationResult(bestPermutation.getKey(), bestPermutation.getValue(), nextOffsets);
    }

    private long getBestOffset(final Mapping mapping) {
        return this.minCostOffset.calculate(mapping).get(0);
    }

    private double getCostSafely(final Mapping mapping) {
        return mapping.getPairs().stream()
                .mapToDouble(pair -> this.distanceMeasure.calculate(pair.getKey(), pair.getValue()))
                .sum() + mapping.getUnmappedIntervalsOfLargerDataset().size();
    }

    private Mapping getMappingForOffset(final long offset) {
        this.candidateDataset.setOffset(offset);
        this.alreadyUsedOffsets.add(offset);
        final CostMatrix costMatrix = new CostMatrix(
                this.distanceMeasure, this.originalDataset, this.candidateDataset);
        this.numberOfKuhnMunkres++;
        return this.mapper.calculateMinimumCostMapping(costMatrix)
                .withOffset(offset);
    }

    private double getCost(final Mapping mapping) {
        return this.costCalculator.calculateCost(mapping);
    }

    private List<Long> getClosest(final long target) {
        for (int i = 0; i < this.allowedOffsets.size(); i++) {
            if (this.allowedOffsets.get(i) >= target) {
                if (i > 0) {
                    return ImmutableList.of(this.allowedOffsets.get(i - 1),
                            this.allowedOffsets.get(i));
                } else {
                    return ImmutableList.of(this.allowedOffsets.get(i));
                }
            }
        }
        return ImmutableList.of(this.allowedOffsets.get(this.allowedOffsets.size() - 1));
    }

    /**
     * Each pair of intervals (p, q) from original x other creates four
     * candidates for the best offset. This method calculates all these
     * candidates.
     *
     * @return all values that might be the best offset, sorted in ascending order.
     */
    private List<Long> getPossibleOffsets() {
        return Utils.getPossibleOffsets(this.originalDataset, this.candidateDataset);
    }
}
