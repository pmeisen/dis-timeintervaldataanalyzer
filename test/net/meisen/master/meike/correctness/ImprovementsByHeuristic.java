package net.meisen.master.meike.correctness;

import com.google.common.collect.ImmutableList;
import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.BestShiftFactory;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.ICalculatorFactory;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.IterativeShiftCalculator.MappingStatistics;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.IterativeShiftFactory;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.CombinedNeighborhood;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.EmptyNeighborhood;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.INeighborhood;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.LocalPerturbation;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.ModifiedDistances;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.CentroidOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.CombinedInitial;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.CombinedNext;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.MonotoneMapping;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.IInitialOffsetCalculator;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.INextOffsetCalculator;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.LengthOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.MedianOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.MinCostOffset;
import net.meisen.master.meike.impl.distances.intervals.Factories;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.costCalculation.ConstantCostForUnmappedIntervals;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;
import org.junit.Test;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Tests for evaluating which heuristics help how much for the Sascha data.
 */
public class ImprovementsByHeuristic extends SaschaBasedTest {
    private final List<Integer> modelNumbersToTest = ImmutableList.of(1, 2, 3, 4, 5, 6, 7, 8);

    private final IIntervalDistance intervalDistance = Factories.weightedDistance(1, 1, 1, 1, 1);

    private final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(0);
    private final IMinCostMapper mapper = KuhnMunkres.create();

    private final IInitialOffsetCalculator completeInitialOffsetCalculator = CombinedInitial.from(ImmutableList.of(
            new CentroidOffset(),
            MonotoneMapping.fromDistance(Factories.weightedDistance(1, 1, 1, 0, 0), 5),
            LengthOffset.from(mapper, MinCostOffset.fromIntervalDistance(intervalDistance))));

    private final INeighborhood defaultNeighborhood = new EmptyNeighborhood();
    private final INextOffsetCalculator defaultNextOffsetCalculator = MinCostOffset.fromIntervalDistance(intervalDistance);
    private final IInitialOffsetCalculator defaultInitialOffsetCalculator = new CentroidOffset();

    private final BestShiftFactory bestShiftFactory = BestShiftFactory.from(mapper, intervalDistance, costCalculator);
    private final IterativeShiftFactory iterativeShiftFactory = this.createIterativeShiftFactory(defaultNextOffsetCalculator, defaultNeighborhood);

    @Test
    public void testImprovementsByDifferentMedians() {
        final List<Pair<String, IterativeShiftFactory>> heuristics = ImmutableList.of(
                new Pair<>("Standard", this.createMedianFactory(true, false, false, false, false)),
                new Pair<>("Largest", this.createMedianFactory(false, true, false, false, false)),
                new Pair<>("Smallest", this.createMedianFactory(false, false, true, false, false)),
                new Pair<>("Most Extreme", this.createMedianFactory(false, false, false, true, false)),
                new Pair<>("Least Extreme", this.createMedianFactory(false, false, false, false, true)),
                new Pair<>("Smallest & Most Extreme", this.createMedianFactory(true, false, false, true, false)),
                new Pair<>("All", this.createMedianFactory(true, true, true, true, true)));
        this.calculateImprovements(heuristics);
    }

    @Test
    public void testImprovementsByDifferentLengthModifications() {
        final List<Pair<String, IterativeShiftFactory>> heuristics = ImmutableList.of(
                new Pair<>("Length Four", this.createLengthModificationFactory(
                        Factories.weightedDistance(1, 1, 4, 0, 0))),
                new Pair<>("Length Ten", this.createLengthModificationFactory(
                        Factories.weightedDistance(1, 1, 10, 0, 0))),
                new Pair<>("Zero Length", this.createLengthModificationFactory(
                        Factories.weightedDistance(1, 1, 0, 0, 0))),
                new Pair<>("Combined", this.createLengthModificationFactory(
                        Factories.weightedDistance(1, 1, 4, 0, 0),
                        Factories.weightedDistance(1, 1, 10, 0, 0),
                        Factories.weightedDistance(1, 1, 0, 0, 0))));
        this.calculateImprovements(heuristics);
    }

    @Test
    public void testImprovementsByMediansAndLengthModifications() {
        final INextOffsetCalculator nextOffsetCalculator = MedianOffset.usingAll();

        final INeighborhood fourNeighborhood = ModifiedDistances.using(
                ImmutableList.of(
                        Factories.weightedDistance(1, 1, 4, 0, 0)),
                mapper);

        final INeighborhood combinedNeighborhood = ModifiedDistances.using(
                ImmutableList.of(
                        Factories.weightedDistance(1, 1, 4, 0, 0),
                        Factories.weightedDistance(1, 1, 10, 0, 0),
                        Factories.weightedDistance(1, 1, 0, 0, 0)),
                mapper);

        final List<Pair<String, IterativeShiftFactory>> heuristics = ImmutableList.of(
                new Pair<>("Only Median", this.createIterativeShiftFactory(nextOffsetCalculator, defaultNeighborhood)),
                new Pair<>("With Length Four", this.createIterativeShiftFactory(nextOffsetCalculator, fourNeighborhood)),
                new Pair<>("With Combined Length", this.createIterativeShiftFactory(nextOffsetCalculator, combinedNeighborhood)));

        this.calculateImprovements(heuristics);
    }

    @Test
    public void testInfluenceOfMonotoneMappingHeuristic() {
        final IInitialOffsetCalculator monotoneMapping = MonotoneMapping.fromDistance(intervalDistance, 5);

        final INextOffsetCalculator allMediansNextOffset = MedianOffset.usingAll();
        final INextOffsetCalculator standardMedianNextOffset =
                MedianOffset.including(true, false, false, false, false);
        final INeighborhood lengthModifications = ModifiedDistances.using(
                ImmutableList.of(
                        Factories.weightedDistance(1, 1, 4, 0, 0),
                        Factories.weightedDistance(5, 5, 1, 1, 1)),
                mapper);
        final INeighborhood localPerturbationNeighborhood = new LocalPerturbation();

        final List<Pair<String, IterativeShiftFactory>> heuristics = ImmutableList.of(
                new Pair<>("Only Monotone Mappings", this.createIterativeShiftFactory(
                        monotoneMapping, defaultNextOffsetCalculator, defaultNeighborhood)),
                new Pair<>("With Medians", this.createIterativeShiftFactory(
                        monotoneMapping,
                        CombinedNext.from(ImmutableList.of(defaultNextOffsetCalculator, allMediansNextOffset)),
                        defaultNeighborhood)),
                new Pair<>("With Medians & Length", this.createIterativeShiftFactory(
                        monotoneMapping,
                        CombinedNext.from(ImmutableList.of(defaultNextOffsetCalculator, allMediansNextOffset)),
                        lengthModifications)),
                new Pair<>("With Local Perturbation", this.createIterativeShiftFactory(
                        monotoneMapping,
                        defaultNextOffsetCalculator,
                        localPerturbationNeighborhood)),
                new Pair<>("Everything", this.createIterativeShiftFactory(
                        CombinedInitial.from(ImmutableList.of(new CentroidOffset(), monotoneMapping)),
                        CombinedNext.from(ImmutableList.of(defaultNextOffsetCalculator, allMediansNextOffset)),
                        CombinedNeighborhood.from(ImmutableList.of(lengthModifications, localPerturbationNeighborhood)))));

        this.calculateImprovements(heuristics);
    }

    private class Result {
        public final String id;
        public final double bestCost;
        public final List<Double> gaps;
        public final List<MappingStatistics> mappingStatistics;

        public Result(final String id, final double bestCost, final List<Double> gaps, final List<MappingStatistics> mappingStatistics) {
            this.id = id;
            this.bestCost = bestCost;
            this.gaps = gaps;
            this.mappingStatistics = mappingStatistics;
        }
    }

    private void calculateImprovements(final List<Pair<String, IterativeShiftFactory>> heuristics) {
        for (final int modelNumber : modelNumbersToTest) {
            this.logHeader(modelNumber, heuristics);

            final List<IterativeShiftFactory> factories = heuristics.stream().map(Pair::getValue).collect(Collectors.toList());

            final Datasets datasets = this.loadDatasets(modelNumber, allCandidateDates.get(modelNumber - 1));
            datasets.candidates.stream()
                    .map(candidate -> this.getResultsForCandidate(candidate, datasets.original, factories))
                    .sorted(Comparator.comparing(result -> result.bestCost))
                    .forEach(this::logResult);
            this.unload();
        }
    }

    private void logHeader(final int modelNumber, final List<Pair<String, IterativeShiftFactory>> heuristics) {
        logger.log("-------------------------");
        logger.log("Test set number " + modelNumber);
        logger.log("-------------------------");
        final String heuristicsTitles = heuristics.stream()
                .map(Pair::getKey)
                .collect(Collectors.joining("\t\t"));
        logger.log("Candidate\tBest\t\tIterative gap\t\t" + heuristicsTitles);
    }

    private Result getResultsForCandidate(final Dataset candidate,
                                          final Dataset original,
                                          final List<IterativeShiftFactory> factories) {
        final double bestCost = this.getCost(this.calculate(this.bestShiftFactory, original, candidate));

        final Pair<Mapping, MappingStatistics> iterativeMapping = this.calc(this.iterativeShiftFactory, original, candidate);
        final double iterativeCost = this.getCost(iterativeMapping.getKey());

        final List<Pair<Mapping, MappingStatistics>> heuristicsMappings = factories.stream()
                .map(f -> this.calc(f, original, candidate))
                .collect(Collectors.toList());
        final List<Double> gaps = Stream.concat(
                Stream.of(iterativeCost - bestCost),
                heuristicsMappings.stream().map(m -> this.getCost(m.getKey()) - bestCost)).collect(Collectors.toList());
        final List<MappingStatistics> statistics = Stream.concat(
                Stream.of(iterativeMapping.getValue()),
                heuristicsMappings.stream().map(Pair::getValue)).collect(Collectors.toList());
        return new Result(candidate.getId(), bestCost, gaps, statistics);
    }

    private void logResult(final Result result) {
        final StringBuilder outputBuilder = new StringBuilder(result.id);
        outputBuilder.append("\t");
        outputBuilder.append(this.formatCost(result.bestCost, result.bestCost));
        outputBuilder.append("\t[");
        outputBuilder.append(result.mappingStatistics.get(0).possibleOffsets);
        outputBuilder.append("]");
        for (int i = 0; i < result.gaps.size(); i++) {
            outputBuilder.append("\t");
            outputBuilder.append(this.formatCost(result.gaps.get(i), result.bestCost));
            outputBuilder.append("\t[");
            outputBuilder.append(result.mappingStatistics.get(i).initialOffsets);
            outputBuilder.append(",");
            outputBuilder.append(result.mappingStatistics.get(i).usedOffsets);
            outputBuilder.append("]");
        }
        logger.log(outputBuilder.toString());
    }

    private Mapping calculate(final ICalculatorFactory factory, final Dataset original, final Dataset candidate) {
        return factory.getDistanceCalculatorFor(original, candidate).finalMapping();
    }

    private Pair<Mapping, MappingStatistics> calc(final IterativeShiftFactory factory, final Dataset original, final Dataset candidate) {
        return factory.getDistanceCalculatorFor(original, candidate).getFinalMapping();
    }

    private double getCost(final Mapping mapping) {
        return mapping.getMappingCosts().stream()
                .mapToDouble(c -> c.orElse(0.0))
                .sum();
    }

    private String formatCost(final double cost, final double total) {
        return String.format("%5.4f", cost) + " (" + String.format("%4.2f", cost * 100.0 / total) + ")";
    }

    private IterativeShiftFactory createIterativeShiftFactory(
            final INextOffsetCalculator nextOffsetCalculator,
            final INeighborhood neighborhood) {

        return IterativeShiftFactory.from(mapper, costCalculator, intervalDistance, defaultInitialOffsetCalculator,
                nextOffsetCalculator, neighborhood);
    }

    private IterativeShiftFactory createIterativeShiftFactory(
            final IInitialOffsetCalculator initialOffsetCalculator,
            final INextOffsetCalculator nextOffsetCalculator,
            final INeighborhood neighborhood) {
        return IterativeShiftFactory.from(mapper, costCalculator, intervalDistance,
                initialOffsetCalculator, nextOffsetCalculator, neighborhood);
    }

    private IterativeShiftFactory createLengthModificationFactory(IIntervalDistance... distances) {
        return this.createIterativeShiftFactory(this.defaultNextOffsetCalculator,
                ModifiedDistances.using(ImmutableList.copyOf(distances), this.mapper));
    }

    private IterativeShiftFactory createMedianFactory(boolean standard, boolean large, boolean small, boolean extreme, boolean moderate) {
        return this.createIterativeShiftFactory(
                MedianOffset.including(standard, large, small, extreme, moderate),
                defaultNeighborhood);
    }
}
