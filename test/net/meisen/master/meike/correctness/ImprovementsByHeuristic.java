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
    public void testMedianOffsetImprovements() {
        final List<Pair<String, IterativeShiftFactory>> heuristics = ImmutableList.of(
                new Pair<>("Standard", this.createMedianFactory(true, false, false, false, false)),
                new Pair<>("Largest\t", this.createMedianFactory(false, true, false, false, false)),
                new Pair<>("Smallest", this.createMedianFactory(false, false, true, false, false)),
                new Pair<>("Extreme\t", this.createMedianFactory(false, false, false, true, false)),
                new Pair<>("Moderate", this.createMedianFactory(false, false, false, false, true)),
                new Pair<>("SmExtr\t", this.createMedianFactory(true, false, false, true, false)),
                new Pair<>("Combined", this.createMedianFactory(true, true, true, true, true)));

        this.calculateImprovements(heuristics);
    }

    @Test
    public void testLengthNeighborhoodImprovements() {
        final INeighborhood fourNeighborhood = ModifiedDistances.using(
                ImmutableList.of(
                        Factories.weightedDistance(1, 1, 4, 0, 0)),
                mapper);
        final INeighborhood tenNeighborhood = ModifiedDistances.using(
                ImmutableList.of(
                        Factories.weightedDistance(1, 1, 10, 0, 0)),
                mapper);
        final INeighborhood zeroNeighborhood = ModifiedDistances.using(
                ImmutableList.of(
                        Factories.weightedDistance(1, 1, 0, 0, 0)),
                mapper);
        final INeighborhood combinedNeighborhood = ModifiedDistances.using(
                ImmutableList.of(
                        Factories.weightedDistance(1, 1, 4, 0, 0),
                        Factories.weightedDistance(1, 1, 10, 0, 0),
                        Factories.weightedDistance(1, 1, 0, 0, 0)),
                mapper);

        final List<Pair<String, IterativeShiftFactory>> heuristics = ImmutableList.of(
                new Pair<>("Four Length", this.createLengthFactory(fourNeighborhood)),
                new Pair<>("Ten Length", this.createLengthFactory(tenNeighborhood)),
                new Pair<>("Zero Length", this.createLengthFactory(zeroNeighborhood)),
                new Pair<>("Combined", this.createLengthFactory(combinedNeighborhood)));

        this.calculateImprovements(heuristics);
    }

    @Test
    public void testCombinedImprovement() {
        final INextOffsetCalculator nextOffsetCalculator =
                this.createMedianOffset(true, true, true, true, true);

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
                new Pair<>("Only Median",this.createIterativeShiftFactory(nextOffsetCalculator, defaultNeighborhood)),
                new Pair<>("With Four L", this.createIterativeShiftFactory(nextOffsetCalculator, fourNeighborhood)),
                new Pair<>("With Combin", this.createIterativeShiftFactory(nextOffsetCalculator, combinedNeighborhood)));

        this.calculateImprovements(heuristics);
    }

    @Test
    public void testMostPromisingCandidates() {
        final INextOffsetCalculator allMediansNextOffset =
                this.createMedianOffset(true, true, true, true, true);

        final INeighborhood modifiedLengthNeighborhood = ModifiedDistances.using(
                ImmutableList.of(
                        Factories.weightedDistance(1, 1, 4, 0, 0),
                        Factories.weightedDistance(5, 5, 1, 1, 1)),
                mapper);

        final INeighborhood localPerturbationNeighborhood = new LocalPerturbation();

        final List<Pair<String, IterativeShiftFactory>> heuristics = ImmutableList.of(
                new Pair<>("Everything",this.createIterativeShiftFactory(allMediansNextOffset,
                        CombinedNeighborhood.from(ImmutableList.of(modifiedLengthNeighborhood, localPerturbationNeighborhood)))),
                new Pair<>("Median Length", this.createIterativeShiftFactory(allMediansNextOffset, modifiedLengthNeighborhood)));

        this.calculateImprovements(heuristics);
    }

    @Test
    public void testInfluenceOfMonotoneMappingHeuristic() {
        final IInitialOffsetCalculator monotoneMapping = MonotoneMapping.fromDistance(intervalDistance, 5);
        final INextOffsetCalculator allMediansNextOffset =
                this.createMedianOffset(true, true, true, true, true);
        final INextOffsetCalculator standardMedianNextOffset =
                this.createMedianOffset(true, false, false, false, false);
        final INeighborhood modifiedLengthNeighborhood = ModifiedDistances.using(
                ImmutableList.of(
                        Factories.weightedDistance(1, 1, 4, 0, 0),
                        Factories.weightedDistance(5, 5, 1, 1, 1)),
                mapper);
        final INeighborhood localPerturbationNeighborhood = new LocalPerturbation();

        final List<Pair<String, IterativeShiftFactory>> heuristics = ImmutableList.of(
                new Pair<>("Only MonMap", this.createIterativeShiftFactory(
                        monotoneMapping, defaultNextOffsetCalculator, defaultNeighborhood)),
                new Pair<>("WithMedians", this.createIterativeShiftFactory(
                        monotoneMapping,
                        CombinedNext.from(ImmutableList.of(defaultNextOffsetCalculator, allMediansNextOffset)),
                        defaultNeighborhood)),
                new Pair<>("WithMed&Length", this.createIterativeShiftFactory(
                        monotoneMapping,
                        CombinedNext.from(ImmutableList.of(defaultNextOffsetCalculator, allMediansNextOffset)),
                        modifiedLengthNeighborhood)),
                new Pair<>("WithLocalPerturbation", this.createIterativeShiftFactory(
                        monotoneMapping,
                        defaultNextOffsetCalculator,
                        localPerturbationNeighborhood)),
                new Pair<>("Everything", this.createIterativeShiftFactory(
                        CombinedInitial.from(ImmutableList.of(new CentroidOffset(), monotoneMapping)),
                        CombinedNext.from(ImmutableList.of(defaultNextOffsetCalculator, allMediansNextOffset)),
                        CombinedNeighborhood.from(ImmutableList.of(modifiedLengthNeighborhood, localPerturbationNeighborhood)))));

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
            logger.log("-------------------------");
            logger.log("Test set number " + modelNumber);
            logger.log("-------------------------");
            final String heuristicsTitles = heuristics.stream()
                    .map(Pair::getKey)
                    .collect(Collectors.joining("\t\t"));
            logger.log("Candidate\tBest\t\tIter gap\t\t" + heuristicsTitles);

            final Datasets datasets = this.loadDatasets(modelNumber, allCandidateDates.get(modelNumber - 1));
            final List<IterativeShiftFactory> factories = heuristics.stream().map(Pair::getValue).collect(Collectors.toList());

            datasets.candidates.stream()
                    .map(candidate -> {
                        final double bestCost = this.getCost(this.calculate(this.bestShiftFactory, datasets.original, candidate));
                        final Pair<Mapping, MappingStatistics> iterativeMapping = this.calc(this.iterativeShiftFactory, datasets.original, candidate);
                        final double iterativeCost = this.getCost(iterativeMapping.getKey());
                        final List<Pair<Mapping, MappingStatistics>> mappings = factories.stream()
                                .map(f -> this.calc(f, datasets.original, candidate))
                                .collect(Collectors.toList());
                        final List<Double> gaps = Stream.concat(
                                Stream.of(iterativeCost - bestCost),
                                mappings.stream().map(m -> this.getCost(m.getKey()) - bestCost)).collect(Collectors.toList());
                        final List<MappingStatistics> statistics = Stream.concat(
                                Stream.of(iterativeMapping.getValue()),
                                mappings.stream().map(Pair::getValue)).collect(Collectors.toList());
                        return new Result(candidate.getId(), bestCost, gaps, statistics);
                    })
                    .sorted(Comparator.comparing(result -> result.bestCost))
                    .forEach(result -> {
                        final StringBuilder outputBuilder = new StringBuilder(result.id);
                        outputBuilder.append("\t");
                        outputBuilder.append(this.formatAbsolute(result.bestCost));
                        outputBuilder.append("\t[");
                        outputBuilder.append(result.mappingStatistics.get(0).possibleOffsets);
                        outputBuilder.append("]");
                        for (int i = 0; i < result.gaps.size(); i++) {
                            outputBuilder.append("\t");
                            outputBuilder.append(this.formatAbsolute(result.gaps.get(i)));
                            outputBuilder.append(this.formatRelative(result.gaps.get(i), result.bestCost));
                            outputBuilder.append("\t[");
                            outputBuilder.append(result.mappingStatistics.get(i).initialOffsets);
                            outputBuilder.append(",");
                            outputBuilder.append(result.mappingStatistics.get(i).usedOffsets);
                            outputBuilder.append("]");
                        }
                        logger.log(outputBuilder.toString());
                    });
            this.unload();
        }
    }

    private void calculateImprovementsOld(final List<Pair<String, IterativeShiftFactory>> heuristics) {
        for (final int modelNumber : modelNumbersToTest) {
            logger.log("-------------------------");
            logger.log("Test set number " + modelNumber);
            logger.log("-------------------------");
            final String heuristicsTitles = heuristics.stream()
                    .map(Pair::getKey)
                    .collect(Collectors.joining("\t\t"));
            logger.log("Candidate\tBest\tIter gap\t\t" + heuristicsTitles);

            final Datasets datasets = this.loadDatasets(modelNumber, allCandidateDates.get(modelNumber - 1));
            final List<ICalculatorFactory> factories = heuristics.stream().map(Pair::getValue).collect(Collectors.toList());

            for (final Dataset candidate : datasets.candidates) {
                final Mapping best = this.calculate(this.bestShiftFactory, datasets.original, candidate);
                final Mapping iterative = this.calculate(this.iterativeShiftFactory, datasets.original, candidate);
                final double bestCost = this.getCost(best);
                final double iterativeCost = this.getCost(iterative);

                final List<Mapping> mappings = factories.stream()
                        .map(f -> this.calculate(f, datasets.original, candidate))
                        .collect(Collectors.toList());
                final List<Double> gaps = Stream.concat(
                        Stream.of(iterativeCost - bestCost),
                        mappings.stream().map(m -> this.getCost(m) - bestCost)).collect(Collectors.toList());

                final StringBuilder outputBuilder = new StringBuilder(candidate.getId());
                outputBuilder.append("\t");
                outputBuilder.append(this.formatAbsolute(bestCost));
                for (final Double gap : gaps) {
                    outputBuilder.append("\t");
                    outputBuilder.append(this.formatAbsolute(gap));
                    outputBuilder.append(this.formatRelative(gap, bestCost));
                }
                logger.log(outputBuilder.toString());

                this.unload();
            }
        }
    }

    private Mapping calculate(final ICalculatorFactory factory, final Dataset original, final Dataset candidate) {
        return factory.getDistanceCalculatorFor(original, candidate).finalMapping();
    }

    private Pair<Mapping, MappingStatistics> calc(final IterativeShiftFactory factory, final Dataset original, final Dataset candidate) {
        return factory.getDistanceCalculatorFor(original, candidate).getFinalMapping();
    }

    private double getCost(final Mapping mapping) {
        return mapping.getMappingCosts().stream().mapToDouble(c -> c.orElse(0.0)).sum();
    }

    private String formatAbsolute(final double cost) {
        return String.format("%5.4f", cost);
    }

    private String formatRelative(final double cost, final double total) {
        return " (" + String.format("%4.2f", cost * 100.0 / total) + ")";
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

    private IterativeShiftFactory createLengthFactory(final INeighborhood neighborhood) {
        return this.createIterativeShiftFactory(defaultNextOffsetCalculator, neighborhood);
    }

    private IterativeShiftFactory createMedianFactory(boolean standard, boolean large, boolean small, boolean extreme, boolean moderate) {
        return this.createIterativeShiftFactory(
                this.createMedianOffset(standard, large, small, extreme, moderate),
                defaultNeighborhood);
    }

    private MedianOffset createMedianOffset(boolean standard, boolean large, boolean small, boolean extreme, boolean moderate) {
        final MedianOffset offset = new MedianOffset();
        offset.useStandard = standard;
        offset.useLarger = large;
        offset.useSmaller = small;
        offset.useMostExtreme = extreme;
        offset.useLeastExtreme = moderate;
        return offset;
    }
}
