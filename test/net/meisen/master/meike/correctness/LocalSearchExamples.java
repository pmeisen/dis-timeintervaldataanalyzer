package net.meisen.master.meike.correctness;

import com.google.common.collect.ImmutableList;
import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.BestShiftFactory;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.IterativeShiftFactory;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.EmptyNeighborhood;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.INeighborhood;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.LocalPerturbation;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.ModifiedDistances;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.CentroidOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.CombinedInitial;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.IInitialOffsetCalculator;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.INextOffsetCalculator;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.LengthOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.MedianOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.MinCostOffset;
import net.meisen.master.meike.impl.distances.intervals.Factories;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.logging.MappingLogger;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.costCalculation.ConstantCostForUnmappedIntervals;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;
import org.junit.Test;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Test class for finding examples for which the local search heuristics help.
 */
public class LocalSearchExamples extends SaschaBasedTest {
    private final IIntervalDistance intervalDistance =
            Factories.weightedDistance(1, 1, 1, 0, 0);

    private final ICostCalculator costCalculator =
            ConstantCostForUnmappedIntervals.fromCost(0);

    private final BestShiftFactory bestShiftFactory = BestShiftFactory.from(
            KuhnMunkres.create(), intervalDistance, this.costCalculator);

    private final IterativeShiftFactory iterativeShiftFactory =
            this.createIterativeShiftFactory(new EmptyNeighborhood());

    private final IterativeShiftFactory withLengthModification =
            this.createIterativeShiftFactory(ModifiedDistances.using(ImmutableList.of(
                    Factories.weightedDistance(1, 1, 4, 0, 0)),
                    KuhnMunkres.create()));

    private final IterativeShiftFactory withLocalPerturbation =
            this.createIterativeShiftFactory(new LocalPerturbation());

    private final List<IterativeShiftFactory> withMedianOffsets =
            this.createFactoriesWithMedianOffsets();

    private final IterativeShiftFactory withMedianAndLengthModification =
            this.createIterativeShiftFactory(MedianOffset.usingAll(), new LocalPerturbation());

    private final List<Integer> modelNumbersToTest = ImmutableList.of(1, 2, 3, 4, 5, 6, 7, 8);

    private String formatAbsolute(final double cost) {
        return String.format("%5.4f", cost);
    }

    private String formatRelative(final double cost, final double total) {
        return " (" + String.format("%4.2f", cost * 100.0 / total) + ")";
    }

    @Test
    public void investigateMedianNeighborhoodImprovements() {
        final List<Pair<Integer, List<String>>> testDatasets = ImmutableList.of(
                new Pair<>(6, ImmutableList.of("16.11.2017", "23.10.2017")),
                new Pair<>(7, ImmutableList.of("15.03.2017", "15.09.2017")),
                new Pair<>(3, ImmutableList.of("17.02.2017", "28.02.2017", "05.04.2017", "08.06.2017",
                        "15.06.2017", "08.08.2017", "16.10.2017", "15.11.2017", "04.12.2017")));

        for (final Pair<Integer, List<String>> testDataset : testDatasets) {
            final Datasets datasets = this.loadDatasets(testDataset.getKey(), testDataset.getValue());
            for (final Dataset candidate : datasets.candidates) {
                final Mapping bestMapping = this.bestShiftFactory
                        .getDistanceCalculatorFor(datasets.original, candidate)
                        .finalMapping();
                final Mapping iterativeMapping = this.iterativeShiftFactory
                        .getDistanceCalculatorFor(datasets.original, candidate)
                        .finalMapping();
                final List<Mapping> medianMappings = this.withMedianOffsets.stream()
                        .map(o -> o.getDistanceCalculatorFor(datasets.original, candidate).finalMapping())
                        .collect(Collectors.toList());
                final Mapping lengthMapping = this.withLengthModification
                        .getDistanceCalculatorFor(datasets.original, candidate)
                        .finalMapping();
                final Mapping combinedMapping = this.withMedianAndLengthModification
                        .getDistanceCalculatorFor(datasets.original, candidate)
                        .finalMapping();
                logger.log(candidate.getId());
                logger.log(Utils.getPlotterCommand(testDataset.getKey(), candidate.getId(), bestMapping, "-best"));
                logger.log(Utils.getPlotterCommand(testDataset.getKey(), candidate.getId(), iterativeMapping, "-iterative"));
                for (int i = 0; i < medianMappings.size(); i++) {
                    logger.log(Utils.getPlotterCommand(testDataset.getKey(), candidate.getId(), medianMappings.get(i), "-median" + (i+1)));
                }
                logger.log(Utils.getPlotterCommand(testDataset.getKey(), candidate.getId(), lengthMapping, "-length"));
                logger.log(Utils.getPlotterCommand(testDataset.getKey(), candidate.getId(), combinedMapping, "-combined"));
            }
            this.unload();
        }
    }

    @Test
    public void investigateMappingProgression() {
        final int testSet = 6;
        final String date = "16.11.2017";

        final Datasets datasets = this.loadDatasets(testSet, ImmutableList.of(date));
        final Dataset candidate = datasets.candidates.get(0);

        this.iterativeShiftFactory.setMappingLogger(MappingLogger.createFor(testSet, date, "iterative"));
        final Mapping iterativeMapping = this.iterativeShiftFactory
                .getDistanceCalculatorFor(datasets.original, candidate)
                .finalMapping();
        this.iterativeShiftFactory.disableMappingLogger();

        final IterativeShiftFactory withMedian = this.createIterativeShiftFactory(
                MedianOffset.including(false, false, false, true, false),
                new EmptyNeighborhood());
        withMedian.setMappingLogger(MappingLogger.createFor(testSet, date, "median"));
        final Mapping medianMapping = withMedian
                .getDistanceCalculatorFor(datasets.original, candidate)
                .finalMapping();
        withMedian.disableMappingLogger();

        this.unload();
    }

    private double getCost(final Mapping mapping) {
        return mapping.getMappingCosts().stream().mapToDouble(c -> c.orElse(0.0)).sum();
    }

    private IterativeShiftFactory createIterativeShiftFactory(
            final INeighborhood neighborhood) {
        final INextOffsetCalculator nextOffsetCalculator =
                MinCostOffset.fromIntervalDistance(this.intervalDistance);
        return this.createIterativeShiftFactory(nextOffsetCalculator, neighborhood);
    }

    private IterativeShiftFactory createIterativeShiftFactory(
            final INextOffsetCalculator nextOffsetCalculator) {
        return this.createIterativeShiftFactory(nextOffsetCalculator, new EmptyNeighborhood());
    }

    private IterativeShiftFactory createIterativeShiftFactory(
            final INextOffsetCalculator nextOffsetCalculator,
            final INeighborhood neighborhood) {
        final IMinCostMapper mapper = KuhnMunkres.create();

        final IInitialOffsetCalculator initialOffsetCalculator =
                CombinedInitial.from(ImmutableList.of(
                        new CentroidOffset(),
                        LengthOffset.from(mapper, MinCostOffset.fromIntervalDistance(this.intervalDistance))));

        return IterativeShiftFactory.from(mapper, costCalculator, intervalDistance,
                initialOffsetCalculator, nextOffsetCalculator, neighborhood);
    }

    private List<IterativeShiftFactory> createFactoriesWithMedianOffsets() {
        final List<MedianOffset> offsets = ImmutableList.of(
                MedianOffset.including(true, false, false, false, false),
                MedianOffset.including(false, true, false, false, false),
                MedianOffset.including(false, false, true, false, false),
                MedianOffset.including(false, false, false, true, false),
                MedianOffset.including(false, false, false, false, true),
                MedianOffset.including(true, false, false, true, false));

        return offsets.stream().map(this::createIterativeShiftFactory).collect(Collectors.toList());
    }
}
