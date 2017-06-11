package net.meisen.master.meike.correctness;

import com.google.common.collect.ImmutableList;
import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.BestShiftDistance;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.IterativeShiftDistance;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.EmptyNeighborhood;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.INeighborhood;
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

/**
 * Test class for finding examples for which the local search heuristics help.
 */
public class LocalSearchExamples extends SaschaBasedTest {
    private final IIntervalDistance intervalDistance =
            Factories.weightedDistance(1, 1, 1, 0, 0);
    private final ICostCalculator costCalculator =
            ConstantCostForUnmappedIntervals.fromCost(0);
    private final BestShiftDistance bestShiftDistance =
            BestShiftDistance.from(KuhnMunkres.create(), intervalDistance, this.costCalculator);
    private final IterativeShiftDistance iterativeShiftDistance =
            this.createIterativeShiftDistance(new EmptyNeighborhood());
    private final IterativeShiftDistance withLengthModification =
            this.createIterativeShiftDistance(ModifiedDistances.using(ImmutableList.of(
                    Factories.weightedDistance(1, 1, 4, 0, 0)),
                    KuhnMunkres.create()));
    private final IterativeShiftDistance withMedianNextOffset =
            this.createIterativeShiftDistance(new MedianOffset());

    private final List<Integer> modelNumbersToTest = ImmutableList.of(1, 2, 3, 4, 5, 6, 7, 8);

    @Test
    public void calculateBestShiftVsIterative() {
        for (final int modelNumber : modelNumbersToTest) {
            logger.log("-------------------------");
            logger.log("Test set number " + modelNumber);
            logger.log("-------------------------");

            final Datasets datasets = this.loadDatasets(modelNumber, allCandidateDates.get(modelNumber - 1));

            for (final Dataset candidate : datasets.candidates) {
                final Mapping best = this.bestShiftDistance.calculate(datasets.original, candidate);
                final Mapping iterative = this.iterativeShiftDistance.calculate(datasets.original, candidate);
                final double costGap = this.getCostGap(best, iterative);
                if (costGap > 0) {
                    logger.log(candidate.getId() + "\t" + String.format("%5.4f", costGap));
                } else {
                    logger.log("\t\t\t\t\t\t\t" + candidate.getId());
                }
            }

            this.unload();
        }
    }

    @Test
    public void getBetterWithLengthNeighborhood() {
        for (final int modelNumber : modelNumbersToTest) {
            logger.log("-------------------------");
            logger.log("Test set number " + modelNumber);
            logger.log("-------------------------");

            final Datasets datasets = this.loadDatasets(modelNumber, allCandidateDates.get(modelNumber - 1));

            for (final Dataset candidate : datasets.candidates) {
                final Mapping best = this.bestShiftDistance.calculate(datasets.original, candidate);
                final Mapping iterative = this.iterativeShiftDistance.calculate(datasets.original, candidate);
                final double costGap = this.getCostGap(best, iterative);
                if (costGap > 0) {
                    final Mapping withLength = this.withLengthModification.calculate(datasets.original, candidate);
                    final double newGap = this.getCostGap(best, withLength);
                    if (newGap < costGap) {
                        logger.log(candidate.getId() + "\t" + String.format("%5.4f", newGap) + "\t" + String.format("%5.4f", costGap));
                    }
                }
            }

            this.unload();
        }
    }

    @Test
    public void getBetterWithMedianNeighborhood() {
        for (final int modelNumber : modelNumbersToTest) {
            logger.log("-------------------------");
            logger.log("Test set number " + modelNumber);
            logger.log("-------------------------");

            final Datasets datasets = this.loadDatasets(modelNumber, allCandidateDates.get(modelNumber - 1));

            for (final Dataset candidate : datasets.candidates) {
                final Mapping best = this.bestShiftDistance.calculate(datasets.original, candidate);
                final Mapping iterative = this.iterativeShiftDistance.calculate(datasets.original, candidate);
                final double costGap = this.getCostGap(best, iterative);
                if (costGap > 0) {
                    final Mapping withLength = this.withMedianNextOffset.calculate(datasets.original, candidate);
                    final double newGap = this.getCostGap(best, withLength);
                    if (newGap < costGap) {
                        logger.log(candidate.getId() + "\t"
                                + String.format("%5.4f", newGap) + "\t"
                                + String.format("%5.4f", costGap) + "\t"
                                + String.format("%5.4f", newGap / costGap) + "\t"
                                + String.format("%5.4f", costGap - newGap));
                    }
                }
            }

            this.unload();
        }
    }

    @Test
    public void investigateMedianNeighborhoodImprovements() {
        final List<Pair<Integer, List<String>>> testDatasets = ImmutableList.of(
                new Pair<>(3, ImmutableList.of("29.01.2017", "28.02.2017")));

        for (final Pair<Integer, List<String>> testDataset : testDatasets) {
            final Datasets datasets = this.loadDatasets(testDataset.getKey(), testDataset.getValue());
            for (final Dataset candidate : datasets.candidates) {
                final Mapping bestMapping = this.bestShiftDistance.calculate(datasets.original, candidate);
                final Mapping iterativeMapping = this.iterativeShiftDistance.calculate(datasets.original, candidate);
                final Mapping medianMapping = this.withMedianNextOffset.calculate(datasets.original, candidate);
                logger.log(candidate.getId());
                logger.log(Utils.getPlotterCommand(testDataset.getKey(), candidate.getId(), bestMapping, "-best"));
                logger.log(Utils.getPlotterCommand(testDataset.getKey(), candidate.getId(), iterativeMapping, "-iterative"));
                logger.log(Utils.getPlotterCommand(testDataset.getKey(), candidate.getId(), medianMapping, "-median"));
            }
            this.unload();
        }
    }

    @Test
    public void investigateMappingProgression() {
        final int testSet = 3;
        final String date = "28.02.2017";

        final Datasets datasets = this.loadDatasets(testSet, ImmutableList.of(date));
        final Dataset candidate = datasets.candidates.get(0);

        this.iterativeShiftDistance.setMappingLogger(MappingLogger.createFor(testSet, date, "iterative"));
        final Mapping iterativeMapping = this.iterativeShiftDistance.calculate(datasets.original, candidate);

        this.withMedianNextOffset.setMappingLogger(MappingLogger.createFor(testSet, date, "median"));
        final Mapping medianMapping = this.withMedianNextOffset.calculate(datasets.original, candidate);

        this.unload();
    }

    private double getCostGap(final Mapping bestMapping, final Mapping iterativeMapping) {
        final double bestCost = bestMapping.getMappingCosts().stream().mapToDouble(c -> c.orElse(0.0)).sum();
        final double iterativeCost = iterativeMapping.getMappingCosts().stream().mapToDouble(c -> c.orElse(0.0)).sum();
        return iterativeCost - bestCost;
    }

    private IterativeShiftDistance createIterativeShiftDistance(final INeighborhood neighborhood) {
        final IMinCostMapper mapper = KuhnMunkres.create();

        final INextOffsetCalculator nextOffsetCalculator =
                MinCostOffset.fromIntervalDistance(this.intervalDistance);

        final IInitialOffsetCalculator initialOffsetCalculator =
                CombinedInitial.from(ImmutableList.of(
                        new CentroidOffset(),
                        LengthOffset.from(mapper, nextOffsetCalculator)));

        return IterativeShiftDistance.from(mapper, intervalDistance, initialOffsetCalculator, nextOffsetCalculator, neighborhood);
    }

    private IterativeShiftDistance createIterativeShiftDistance(final INextOffsetCalculator nextOffsetCalculator) {
        final IMinCostMapper mapper = KuhnMunkres.create();

        final IInitialOffsetCalculator initialOffsetCalculator =
                CombinedInitial.from(ImmutableList.of(
                        new CentroidOffset(),
                        LengthOffset.from(mapper, MinCostOffset.fromIntervalDistance(this.intervalDistance))));

        final INeighborhood neighborhood = new EmptyNeighborhood();

        return IterativeShiftDistance.from(mapper, intervalDistance, initialOffsetCalculator, nextOffsetCalculator, neighborhood);
    }
}
