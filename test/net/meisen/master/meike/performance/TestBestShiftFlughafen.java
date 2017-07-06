package net.meisen.master.meike.performance;

import com.google.common.collect.ImmutableList;
import javafx.util.Pair;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.master.meike.impl.distances.datasets.BestShiftCalculator;
import net.meisen.master.meike.impl.distances.datasets.BestShiftFactory;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.DatasetFactory;
import net.meisen.master.meike.impl.distances.intervals.Factories;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.costCalculation.ConstantCostForUnmappedIntervals;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;
import net.meisen.master.meike.impl.mapping.costCalculation.MapAgain;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;
import org.junit.Test;
import org.junit.runner.JUnitCore;

import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Tests for the performance of {@link BestShiftCalculator} on the Flughafen
 * data.
 */
public class TestBestShiftFlughafen extends BasePerformanceTest {
    private final List<String> candidateDates = IntStream.range(2, 32)
            .mapToObj(i -> String.format("%02d", i) + ".01.2008")
            .collect(Collectors.toList());
    private final IMinCostMapper minCostMapper = KuhnMunkres.create();
    private final ICostCalculator zeroCostCalculator = ConstantCostForUnmappedIntervals.fromCost(0);
    private final String header = this.getHeader();
    private final List<Pair<String, IIntervalDistance>> distanceMeasures = ImmutableList.of(
            new Pair<>("A", Factories.weightedDistance(1, 1, 1, 1, 1)),
            new Pair<>("B", Factories.weightedDistance(1, 1, 0, 0, 0)),
            new Pair<>("C", Factories.weightedDistance(0, 0, 0, 1, 1)),
            new Pair<>("D", Factories.weightedDistance(2, 2, 4, 1, 1)),
            new Pair<>("E", Factories.weightedDistance(1, 1, 5, 0, 0)),
            new Pair<>("F", Factories.weightedDistance(1, 1, 1, 3, 3)));

    private static int numberOfThreads;

    public static void main(String[] args) throws Exception {
        numberOfThreads = Integer.parseInt(args[0]);
        System.out.println("Using " + numberOfThreads + " threads.");

        new JUnitCore().run(TestBestShiftFlughafen.class);
    }

    @Test
    public void testPerformance() {
        final LoadDatasetsResult datasets = this.loadDatasets();
        this.logger.log("Load datasets: " + datasets.timeInMilliseconds + " milliseconds.");
        this.logger.log("Original dataset has " + datasets.original.getNumberOfIntervals() + " intervals.");

        for (final Pair<String, IIntervalDistance> distanceMeasure : this.distanceMeasures) {
            final BestShiftFactory bestShiftFactory = BestShiftFactory.from(
                    this.minCostMapper, distanceMeasure.getValue(), this.zeroCostCalculator);
            final MapAgain mapAgainCostCalculator = MapAgain.from(
                    distanceMeasure.getValue(), 0);
            this.logger.log("Distance measure: " + distanceMeasure.getKey());
            this.logger.log(this.header);

            final long startTime = System.currentTimeMillis();
            final ExecutorService executorService = Executors.newFixedThreadPool(numberOfThreads);
            for (final Pair<String, Dataset> candidate : datasets.candidates) {
                executorService.submit(() -> {
                    final BestMappingResult bestMapping = this.calculateBestMapping(
                            bestShiftFactory, mapAgainCostCalculator, datasets.original, candidate.getValue());
                    this.logger.log(this.getResultString(candidate, bestMapping));
                });
            }
            executorService.shutdown();
            try {
                executorService.awaitTermination(Long.MAX_VALUE, TimeUnit.SECONDS);
                final long endTime = System.currentTimeMillis();
                logger.log("Took " + (endTime - startTime) + " milliseconds for  this measure.");
            } catch (final InterruptedException e) {
                throw new IllegalStateException(e);
            }
        }
    }

    private String getHeader() {
        final ResultBuilder resultBuilder = new ResultBuilder("Date");
        resultBuilder.append("numberOfIntervals");
        resultBuilder.append("numberOfOffsets");
        resultBuilder.append("timeInMilliseconds");
        resultBuilder.append("cost");
        resultBuilder.append("mapAgainCost");
        resultBuilder.append("bestOffset");
        resultBuilder.append("mappingIndices");
        return resultBuilder.toString();
    }

    private String getResultString(final Pair<String, Dataset> candidate, final BestMappingResult bestMapping) {
        final ResultBuilder resultBuilder = new ResultBuilder(candidate.getKey());
        resultBuilder.append(candidate.getValue().getNumberOfIntervals());
        resultBuilder.append(bestMapping.numberOfOffsets);
        resultBuilder.append(bestMapping.timeInMilliseconds);
        resultBuilder.append(bestMapping.cost);
        resultBuilder.append(bestMapping.mapAgainCost);
        resultBuilder.append(bestMapping.bestMapping.getOffset());
        resultBuilder.append(bestMapping.bestMapping.getMappingIndicesString());
        return resultBuilder.toString();
    }

    private class BestMappingResult {
        public final Mapping bestMapping;
        public final int numberOfOffsets;
        public final double cost;
        public final double mapAgainCost;
        public final long timeInMilliseconds;

        public BestMappingResult(final Mapping bestMapping,
                                 final int numberOfOffsets,
                                 final double cost,
                                 final double mapAgainCost,
                                 final long timeInMilliseconds) {
            this.bestMapping = bestMapping;
            this.numberOfOffsets = numberOfOffsets;
            this.cost = cost;
            this.mapAgainCost = mapAgainCost - cost;
            this.timeInMilliseconds = timeInMilliseconds;
        }
    }

    private BestMappingResult calculateBestMapping(final BestShiftFactory bestShiftFactory,
                                                   final MapAgain mapAgainCostCalculator,
                                                   final Dataset original,
                                                   final Dataset candidate) {
        final long startTime = System.currentTimeMillis();

        final BestShiftCalculator bestShiftCalculator =
                bestShiftFactory.getDistanceCalculatorFor(original, candidate);
        final int numberOfOffsets = bestShiftCalculator.possibleOffsets.size();
        final Mapping bestShiftMapping = bestShiftCalculator.finalMapping();

        final long endTime = System.currentTimeMillis();

        return new BestMappingResult(bestShiftMapping,
                numberOfOffsets,
                this.zeroCostCalculator.calculateCost(bestShiftMapping),
                mapAgainCostCalculator.calculateCost(bestShiftMapping),
                endTime - startTime);
    }

    private class ResultBuilder {
        private final StringBuilder stringBuilder;
        private final String separator = "\t";

        public ResultBuilder(final String start) {
            this.stringBuilder = new StringBuilder(start);
        }

        public void append(final String text) {
            this.stringBuilder.append(this.separator);
            this.stringBuilder.append(text);
        }

        public void append(final long number) {
            this.stringBuilder.append(this.separator);
            this.stringBuilder.append(number);
        }

        public void append(final double number) {
            this.stringBuilder.append(this.separator);
            this.stringBuilder.append(number);
        }

        public String toString() {
            return this.stringBuilder.toString();
        }
    }

    private class LoadDatasetsResult {
        public final Dataset original;
        public final List<Pair<String, Dataset>> candidates;
        public final long timeInMilliseconds;

        public LoadDatasetsResult(final Dataset original,
                                  final List<Pair<String, Dataset>> candidates,
                                  final long timeInMilliseconds) {
            this.original = original;
            this.candidates = candidates;
            this.timeInMilliseconds = timeInMilliseconds;
        }
    }

    private LoadDatasetsResult loadDatasets() {
        final TidaModel model = this.loadModel();
        final DatasetFactory datasetFactory = DatasetFactory.forModel(model);

        final long startTime = System.currentTimeMillis();

        final Dataset originalDataset = this.getFullDatasetForDate("01.01.2008", model, datasetFactory);
        final List<Pair<String, Dataset>> candidates = this.candidateDates.stream()
                .map(date -> new Pair<>(date, this.getFullDatasetForDate(date, model, datasetFactory)))
                .collect(Collectors.toList());
        final long endTime = System.currentTimeMillis();

        return new LoadDatasetsResult(originalDataset, candidates, endTime - startTime);
    }

    private TidaModel loadModel() {
        return m("/net/meisen/master/meike/performance/flughafen-model.xml", true);
    }

    private Dataset getFullDatasetForDate(final String date,
                                          final TidaModel model,
                                          final DatasetFactory datasetFactory) {
        final String query = "SELECT RECORDS FROM flughafen DURING [" + date + " 00:00:00, " + date + " 23:59:59]";
        return this.getDatasetFor(query, model, datasetFactory, date);
    }
}
