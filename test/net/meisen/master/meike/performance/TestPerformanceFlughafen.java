package net.meisen.master.meike.performance;

import com.google.common.collect.ImmutableSet;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.master.meike.impl.distances.datasets.BestShiftFactory;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.DatasetFactory;
import net.meisen.master.meike.impl.distances.datasets.ICalculatorFactory;
import net.meisen.master.meike.impl.distances.datasets.PlainFactory;
import net.meisen.master.meike.impl.distances.intervals.EndDistance;
import net.meisen.master.meike.impl.distances.intervals.GapDistance;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.IntersectionDistance;
import net.meisen.master.meike.impl.distances.intervals.LengthDistance;
import net.meisen.master.meike.impl.distances.intervals.StartDistance;
import net.meisen.master.meike.impl.distances.intervals.WeightedSumDistance;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.costCalculation.ConstantCostForUnmappedIntervals;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;
import net.meisen.master.meike.impl.mapping.lowerBounds.DoubleMatcher;
import net.meisen.master.meike.impl.mapping.Mapping;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.junit.Assert.assertEquals;

/**
 * Performance tests using the flughafen data.
 */
public class TestPerformanceFlughafen extends BasePerformanceTest {
    private TidaModel loadModel() {
        return m("/net/meisen/master/meike/performance/flughafen-model.xml", true);
    }

    private Dataset getFullDatasetForDate(final String date,
                                          final TidaModel model,
                                          final DatasetFactory datasetFactory) {
        final String query = "SELECT RECORDS FROM flughafen DURING [" + date + " 00:00:00, " + date + " 23:59:59]";
        return this.getDatasetFor(query, model, datasetFactory, date);
    }

    private Dataset getShortDatasetForDate(final String date,
                                           final TidaModel model,
                                           final DatasetFactory datasetFactory) {
        final String query = "SELECT RECORDS FROM flughafen DURING [" + date + " 00:00:00, " + date + " 09:59:59]";
        return this.getDatasetFor(query, model, datasetFactory, date);
    }

    private IIntervalDistance createIntervalDistance() {
        final Map<IIntervalDistance, Double> distances = new HashMap<>();
        distances.put(new EndDistance(), 1.0);
        distances.put(new GapDistance(), 1.0);
        distances.put(new IntersectionDistance(), 1.0);
        distances.put(new LengthDistance(), 1.0);
        distances.put(new StartDistance(), 1.0);
        return new WeightedSumDistance(distances);
    }

    @Test
    public void testTimingKuhnMunkres() {
        final TidaModel model = this.loadModel();
        final DatasetFactory datasetFactory = DatasetFactory.forModel(model);

        final String originalDate = "05.01.2008";
        final ImmutableSet<String> candidateDates = ImmutableSet.of(
                "01.01.2008",
                "02.01.2008",
                "03.01.2008",
                "04.01.2008",
                "05.01.2008",
                "06.01.2008",
                "07.01.2008",
                "08.01.2008",
                "09.01.2008",
                "10.01.2008");

        final Dataset original = this.getFullDatasetForDate(originalDate, model, datasetFactory);

        final List<Dataset> candidates = new ArrayList<>();
        for (final String candidateDate : candidateDates) {
            candidates.add(this.getFullDatasetForDate(candidateDate, model, datasetFactory));
        }

        final IIntervalDistance distanceMeasure = this.createIntervalDistance();

        final IMinCostMapper kuhnMunkres = KuhnMunkres.create();
        final IMinCostMapper lowerBound = DoubleMatcher.create();
        final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(0);

        this.logger.logTiming("Total test run", () -> {
            for (final Dataset candidate : candidates) {
                final CostMatrix costMatrix = this.logger.logTiming("Cost matrix", () ->
                    new CostMatrix(distanceMeasure, original, candidate));
                this.logger.logTiming("Lower bound", () -> {
                    final Mapping mapping = lowerBound.calculateMinimumCostMapping(costMatrix);
                    final double boundCost = costCalculator.calculateCost(mapping);
                    this.logger.log("Bound value:\t" + boundCost);
                });
                this.logger.logTiming("Exact calculation", () -> {
                    final Mapping mapping = kuhnMunkres.calculateMinimumCostMapping(costMatrix);
                    final double exactCost = costCalculator.calculateCost(mapping);
                    this.logger.log("Exact value:\t" + exactCost);
                });
            }
        });
    }

    @Test
    public void testParallelization() {
        final TidaModel model = this.loadModel();
        final DatasetFactory datasetFactory = DatasetFactory.forModel(model);

        final String originalDate = "05.01.2008";
        final ImmutableSet<String> candidateDates = ImmutableSet.of(
                "01.01.2008",
                "02.01.2008",
                "03.01.2008",
                "04.01.2008",
                "05.01.2008",
                "06.01.2008",
                "07.01.2008",
                "08.01.2008",
                "09.01.2008",
                "10.01.2008");

        final Dataset original = this.getFullDatasetForDate(originalDate, model, datasetFactory);

        final List<Dataset> candidates = new ArrayList<>();
        for (final String candidateDate : candidateDates) {
            candidates.add(this.getFullDatasetForDate(candidateDate, model, datasetFactory));
        }

        final ExecutorService executorService = Executors.newFixedThreadPool(6);

        final IIntervalDistance distanceMeasure = this.createIntervalDistance();

        final IMinCostMapper kuhnMunkres = KuhnMunkres.create();
        final IMinCostMapper lowerBound = DoubleMatcher.create();
        final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(0);

        this.logger.logTiming("Total test run", () -> {
            for (final Dataset candidate : candidates) {
                executorService.submit(() -> {
                    final CostMatrix costMatrix = new CostMatrix(distanceMeasure, original, candidate);
                    final Mapping boundMapping = lowerBound.calculateMinimumCostMapping(costMatrix);
                    final double boundCost = costCalculator.calculateCost(boundMapping);
                    final Mapping exactMapping = kuhnMunkres.calculateMinimumCostMapping(costMatrix);
                    final double exactCost = costCalculator.calculateCost(exactMapping);
                });
            }
            executorService.shutdown();
            try {
                executorService.awaitTermination(300, TimeUnit.SECONDS);
            } catch (final InterruptedException e) {
                throw new IllegalStateException(e);
            }
        });
    }

    @Test
    public void testPlainDistanceKuhnMunkresCosts() {
        final TidaModel model = this.loadModel();
        final DatasetFactory datasetFactory = DatasetFactory.forModel(model);

        final Dataset dayOne = this.getShortDatasetForDate("05.01.2008", model, datasetFactory);
        final Dataset dayTwo = this.getShortDatasetForDate("02.01.2008", model, datasetFactory);
        final Dataset dayThree = this.getShortDatasetForDate("03.01.2008", model, datasetFactory);
        final Dataset dayFour = this.getShortDatasetForDate("04.01.2008", model, datasetFactory);

        final ICalculatorFactory distance = PlainFactory.from(
                KuhnMunkres.create(), this.createIntervalDistance());
        final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(5);

        final Mapping mapping1 = distance.getDistanceCalculatorFor(dayOne, dayTwo).finalMapping();
        final Mapping mapping2 = distance.getDistanceCalculatorFor(dayOne, dayThree).finalMapping();
        final Mapping mapping3 = distance.getDistanceCalculatorFor(dayOne, dayFour).finalMapping();

        assertEquals(533.3538571262819, costCalculator.calculateCost(mapping1), 0.0000001);
        assertEquals(884.3171285104353, costCalculator.calculateCost(mapping2), 0.0000001);
        assertEquals(569.051723669841, costCalculator.calculateCost(mapping3), 0.0000001);
    }

    @Test
    public void testBestShiftKuhnMunkresCosts() {
        final TidaModel model = this.loadModel();
        final DatasetFactory datasetFactory = DatasetFactory.forModel(model);

        final Dataset dayOne = this.getShortDatasetForDate("05.01.2008", model, datasetFactory);
        final Dataset dayTwo = this.getShortDatasetForDate("02.01.2008", model, datasetFactory);
        final Dataset dayThree = this.getShortDatasetForDate("03.01.2008", model, datasetFactory);
        final Dataset dayFour = this.getShortDatasetForDate("04.01.2008", model, datasetFactory);

        final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(5);
        final BestShiftFactory distance = BestShiftFactory.from(KuhnMunkres.create(),
                this.createIntervalDistance(), costCalculator);
        distance.setMaxOffset(180000);

        final Mapping mapping1 = distance.getDistanceCalculatorFor(dayOne, dayTwo).finalMapping();
        final Mapping mapping2 = distance.getDistanceCalculatorFor(dayOne, dayThree).finalMapping();
        final Mapping mapping3 = distance.getDistanceCalculatorFor(dayOne, dayFour).finalMapping();

        assertEquals(533.3538571262819, costCalculator.calculateCost(mapping1), 0.0000001);
        assertEquals(881.2912381719334, costCalculator.calculateCost(mapping2), 0.0000001);
        assertEquals(564.4698591965745, costCalculator.calculateCost(mapping3), 0.0000001);
    }
}
