package net.meisen.master.meike.performance;

import com.google.common.collect.ImmutableSet;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.master.meike.impl.distances.datasets.BestShiftDistance;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.DatasetFactory;
import net.meisen.master.meike.impl.distances.datasets.IDatasetDistance;
import net.meisen.master.meike.impl.distances.datasets.PlainDistance;
import net.meisen.master.meike.impl.distances.intervals.EndDistance;
import net.meisen.master.meike.impl.distances.intervals.GapDistance;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.IntersectionDistance;
import net.meisen.master.meike.impl.distances.intervals.LengthDistance;
import net.meisen.master.meike.impl.distances.intervals.StartDistance;
import net.meisen.master.meike.impl.distances.intervals.WeightedSumDistance;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.costCalculation.CompleteMatrix;
import net.meisen.master.meike.impl.mapping.costCalculation.OnlyMatchedIntervals;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;
import net.meisen.master.meike.impl.mapping.lowerBounds.DoubleMatching;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.MappingFactory;
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
        return this.getDatasetFor(query, model, datasetFactory);
    }

    private Dataset getShortDatasetForDate(final String date,
                                           final TidaModel model,
                                           final DatasetFactory datasetFactory) {
        final String query = "SELECT RECORDS FROM flughafen DURING [" + date + " 00:00:00, " + date + " 09:59:59]";
        return this.getDatasetFor(query, model, datasetFactory);
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

        final IMinCostMapper kuhnMunkres =
                KuhnMunkres.from(MappingFactory.from(new OnlyMatchedIntervals()));
        final IMinCostMapper lowerBound =
                DoubleMatching.from(MappingFactory.from(new OnlyMatchedIntervals()), distanceMeasure);

        this.logger.logTiming("Total test run", () -> {
            for (final Dataset candidate : candidates) {
                final CostMatrix costMatrix = this.logger.logTiming("Cost matrix", () ->
                    new CostMatrix(distanceMeasure, original, candidate));
                this.logger.logTiming("Lower bound", () -> {
                    final double boundCost = lowerBound.calculateMinimumCostMapping(costMatrix).getCost();
                    this.logger.log("Bound value:\t" + boundCost);
                });
                this.logger.logTiming("Exact calculation", () -> {
                    final double exactCost = kuhnMunkres.calculateMinimumCostMapping(costMatrix).getCost();
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

        final IMinCostMapper kuhnMunkres =
                KuhnMunkres.from(MappingFactory.from(new OnlyMatchedIntervals()));
        final IMinCostMapper lowerBound =
                DoubleMatching.from(MappingFactory.from(new OnlyMatchedIntervals()), distanceMeasure);

        this.logger.logTiming("Total test run", () -> {
            for (final Dataset candidate : candidates) {
                executorService.submit(() -> {
                    final CostMatrix costMatrix = new CostMatrix(distanceMeasure, original, candidate);
                    final double boundCost = lowerBound.calculateMinimumCostMapping(costMatrix).getCost();
                    final double exactCost = kuhnMunkres.calculateMinimumCostMapping(costMatrix).getCost();
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

        final IDatasetDistance distance = PlainDistance.from(
                KuhnMunkres.from(MappingFactory.from(new CompleteMatrix())),
                this.createIntervalDistance());

        final Mapping mapping1 = distance.calculate(dayOne, dayTwo);
        final Mapping mapping2 = distance.calculate(dayOne, dayThree);
        final Mapping mapping3 = distance.calculate(dayOne, dayFour);

        assertEquals(533.1535571262812, mapping1.getCost(), 0.0000001);
        assertEquals(883.3401090523115, mapping2.getCost(), 0.0000001);
        assertEquals(568.7435313621484, mapping3.getCost(), 0.0000001);
    }

    @Test
    public void testBestShiftKuhnMunkresCosts() {
        final TidaModel model = this.loadModel();
        final DatasetFactory datasetFactory = DatasetFactory.forModel(model);

        final Dataset dayOne = this.getShortDatasetForDate("05.01.2008", model, datasetFactory);
        final Dataset dayTwo = this.getShortDatasetForDate("02.01.2008", model, datasetFactory);
        final Dataset dayThree = this.getShortDatasetForDate("03.01.2008", model, datasetFactory);
        final Dataset dayFour = this.getShortDatasetForDate("04.01.2008", model, datasetFactory);

        final BestShiftDistance distance = BestShiftDistance.from(KuhnMunkres.from(
              MappingFactory.from(new CompleteMatrix())),
                this.createIntervalDistance());
        distance.setMaxOffset(180000);

        final Mapping mapping1 = distance.calculate(dayOne, dayTwo);
        final Mapping mapping2 = distance.calculate(dayOne, dayThree);
        final Mapping mapping3 = distance.calculate(dayOne, dayFour);

        assertEquals(533.1535571262816, mapping1.getCost(), 0.0000001);
        assertEquals(880.3189999366434, mapping2.getCost(), 0.0000001);
        assertEquals(564.1600808460587, mapping3.getCost(), 0.0000001);
    }
}
