package net.meisen.master.meike.performance;

import com.google.common.collect.ImmutableSet;
import javafx.util.Pair;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.DatasetFactory;
import net.meisen.master.meike.impl.distances.intervals.ConstantDistance;
import net.meisen.master.meike.impl.distances.intervals.EndDistance;
import net.meisen.master.meike.impl.distances.intervals.EndPositionDistance;
import net.meisen.master.meike.impl.distances.intervals.GapDistance;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.IntersectionDistance;
import net.meisen.master.meike.impl.distances.intervals.LengthDistance;
import net.meisen.master.meike.impl.distances.intervals.RandomDistance;
import net.meisen.master.meike.impl.distances.intervals.StartDistance;
import net.meisen.master.meike.impl.distances.intervals.StartPositionDistance;
import net.meisen.master.meike.impl.distances.intervals.WeightedSumDistance;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Tests for the performance of cost matrix calculation depending on different
 * distance measures used.
 */
public class TestPerformanceCostMatrix extends BasePerformanceTest {
    private TidaModel loadModel() {
        return m("/net/meisen/master/meike/performance/flughafen-model.xml", true);
    }

    private Dataset getFullDatasetForDate(final String date,
                                          final TidaModel model,
                                          final DatasetFactory datasetFactory) {
        final String query = "SELECT RECORDS FROM flughafen DURING [" + date + " 00:00:00, " + date + " 23:59:59]";
        return this.getDatasetFor(query, model, datasetFactory, date);
    }

    private Pair<Dataset, Collection<Dataset>> loadDatasets() {
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
        return new Pair<>(original, candidates);
    }

    private void createCostMatrices(final IIntervalDistance distance) {
        final Pair<Dataset, Collection<Dataset>> datasets = this.loadDatasets();
        final Dataset original = datasets.getKey();

        this.logger.logTiming("All cost matrices", () -> {
            for (final Dataset candidate : datasets.getValue()) {
                this.logger.logTiming("Cost matrix", () -> {
                    final CostMatrix costMatrix =
                            new CostMatrix(distance, original, candidate);
                });
            }
        });
    }

    @Test
    public void testConstantDistance() {
        final IIntervalDistance distance = new ConstantDistance();
        this.createCostMatrices(distance);
    }

    @Test
    public void testRandomDistance() {
        final IIntervalDistance distance = new RandomDistance();
        this.createCostMatrices(distance);
    }

    @Test
    public void testEndPositionDistance() {
        final IIntervalDistance distance = new EndPositionDistance();
        this.createCostMatrices(distance);
    }

    @Test
    public void testGapDistance() {
        final IIntervalDistance distance = new GapDistance();
        this.createCostMatrices(distance);
    }

    @Test
    public void testIntersectionDistance() {
        final IIntervalDistance distance = new IntersectionDistance();
        this.createCostMatrices(distance);
    }

    @Test
    public void testStartDistance() {
        final IIntervalDistance distance = new StartDistance();
        this.createCostMatrices(distance);
    }

    @Test
    public void testStartPositionDistance() {
        final IIntervalDistance distance = new StartPositionDistance();
        this.createCostMatrices(distance);
    }

    @Test
    public void testLengthDistance() {
        final IIntervalDistance distance = new LengthDistance();
        this.createCostMatrices(distance);
    }

    @Test
    public void testFullDistance() {
        final Map<IIntervalDistance, Double> distances = new HashMap<>();
        distances.put(new EndDistance(), 1.0);
        distances.put(new GapDistance(), 1.0);
        distances.put(new IntersectionDistance(), 1.0);
        distances.put(new LengthDistance(), 1.0);
        distances.put(new StartDistance(), 1.0);
        this.createCostMatrices(new WeightedSumDistance(distances));
    }
}
