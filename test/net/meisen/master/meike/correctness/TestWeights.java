package net.meisen.master.meike.correctness;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.correctness.Utils.Neighbor;
import net.meisen.master.meike.impl.distances.datasets.BestShiftFactory;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.ICalculatorFactory;
import net.meisen.master.meike.impl.distances.intervals.Factories;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.LengthDistance;
import net.meisen.master.meike.impl.distances.intervals.WeightedSumDistance;
import net.meisen.master.meike.impl.knnSearch.BoundedDataset;
import net.meisen.master.meike.impl.knnSearch.NearestNeighborSearch;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.costCalculation.ConstantCostForUnmappedIntervals;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;
import net.meisen.master.meike.impl.mapping.lowerBounds.DoubleMatchingBound;
import net.meisen.master.meike.impl.mapping.lowerBounds.ILowerBound;
import net.meisen.master.meike.impl.mapping.upperBounds.Exact;
import net.meisen.master.meike.impl.mapping.upperBounds.IUpperBound;
import org.junit.Test;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Tests for checking how well certain weights for a {@link WeightedSumDistance}
 * work for a certain test set. For that, we use the KNN search to rank all
 * candidates and log the commands for plotting them in that order.
 */
public class TestWeights extends SaschaBasedTest {
    private final List<IIntervalDistance> weightedDistances = ImmutableList.of(
            Factories.weightedDistance(1, 1, 1, 1, 1),
            Factories.weightedDistance(2, 2, 4, 1, 1),
            Factories.weightedDistance(4, 4, 1, 0, 0),
            Factories.weightedDistance(1, 1, 10, 0, 0),
            Factories.weightedDistance(0, 0, 0, 1, 1),
            Factories.weightedDistance(1, 1, 0, 0, 0),
            Factories.weightedDistance(1, 0, 0, 0, 0),
            Factories.weightedDistance(0, 1, 0, 0, 0));

    private final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(0);

    private final List<Integer> modelNumbersToTest = ImmutableList.of(1, 2, 3, 4, 5, 6, 7, 8);

    @Test
    public void testNearestNeighborsForDifferentWeights() {
        for (final int modelNumber : modelNumbersToTest) {
            this.logHeader(modelNumber);
            final Datasets datasets = this.loadDatasets(modelNumber, allCandidateDates.get(modelNumber - 1));
            for (final IIntervalDistance distance : weightedDistances) {
                final List<BoundedDataset> neighbors = this.findNearestNeighbors(datasets, distance);
                this.logPlotterCommand(modelNumber, distance, datasets.original, neighbors);
            }
            this.unload();
        }
    }

    private void logHeader(final int modelNumber) {
        this.logger.log("----------------------------");
        this.logger.log("Output for test set number " + modelNumber);
        this.logger.log("----------------------------");
    }

    private void logPlotterCommand(final int modelNumber,
                                   final IIntervalDistance distanceMeasure,
                                   final Dataset original,
                                   final List<BoundedDataset> nearestNeighbors) {
        final ICalculatorFactory bestShiftFactory = this.getBestShiftFactory(distanceMeasure);

        final String inputFileName = "/home/meike/masterarbeit/data/sascha/sascha" + modelNumber + ".csv";
        final String outputFileName = "testWeights_" + modelNumber + "_" + distanceMeasure;
        final String title = "Dataset " + modelNumber + ", weights " + distanceMeasure;
        final List<Neighbor> neighbors = nearestNeighbors.stream()
                .map(n -> {
                    final Mapping bestMapping = bestShiftFactory
                            .getDistanceCalculatorFor(original, n.getDataset())
                            .finalMapping();
                    return new Neighbor(n.getDataset().getId(), bestMapping, n.getUpperBound());
                }).collect(Collectors.toList());
        this.logger.log(Utils.getNeighborsPlotterCommand(inputFileName, outputFileName, title, neighbors));
    }

    private List<BoundedDataset> findNearestNeighbors(final Datasets datasets, final IIntervalDistance distance) {
        return this.createKnnSearch(distance)
                .find(datasets.candidates.size(), datasets.original, datasets.candidates);
    }

    private NearestNeighborSearch createKnnSearch(final IIntervalDistance distance) {
        final List<ILowerBound> lowerBounds = ImmutableList.of(
                DoubleMatchingBound.from(new LengthDistance(), this.costCalculator));

        final List<IUpperBound> upperBounds = ImmutableList.of(
                Exact.from(this.getBestShiftFactory(distance), this.costCalculator));

        return NearestNeighborSearch.from(lowerBounds, upperBounds);
    }

    private ICalculatorFactory getBestShiftFactory(final IIntervalDistance distance) {
        return BestShiftFactory.from(KuhnMunkres.create(), distance, this.costCalculator);
    }
}
