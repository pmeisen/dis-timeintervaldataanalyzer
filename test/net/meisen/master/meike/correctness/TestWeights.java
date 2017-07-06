package net.meisen.master.meike.correctness;

import com.google.common.collect.ImmutableList;
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

/**
 * Tests for checking how well certain weights for a {@link WeightedSumDistance}
 * work for a certain test set.
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
            logger.log("----------------------------");
            logger.log("Output for test set number " + modelNumber);
            logger.log("----------------------------");

            final Datasets datasets = this.loadDatasets(modelNumber, allCandidateDates.get(modelNumber - 1));

            for (final IIntervalDistance distance : weightedDistances) {
                final List<BoundedDataset> neighbors = this.findNearestNeighbors(datasets, distance);
                this.logPlotterCommands(modelNumber, distance, datasets.original, neighbors);
            }

            this.unload();
        }
    }

    private void logPlotterCommands(final int modelNumber,
                                    final IIntervalDistance distance,
                                    final Dataset original,
                                    final List<BoundedDataset> nearestNeighbors) {
        final ICalculatorFactory bestShiftFactory = this.getBestShiftFactory(distance);

        final StringBuilder commandBuilder = new StringBuilder(
                "python neighbors-plotter.py /home/meike/masterarbeit/data/sascha/sascha" + modelNumber + ".csv \"");

        for (final BoundedDataset neighbor : nearestNeighbors) {
            if (nearestNeighbors.indexOf(neighbor) > 0) {
                commandBuilder.append("_");
            }
            final Mapping bestMapping = bestShiftFactory
                    .getDistanceCalculatorFor(original, neighbor.getDataset())
                    .finalMapping();
            commandBuilder.append(this.getAmerican(neighbor.getDataset().getId()) + ";"
                    + bestMapping.getOffset() + ";" + bestMapping.getMappingIndicesString() + ";" + neighbor.getUpperBound());
        }

        commandBuilder.append("\" \"Dataset " + modelNumber + ", weights " + distance.toString() + "\"");
        commandBuilder.append(" testWeightsOutput" + modelNumber + '_' + distance.toString());
        logger.log(commandBuilder.toString());
    }

    private String getAmerican(final String date) {
        return "2017-" + date.substring(3, 5) + "-" + date.substring(0,2);
    }

    private List<BoundedDataset> findNearestNeighbors(final Datasets datasets, final IIntervalDistance distance) {
        final NearestNeighborSearch knnSearch = this.createKnnSearch(distance);

        return knnSearch.find(datasets.candidates.size(), datasets.original, datasets.candidates);
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
