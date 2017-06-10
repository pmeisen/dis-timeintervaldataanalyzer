package net.meisen.master.meike.correctness;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.BestShiftDistance;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.IDatasetDistance;
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
            Factories.weightedDistance(4, 4, 2, 0, 0),
            Factories.weightedDistance(0, 0, 1, 1, 1));

    private final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(0);

    private final List<Integer> modelNumbersToTest = ImmutableList.of(1, 2, 3, 4, 5, 6, 7, 8);

    @Test
    public void foo() {
        for (final int modelNumber : modelNumbersToTest) {
            logger.log("----------------------------");
            logger.log("Output for test set number " + modelNumber);
            logger.log("----------------------------");

            final Datasets datasets = this.loadDatasets(modelNumber, allCandidateDates.get(modelNumber - 1));

            for (final IIntervalDistance distance : weightedDistances) {
                /*logger.log("---------------------------------");
                logger.log("Weights: " + distance.toString());
                logger.log("---------------------------------");*/
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
        final IDatasetDistance bestShift = this.getBestShiftDistance(distance);

        final StringBuilder commandBuilder = new StringBuilder(
                "python neighbors-plotter.py /home/meike/masterarbeit/data/sascha/sascha" + modelNumber + ".csv \"");

        for (final BoundedDataset neighbor : nearestNeighbors) {
            if (nearestNeighbors.indexOf(neighbor) > 0) {
                commandBuilder.append("_");
            }
            final Mapping bestMapping = bestShift.calculate(original, neighbor.getDataset());
            commandBuilder.append(this.getAmerican(neighbor.getDataset().getId()) + ";"
                    + bestMapping.getOffset() + ";" + bestMapping.getMappingIndices() + ";" + neighbor.getUpperBound());
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

        return knnSearch.find(10, datasets.original, datasets.candidates);
    }

    private NearestNeighborSearch createKnnSearch(final IIntervalDistance distance) {
        final List<ILowerBound> lowerBounds = ImmutableList.of(
                DoubleMatchingBound.from(new LengthDistance(), this.costCalculator));

        final List<IUpperBound> upperBounds = ImmutableList.of(
                Exact.from(this.getBestShiftDistance(distance), this.costCalculator));

        return NearestNeighborSearch.from(lowerBounds, upperBounds);
    }

    private IDatasetDistance getBestShiftDistance(final IIntervalDistance distance) {
        return BestShiftDistance.from(KuhnMunkres.create(), distance, this.costCalculator);
    }
}
