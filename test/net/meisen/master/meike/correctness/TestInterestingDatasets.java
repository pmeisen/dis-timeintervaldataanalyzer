package net.meisen.master.meike.correctness;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.BestShiftDistance;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.IDatasetDistance;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.IterativeShiftDistance;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.INeighborhood;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.ModifiedDistances;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.CentroidOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.CombinedInitial;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.CombinedNext;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.IInitialOffsetCalculator;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.INextOffsetCalculator;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.LengthOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.MedianOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.MinCostOffset;
import net.meisen.master.meike.impl.distances.intervals.Factories;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.LengthDistance;
import net.meisen.master.meike.impl.knnSearch.BoundedDataset;
import net.meisen.master.meike.impl.knnSearch.NearestNeighborSearch;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
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

public class TestInterestingDatasets extends SaschaBasedTest {
    private final List<IIntervalDistance> intervalDistances = ImmutableList.of(
            Factories.weightedDistance(1, 1, 1, 1, 1)//,
            /*Factories.weightedDistance(2, 2, 4, 1, 1),
            Factories.weightedDistance(4, 4, 2, 0, 0),
            Factories.weightedDistance(0, 0, 1, 1, 1)*/);

    private final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(0);

    private final List<Integer> modelNumbersToTest = ImmutableList.of(3, 6, 7, 8);
    private static final int numberOfNeighborsToFind = 30;

    @Test
    public void foo() {
        for (final int modelNumber : modelNumbersToTest) {
            logger.log("-------------------------");
            logger.log("Testing test set number " + modelNumber);
            logger.log("-------------------------");

            final Datasets datasets = this.loadDatasets(modelNumber, allCandidateDates.get(modelNumber - 1));

            for (final IIntervalDistance distance : intervalDistances) {
                logger.log("---------------------------------");
                logger.log("Weights: " + distance.toString());
                logger.log("---------------------------------");
                final List<BoundedDataset> neighbors = this.findNearestNeighbors(datasets, distance);
                this.logCosts(neighbors);
                this.logPlotterCommands(modelNumber, distance, datasets.original, neighbors);
            }

            this.unload();
        }
    }

    private void logCosts(final List<BoundedDataset> nearestNeighbors) {
        logger.log("Costs of nearest neighbors:");
        for (final BoundedDataset neighbor : nearestNeighbors){
            logger.log(nearestNeighbors.indexOf(neighbor) + ":\t" + neighbor.getDataset().getId() + " \t " + neighbor.getUpperBound());
        }
    }

    private void logPlotterCommands(final int modelNumber,
                                    final IIntervalDistance distance,
                                    final Dataset original,
                                    final List<BoundedDataset> nearestNeighbors) {
        logger.log("Plotting commands:");
        final IDatasetDistance bestShift = this.getBestShiftDistance(distance);
        final IDatasetDistance iterative = this.getIterativeDistance(distance);
        for (final BoundedDataset neighbor : nearestNeighbors) {
            final Mapping bestMapping = bestShift.calculate(original, neighbor.getDataset());
            final Mapping iterativeMapping = iterative.calculate(original, neighbor.getDataset());
            if (bestMapping.getOffset() != iterativeMapping.getOffset()) {
                logger.log(nearestNeighbors.indexOf(neighbor) + ":\t" + String.format("%6.5f", getCostGap(bestMapping, iterativeMapping)));
                logger.log(Utils.getPlotterCommand(modelNumber, neighbor.getDataset().getId(), bestMapping, "-best"));
                logger.log(Utils.getPlotterCommand(modelNumber, neighbor.getDataset().getId(), iterativeMapping, "-iterative"));
            } else if (nearestNeighbors.indexOf(neighbor) < 3) {
                //logger.log(Utils.getPlotterCommand(modelNumber, neighbor.getDataset().getId(), bestMapping, "-best"));
            }
        }
    }

    private double getCostGap(final Mapping bestMapping, final Mapping iterativeMapping) {
        final double bestCost = bestMapping.getMappingCosts().stream().mapToDouble(c -> c.orElse(0.0)).sum();
        final double iterativeCost = iterativeMapping.getMappingCosts().stream().mapToDouble(c -> c.orElse(0.0)).sum();
        return iterativeCost - bestCost;
    }

    private List<BoundedDataset> findNearestNeighbors(final Datasets datasets, final IIntervalDistance distance) {
        final NearestNeighborSearch knnSearch = this.createKnnSearch(distance);

        return knnSearch.find(numberOfNeighborsToFind, datasets.original, datasets.candidates);
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

    private IDatasetDistance getIterativeDistance(final IIntervalDistance distance) {
        final IMinCostMapper mapper = KuhnMunkres.create();

        final INextOffsetCalculator nextOffsetCalculator =
                CombinedNext.from(ImmutableList.of(
                        new MedianOffset(),
                        MinCostOffset.fromIntervalDistance(distance)));

        final IInitialOffsetCalculator initialOffsetCalculator =
                CombinedInitial.from(ImmutableList.of(
                        new CentroidOffset(),
                        LengthOffset.from(mapper, nextOffsetCalculator)));


        final INeighborhood neighborhood = ModifiedDistances.using(ImmutableList.of(
                Factories.weightedDistance(1, 1, 3, 0, 0),
                Factories.weightedDistance(2, 2, 5, 1, 1),
                Factories.weightedDistance(2, 2, 0, 1, 1),
                Factories.weightedDistance(1, 0, 0, 0, 0)),
                mapper);

        return IterativeShiftDistance.from(mapper, distance, initialOffsetCalculator, nextOffsetCalculator, neighborhood);
    }
}
