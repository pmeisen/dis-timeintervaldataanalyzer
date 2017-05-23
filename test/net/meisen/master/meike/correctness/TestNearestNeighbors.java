package net.meisen.master.meike.correctness;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.BestShiftDistance;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.LengthDistance;
import net.meisen.master.meike.impl.knnSearch.BoundedDataset;
import net.meisen.master.meike.impl.knnSearch.NearestNeighborSearch;
import net.meisen.master.meike.impl.mapping.costCalculation.ConstantCostForUnmappedIntervals;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;
import net.meisen.master.meike.impl.mapping.costCalculation.MapAgain;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;
import net.meisen.master.meike.impl.mapping.lowerBounds.DoubleMatchingBound;
import net.meisen.master.meike.impl.mapping.upperBounds.Exact;
import net.meisen.master.meike.impl.distances.intervals.Factories;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

public class TestNearestNeighbors extends SaschaBasedTest {
    private static final List<IIntervalDistance> intervalDistances = ImmutableList.of(
            Factories.weightedDistance(1.0, 1.0, 1.0, 1.0, 1.0),
            Factories.weightedDistance(1.0, 1.0, 3.0, 0.0, 0.0),
            Factories.weightedDistance(3.0, 3.0, 1.0, 2.0, 2.0));
    private static final List<ICostCalculator> constantCalculators = ImmutableList.of(
            ConstantCostForUnmappedIntervals.fromCost(0),
            ConstantCostForUnmappedIntervals.fromCost(1),
            ConstantCostForUnmappedIntervals.fromCost(5));

    @Test
    public void testNearestNeighbors() {
        for (int i = 1; i <= allCandidateDates.size(); i++) {
            logger.log("===========");
            logger.log("Test set " + i + ":");
            logger.log("===========");
            final Datasets datasets = this.loadDatasets(i, allCandidateDates.get(i-1));
            for (final IIntervalDistance distance : intervalDistances) {
                logger.log("Gewichte " + distance.toString());
                logger.log("---------------------------------");
                for (final ICostCalculator costCalculator : this.getCostCalculators(distance)) {
                    final List<BoundedDataset> nearestNeighbors =
                            this.getNearestNeighbors(costCalculator, datasets, distance);
                    for (final BoundedDataset neighbor : nearestNeighbors) {
                        logger.log(neighbor.getDataset().getId() + " - " + neighbor.getUpperBound());
                    }
                    logger.log("-----");
                }
            }
            this.unload();
        }
    }

    private List<BoundedDataset> getNearestNeighbors(
            final ICostCalculator costCalculator,
            final Datasets datasets,
            final IIntervalDistance intervalDistance) {
        final NearestNeighborSearch knnSearch = NearestNeighborSearch.from(
                ImmutableList.of(DoubleMatchingBound.from(new LengthDistance(), costCalculator)),
                ImmutableList.of(Exact.from(BestShiftDistance.from(KuhnMunkres.create(), intervalDistance, costCalculator), costCalculator)));
        return knnSearch.find(5, datasets.original, datasets.candidates);
    }

    private List<ICostCalculator> getCostCalculators(final IIntervalDistance distance) {
        final List<ICostCalculator> result = new ArrayList<>(constantCalculators);
        result.add(MapAgain.from(distance, 10));
        return result;
    }
}
