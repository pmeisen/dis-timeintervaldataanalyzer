package net.meisen.master.meike.correctness;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.correctness.KuhnMunkresResult.Builder;
import net.meisen.master.meike.impl.distances.datasets.BestShiftFactory;
import net.meisen.master.meike.impl.distances.datasets.ICalculatorFactory;
import net.meisen.master.meike.impl.distances.datasets.PlainFactory;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.IterativeShiftFactory;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.ModifiedDistances;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.CentroidOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.CombinedInitial;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.CombinedNext;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.LengthOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.MedianOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.MinCostOffset;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.costCalculation.ConstantCostForUnmappedIntervals;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;
import net.meisen.master.meike.impl.distances.intervals.Factories;
import org.junit.Test;

import java.util.List;
import java.util.stream.Collectors;

import static net.meisen.master.meike.correctness.KuhnMunkresAssert.assertThatResult;

public class TestKuhnMunkres extends SaschaBasedTest {

    @Test
    public void testKuhnMunkres() {
        final IIntervalDistance equalWeightsDistance =
                Factories.weightedDistance(1.0, 1.0, 1.0, 1.0, 1.0);
        final IIntervalDistance lengthImportantDistance =
                Factories.weightedDistance(1.0, 1.0, 5.0, 1.0, 1.0);
        final List<IIntervalDistance> distances = ImmutableList.of(equalWeightsDistance);
        for (int i = 1; i <= allCandidateDates.size(); i++) {
            logger.log("Test set " + i + ":");
            final Datasets datasets = this.loadDatasets(i, allCandidateDates.get(i-1));

            for (final IIntervalDistance distance : distances) {
                logger.log("Gewichte " + distance.toString());
                logger.log("---------------------------------");
                final List<KuhnMunkresResult> results =
                        this.getKuhnMunkresDatasetDistances(datasets, distance);
                for (final KuhnMunkresResult result : results) {
                    logger.log(result.toString());
                    logger.log(result.getImprovements() + '\n');
                    assertThatResult(result).hasValidCosts();
                }
            }
            this.unload();
        }
    }

    private List<KuhnMunkresResult> getKuhnMunkresDatasetDistances(
            final Datasets datasets, final IIntervalDistance intervalDistance) {
        final IMinCostMapper kuhnMunkres = KuhnMunkres.create();
        final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(1);

        final ICalculatorFactory plainFactory = PlainFactory.from(kuhnMunkres, intervalDistance);
        final ICalculatorFactory iterativeFactory = this.createIterativeFactory(kuhnMunkres, intervalDistance, costCalculator);
        final ICalculatorFactory bestShiftFactory = BestShiftFactory.from(kuhnMunkres, intervalDistance, costCalculator);

        return datasets.candidates.stream()
                .map(candidate -> Builder.forDataset(candidate)
                        .withPlainDistanceResult(plainFactory
                                .getDistanceCalculatorFor(datasets.original, candidate)
                                .finalMapping())
                        .withIterativeShiftDistanceResult(iterativeFactory
                                .getDistanceCalculatorFor(datasets.original, candidate)
                                .finalMapping())
                        .withBestShiftDistanceResult(bestShiftFactory
                                .getDistanceCalculatorFor(datasets.original, candidate)
                                .finalMapping())
                        .build(costCalculator))
                .collect(Collectors.toList());
    }

    private ICalculatorFactory createIterativeFactory(
            final IMinCostMapper minCostMapper,
            final IIntervalDistance intervalDistance,
            final ICostCalculator costCalculator) {
        return IterativeShiftFactory.from(
                minCostMapper,
                costCalculator,
                intervalDistance,
                CombinedInitial.from(ImmutableList.of(
                        new CentroidOffset(),
                        LengthOffset.from(minCostMapper, new MedianOffset()))),
                CombinedNext.from(ImmutableList.of(
                        new MedianOffset(),
                        MinCostOffset.fromIntervalDistance(intervalDistance))),
                ModifiedDistances.using(ImmutableList.of(
                        Factories.weightedDistance(1, 1, 3, 0, 0),
                        Factories.weightedDistance(2, 2, 5, 1, 1)),
                        minCostMapper));
    }
}
