package net.meisen.master.meike.correctness;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.correctness.KuhnMunkresResult.Builder;
import net.meisen.master.meike.impl.distances.datasets.BestShiftDistance;
import net.meisen.master.meike.impl.distances.datasets.IDatasetDistance;
import net.meisen.master.meike.impl.distances.datasets.IterativeShiftDistance;
import net.meisen.master.meike.impl.distances.datasets.PlainDistance;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.costCalculation.ConstantCostForUnmappedIntervals;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;
import net.meisen.master.meike.testUtils.Factories;
import org.junit.Test;

import java.util.List;
import java.util.stream.Collectors;

import static net.meisen.master.meike.correctness.KuhnMunkresAssert.assertThatResult;

public class TestKuhnMunkres extends SaschaBasedTest {
    private List<KuhnMunkresResult> getKuhnMunkresDatasetDistances(
            final Datasets datasets, final IIntervalDistance intervalDistance) {
        final IMinCostMapper kuhnMunkres = KuhnMunkres.create();
        final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(1);

        final IDatasetDistance plainDistance = PlainDistance.from(kuhnMunkres, intervalDistance);
        final IDatasetDistance iterativeDistance = IterativeShiftDistance.from(kuhnMunkres, intervalDistance);
        final IDatasetDistance bestShiftDistance = BestShiftDistance.from(kuhnMunkres, intervalDistance, costCalculator);

        return datasets.candidates.stream()
                .map(candidate -> Builder.forDataset(candidate)
                        .withPlainDistanceResult(plainDistance.calculate(datasets.original, candidate))
                        .withIterativeShiftDistanceResult(iterativeDistance.calculate(datasets.original, candidate))
                        .withBestShiftDistanceResult(bestShiftDistance.calculate(datasets.original, candidate))
                        .build(costCalculator))
                .collect(Collectors.toList());
    }

    @Test
    public void testKuhnMunkres() {
        final IIntervalDistance equalWeightsDistance =
                Factories.weightedDistance(1.0, 1.0, 1.0, 1.0, 1.0);
        final IIntervalDistance lengthImportantDistance =
                Factories.weightedDistance(1.0, 1.0, 5.0, 1.0, 1.0);
        final List<IIntervalDistance> distances = ImmutableList.of(equalWeightsDistance, lengthImportantDistance);
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
                    logger.log("\t\t" + result.getImprovements());
                    assertThatResult(result).hasValidCosts();
                }
            }
            this.unload();
        }
    }
}
