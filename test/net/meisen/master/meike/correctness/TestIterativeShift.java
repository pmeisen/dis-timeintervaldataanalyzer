package net.meisen.master.meike.correctness;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.BestShiftDistance;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.IDatasetDistance;
import net.meisen.master.meike.impl.distances.datasets.IterativeShiftDistance;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.costCalculation.ConstantCostForUnmappedIntervals;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;
import net.meisen.master.meike.testUtils.Factories;
import org.junit.Test;

import java.util.List;
import java.util.stream.Collectors;

public class TestIterativeShift extends SaschaBasedTest {
    @Test
    public void testIrregularitiesInFirstTestSet() {
        final IMinCostMapper kuhnMunkres = KuhnMunkres.create();
        final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(1);

        final IIntervalDistance equalWeightsDistance =
                Factories.weightedDistance(1.0, 1.0, 1.0, 1.0, 1.0);
        final IDatasetDistance iterativeDistance =
                IterativeShiftDistance.from(kuhnMunkres, equalWeightsDistance);
        final IDatasetDistance bestShiftDistance =
                BestShiftDistance.from(kuhnMunkres, equalWeightsDistance, costCalculator);

        final Datasets datasets = this.loadDatasets(1, ImmutableList.of("11.03.2017", "06.05.2017"));
        for (final Dataset candidate : datasets.candidates) {
            logger.log(getRepresentation(datasets.original));
            logger.log(getRepresentation(candidate));

            final Mapping iterativeMapping = iterativeDistance.calculate(datasets.original, candidate);
            logger.log("Iterative : \t " + getRepresentation(iterativeMapping));

            final Mapping bestMapping = bestShiftDistance.calculate(datasets.original, candidate);
            logger.log("Best Shift: \t " + getRepresentation(bestMapping));
        }
    }

    private String getRepresentation(final Dataset dataset) {
        return String.join(", ", getIntervalRepresentations(dataset));
    }

    private List<String> getIntervalRepresentations(final Dataset dataset) {
        return dataset.getIntervals().stream()
                .map(Interval::toString)
                .collect(Collectors.toList());
    }

    private String getRepresentation(final Mapping mapping) {
        return mapping.getMappingIndices();
    }
}
