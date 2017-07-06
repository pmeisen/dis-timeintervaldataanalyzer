package net.meisen.master.meike.correctness;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.BestShiftFactory;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.ICalculatorFactory;
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
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.costCalculation.ConstantCostForUnmappedIntervals;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;
import net.meisen.master.meike.impl.distances.intervals.Factories;
import org.junit.Test;

import java.util.List;

public class TestIterativeVsBestShift extends SaschaBasedTest {
    @Test
    public void testIrregularities() {
        final int modelNumber = 3;
        final List<String> dates = ImmutableList.of(
                "17.02.2017", "15.03.2017", "22.03.2017", "05.04.2017",
                "14.04.2017", "01.06.2017", "27.06.2017", "28.09.2017",
                "16.10.2017", "15.11.2017", "04.12.2017");
        final IMinCostMapper kuhnMunkres = KuhnMunkres.create();
        final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(0);

        final IIntervalDistance equalWeightsDistance =
                Factories.weightedDistance(1.0, 1.0, 1.0, 1.0, 1.0);
        final ICalculatorFactory iterativeFactory =
                IterativeShiftFactory.from(kuhnMunkres,
                        costCalculator,
                        equalWeightsDistance,
                        CombinedInitial.from(ImmutableList.of(
                                new CentroidOffset(),
                                LengthOffset.from(kuhnMunkres, MedianOffset.usingAll()))),
                        CombinedNext.from(ImmutableList.of(
                                MedianOffset.usingAll(),
                                MinCostOffset.fromIntervalDistance(equalWeightsDistance))),
                        ModifiedDistances.using(ImmutableList.of(
                                Factories.weightedDistance(1, 1, 3, 0, 0),
                                Factories.weightedDistance(2, 2, 5, 1, 1)),
                                kuhnMunkres));
        final ICalculatorFactory bestShiftFactory =
                BestShiftFactory.from(kuhnMunkres, equalWeightsDistance, costCalculator);

        final Datasets datasets = this.loadDatasets(modelNumber, dates);
        for (final Dataset candidate : datasets.candidates) {
            final Mapping iterativeMapping = iterativeFactory
                    .getDistanceCalculatorFor(datasets.original, candidate)
                    .finalMapping();
            logger.log(Utils.getPlotterCommand(modelNumber, candidate.getId(), iterativeMapping, "-iterative"));

            final Mapping bestMapping = bestShiftFactory
                    .getDistanceCalculatorFor(datasets.original, candidate)
                    .finalMapping();
            logger.log(Utils.getPlotterCommand(modelNumber, candidate.getId(), bestMapping, "-best"));
        }
    }
}
