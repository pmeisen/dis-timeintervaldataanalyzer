package net.meisen.master.meike.examples;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.BestShiftFactory;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.PlainFactory;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.IterativeShiftFactory;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.EmptyNeighborhood;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.LocalPerturbation;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.CentroidOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.MonotoneMapping;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.MedianOffset;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.MinCostOffset;
import net.meisen.master.meike.impl.distances.intervals.Factories;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.costCalculation.ConstantCostForUnmappedIntervals;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;
import net.meisen.master.meike.impl.mapping.exact.KuhnMunkres;
import org.junit.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;

/**
 * Used for finding example datasets for which the plain iterative shift
 * distance does not find the optimal shift.
 */
public class IterativeSuboptimal {
    private final Dataset original = new Dataset(ImmutableList.of(
            new Interval(0L, 2L),
            new Interval(30L, 32L),
            new Interval(33L, 35L),
            new Interval(36L, 38L),
            new Interval(39L, 41L),
            new Interval(42L, 44L),
            new Interval(45L, 47L)));
    private final Dataset candidate = new Dataset(ImmutableList.of(
            new Interval(30L, 32L),
            new Interval(33L, 35L),
            new Interval(36L, 38L),
            new Interval(39L, 41L),
            new Interval(42L, 44L),
            new Interval(45L, 47L),
            new Interval(65L, 67L)));

    private final IMinCostMapper mapper = KuhnMunkres.create();
    private final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(0);
    private final IIntervalDistance intervalDistance = Factories.weightedDistance(1, 1, 1, 0, 0);
    private final PlainFactory plainFactory = PlainFactory.from(mapper, intervalDistance);
    private final BestShiftFactory bestShiftFactory = BestShiftFactory.from(mapper, intervalDistance, costCalculator);
    private final IterativeShiftFactory iterativeShiftFactory = createIterativeShiftFactory();

    @Test
    public void testThatZeroOffsetIsOptimal() {
        final Mapping defaultMapping = plainFactory
                .getDistanceCalculatorFor(original, candidate)
                .finalMapping();
        final Mapping bestMapping = bestShiftFactory
                .getDistanceCalculatorFor(original, candidate)
                .finalMapping();

        System.out.println(costCalculator.calculateCost(defaultMapping));
        System.out.println(costCalculator.calculateCost(bestMapping));

        assertEquals(costCalculator.calculateCost(defaultMapping),
                costCalculator.calculateCost(bestMapping), 0);
    }

    @Test
    public void testThatIterativeDoesNotMapOptimally() {
        final Mapping iterativeMapping = iterativeShiftFactory
                .getDistanceCalculatorFor(original, candidate)
                .finalMapping();

        System.out.println(costCalculator.calculateCost(iterativeMapping));

        assertEquals("0,3,4,5,2,1,6", iterativeMapping.getMappingIndicesString());
    }

    @Test
    public void testThatBestShiftDoesNotMapInOrder() {
        final Mapping bestMapping = bestShiftFactory
                .getDistanceCalculatorFor(original, candidate)
                .finalMapping();

        System.out.println(costCalculator.calculateCost(bestMapping));

        assertEquals("6,0,1,2,3,4,5", bestMapping.getMappingIndicesString());
    }

    @Test
    public void testCostsOfMappings() {
        final Mapping defaultMapping = plainFactory
                .getDistanceCalculatorFor(original, candidate)
                .finalMapping();
        final Mapping bestMapping = bestShiftFactory
                .getDistanceCalculatorFor(original, candidate)
                .finalMapping();
        final Mapping iterativeMapping = iterativeShiftFactory
                .getDistanceCalculatorFor(original, candidate)
                .finalMapping();

        assertEquals(1.9402985074626866, costCalculator.calculateCost(defaultMapping), 0.00000000001);
        assertEquals(1.9402985074626866, costCalculator.calculateCost(bestMapping), 0.00000000001);
        assertEquals(7.032680363115146, costCalculator.calculateCost(iterativeMapping), 0.00000000001);
    }

    @Test
    public void testThatHeuristicsAreNotOptimal() {
        final List<MedianOffset> medianOffsets = ImmutableList.of(
                MedianOffset.including(true, false, false, false, false),
                MedianOffset.including(false, true, false, false, false),
                MedianOffset.including(false, false, true, false, false),
                MedianOffset.including(false, false, false, true, false),
                MedianOffset.including(false, false, false, false, true));
        for (final MedianOffset offset : medianOffsets) {
            final IterativeShiftFactory heuristic = this.createHeuristicsFactory(offset);
            final Mapping mapping = heuristic.getDistanceCalculatorFor(original, candidate).finalMapping();

            System.out.println(mapping.getMappingIndicesString());
            System.out.println(costCalculator.calculateCost(mapping));

            assertThat(costCalculator.calculateCost(mapping)).isGreaterThan(3.5);
        }
    }

    @Test
    public void testThatMonotoneMappingIsBetter() {
        final IterativeShiftFactory factory = IterativeShiftFactory.from(
                mapper, costCalculator, intervalDistance,
                MonotoneMapping.fromDistance(intervalDistance, 1),
                MinCostOffset.fromIntervalDistance(intervalDistance),
                new EmptyNeighborhood());
        final Mapping mapping = factory.getDistanceCalculatorFor(original, candidate).finalMapping();

        assertEquals("6,0,1,2,3,4,5", mapping.getMappingIndicesString());
        assertEquals(1.9402985074626866, costCalculator.calculateCost(mapping), 0.00000000001);
    }

    private IterativeShiftFactory createIterativeShiftFactory() {
        return IterativeShiftFactory.from(
                KuhnMunkres.create(),
                costCalculator,
                intervalDistance,
                new CentroidOffset(),
                MinCostOffset.fromIntervalDistance(intervalDistance),
                new EmptyNeighborhood());
    }

    private IterativeShiftFactory createHeuristicsFactory(final MedianOffset medianOffset) {
        return IterativeShiftFactory.from(
                mapper,
                costCalculator,
                intervalDistance,
                new CentroidOffset(),
                medianOffset,
                new LocalPerturbation());
    }
}
