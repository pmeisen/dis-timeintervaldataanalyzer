package net.meisen.master.meike.impl.mapping.costCalculation;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.exact.AbsoluteStartDistance;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class TestConstantCostForUnmappedIntervals {
    private final Dataset someDataset = new Dataset(ImmutableList.of(
            new Interval(0L, 0L),
            new Interval(1L, 1L),
            new Interval(2L, 2L),
            new Interval(3L, 3L),
            new Interval(4L, 4L)));
    private final Dataset longerDataset = new Dataset(ImmutableList.of(
            new Interval(0L, 0L),
            new Interval(1L, 1L),
            new Interval(2L, 2L),
            new Interval(3L, 3L),
            new Interval(4L, 4L),
            new Interval(5L, 5L)));
    private final Dataset shorterDataset = new Dataset(ImmutableList.of(
            new Interval(0L, 0L),
            new Interval(1L, 1L),
            new Interval(2L, 2L)));

    @Test
    public void testIdentityMappingGivesNoCost() {
        final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(1.111);
        final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, someDataset);
        final Mapping mapping = Mapping.create(ImmutableList.of(0, 1, 2, 3, 4), costMatrix);

        final double cost = costCalculator.calculateCost(mapping);
        assertEquals(0, cost, 0.000001);
    }

    @Test
    public void testLongerDatasetGivesCorrectNumberOfDummyCosts() {
        final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(2.222);
        final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, longerDataset);
        final Mapping mapping = Mapping.create(ImmutableList.of(0, 1, 2, 3, 4, 5), costMatrix);

        final double cost = costCalculator.calculateCost(mapping);
        assertEquals(2.222, cost, 0.000001);
    }

    @Test
    public void testShorterDatasetGivesCorrectNumberOfDummyCosts() {
        final ICostCalculator costCalculator = ConstantCostForUnmappedIntervals.fromCost(3.333);
        final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, shorterDataset);
        final Mapping mapping = Mapping.create(ImmutableList.of(0, 1, 2, 3, 4), costMatrix);

        final double cost = costCalculator.calculateCost(mapping);
        assertEquals(6.666, cost, 0.000001);
    }
}
