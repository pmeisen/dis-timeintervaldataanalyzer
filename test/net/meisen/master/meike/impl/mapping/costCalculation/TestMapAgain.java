package net.meisen.master.meike.impl.mapping.costCalculation;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.Mapping;
import net.meisen.master.meike.impl.mapping.exact.AbsoluteStartDistance;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import static org.junit.Assert.assertEquals;

public class TestMapAgain {
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
    private final Dataset muchLongerDataset = new Dataset(ImmutableList.of(
            new Interval(0L, 0L),
            new Interval(1L, 1L),
            new Interval(2L, 2L),
            new Interval(3L, 3L),
            new Interval(4L, 4L),
            new Interval(5L, 5L),
            new Interval(6L, 6L),
            new Interval(7L, 7L),
            new Interval(8L, 8L),
            new Interval(9L, 9L),
            new Interval(10L, 10L),
            new Interval(11L, 11L)));
    private final Dataset shorterDataset = new Dataset(ImmutableList.of(
            new Interval(0L, 0L),
            new Interval(1L, 1L),
            new Interval(2L, 2L)));
    private final Dataset muchShorterDataset = new Dataset(ImmutableList.of(
            new Interval(2L, 2L)));

    @Nested
    class SameLengthDataset {
        @Test
        public void identityMappingGivesNoCost() {
            final ICostCalculator costCalculator = MapAgain.from(new AbsoluteStartDistance(), 5.7);

            final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, someDataset);
            final Mapping mapping = Mapping.create(ImmutableList.of(0, 1, 2, 3, 4), costMatrix);

            final double cost = costCalculator.calculateCost(mapping);
            assertEquals(0, cost, 0.000001);
        }

        @Test
        public void offByOneMappingCorrect() {
            final ICostCalculator costCalculator = MapAgain.from(new AbsoluteStartDistance(), 5.7);

            final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, someDataset);
            final Mapping mapping = Mapping.create(ImmutableList.of(1, 2, 3, 4, 0), costMatrix);

            final double cost = costCalculator.calculateCost(mapping);
            assertEquals(8, cost, 0.000001);
        }
    }

    @Nested
    class LongerDataset {
        @Test
        public void costForAdditionalIntervalIsOne() {
            final ICostCalculator costCalculator = MapAgain.from(new AbsoluteStartDistance(), 5.7);

            final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, longerDataset);
            final Mapping mapping = Mapping.create(ImmutableList.of(0, 1, 2, 3, 4, 5), costMatrix);

            final double cost = costCalculator.calculateCost(mapping);
            assertEquals(1, cost, 0.000001);
        }

        @Test
        public void muchLongerDatasetHasTwoFinallyUnmapped() {
            final ICostCalculator costCalculator = MapAgain.from(new AbsoluteStartDistance(), 1000);

            final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, muchLongerDataset);
            final Mapping mapping = Mapping.create(ImmutableList.of(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), costMatrix);

            final double cost = costCalculator.calculateCost(mapping);
            assertEquals(2025, cost, 0.000001);
        }
    }

    @Nested
    class ShorterDataset {
        @Test
        public void costForMissingIntervalsIsFour() {
            final ICostCalculator costCalculator = MapAgain.from(new AbsoluteStartDistance(), 5.7);

            final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, shorterDataset);
            final Mapping mapping = Mapping.create(ImmutableList.of(0, 1, 2, 3, 4), costMatrix);

            final double cost = costCalculator.calculateCost(mapping);
            assertEquals(4, cost, 0.000001);
        }

        @Test
        public void muchShorterDatasetHasThreeFinallyUnmapped() {
            final ICostCalculator costCalculator = MapAgain.from(new AbsoluteStartDistance(), 1000);

            final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, muchShorterDataset);
            final Mapping mapping = Mapping.create(ImmutableList.of(1, 2, 0, 3, 4), costMatrix);

            final double cost = costCalculator.calculateCost(mapping);
            assertEquals(3001, cost, 0.000001);
        }
    }
}
