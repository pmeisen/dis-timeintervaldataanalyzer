package net.meisen.master.meike.impl.mapping.exact;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.costCalculation.CompleteMatrix;
import net.meisen.master.meike.impl.mapping.costCalculation.OnlyMatchedIntervals;
import net.meisen.master.meike.impl.mapping.MappingFactory;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class TestKuhnMunkres {

    private Dataset originalDataset = new Dataset(ImmutableList.of(
            new Interval(0L, 4L),
            new Interval(2L, 5L),
            new Interval(6L, 8L)));
    private Dataset sameLengthDataset = new Dataset(ImmutableList.of(
            new Interval(1L, 3L),
            new Interval(2L, 7L),
            new Interval(5L, 9L)));
    private Dataset shorterDataset = new Dataset(ImmutableList.of(
            new Interval(5L, 7L),
            new Interval(2L, 3L)));
    private Dataset longerDataset = new Dataset(ImmutableList.of(
            new Interval(0L, 5L),
            new Interval(3L, 9L),
            new Interval(1L, 10L),
            new Interval(11L, 13L)));

    @Test
    public void testCalculationForSameLength() {
        final KuhnMunkres kuhnMunkres =
                KuhnMunkres.from(MappingFactory.from(new OnlyMatchedIntervals()));
        final CostMatrix costMatrix =
                new CostMatrix(new FakeDistance(), this.originalDataset, this.sameLengthDataset);
        final double minimumCost =
                kuhnMunkres.calculateMinimumCostMapping(costMatrix).getCost();

        assertEquals(2.0, minimumCost, 0);
    }

    @Test
    public void testCalculationForShorter() {
        final KuhnMunkres kuhnMunkres =
                KuhnMunkres.from(MappingFactory.from(new CompleteMatrix()));
        final CostMatrix costMatrix =
                new CostMatrix(new FakeDistance(), this.originalDataset, this.shorterDataset);
        final double minimumCost =
                kuhnMunkres.calculateMinimumCostMapping(costMatrix).getCost();

        assertEquals(6.0, minimumCost, 0);
    }

    @Test
    public void testCalculationForLonger() {
        final KuhnMunkres kuhnMunkres =
                KuhnMunkres.from(MappingFactory.from(new CompleteMatrix()));
        final CostMatrix costMatrix =
                new CostMatrix(new FakeDistance(), this.originalDataset, this.longerDataset);
        final double minimumCost =
                kuhnMunkres.calculateMinimumCostMapping(costMatrix).getCost();

        assertEquals(15.0, minimumCost, 0);
    }
}
