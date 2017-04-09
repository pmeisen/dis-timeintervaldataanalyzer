package net.meisen.master.meike.impl.matching.hungarian;

import com.google.common.collect.ImmutableSet;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class TestKuhnMunkres {

    private Dataset originalDataset = new Dataset(ImmutableSet.of(
            new Interval(0L, 4L),
            new Interval(2L, 5L),
            new Interval(6L, 8L)));
    private Dataset sameLengthDataset = new Dataset(ImmutableSet.of(
            new Interval(1L, 3L),
            new Interval(2L, 7L),
            new Interval(5L, 9L)));
    private Dataset shorterDataset = new Dataset(ImmutableSet.of(
            new Interval(5L, 7L),
            new Interval(2L, 3L)));
    private Dataset longerDataset = new Dataset(ImmutableSet.of(
            new Interval(0L, 5L),
            new Interval(3L, 9L),
            new Interval(1L, 10L),
            new Interval(11L, 13L)));

    @Test
    public void testCalculationForSameLength() {
        final KuhnMunkres kuhnMunkres = KuhnMunkres.from(new FakeDistance());
        final double minimumCost = kuhnMunkres.calculateMinimumCost(
                this.originalDataset, this.sameLengthDataset);

        assertEquals(2.0, minimumCost, 0);
    }

    @Test
    public void testCalculationForShorter() {
        final KuhnMunkres kuhnMunkres = KuhnMunkres.from(new FakeDistance());
        final double minimumCost = kuhnMunkres.calculateMinimumCost(
                this.originalDataset, this.shorterDataset);

        assertEquals(6.0, minimumCost, 0);
    }

    @Test
    public void testCalculationForLonger() {
        final KuhnMunkres kuhnMunkres = KuhnMunkres.from(new FakeDistance());
        final double minimumCost = kuhnMunkres.calculateMinimumCost(
                this.originalDataset, this.longerDataset);

        assertEquals(15.0, minimumCost, 0);
    }
}
