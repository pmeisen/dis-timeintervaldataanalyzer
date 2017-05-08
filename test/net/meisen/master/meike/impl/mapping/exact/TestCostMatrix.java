package net.meisen.master.meike.impl.mapping.exact;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import org.junit.Test;

import java.util.Arrays;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;

public class TestCostMatrix {

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
    public void testCostMatrixCreationForSameLength() {
        final CostMatrix costMatrix = new CostMatrix(new FakeDistance(),
                this.originalDataset, this.sameLengthDataset);

        final String expected = "[1.0, 2.0, 5.0]\n" +
                                "[1.0, 0.0, 3.0]\n" +
                                "[5.0, 4.0, 1.0]";

        assertEquals(expected, this.toString(costMatrix.getCosts()));
    }

    @Test
    public void testCostMatrixCreationForShorter() {
        final CostMatrix costMatrix = new CostMatrix(new FakeDistance(),
                this.originalDataset, this.shorterDataset);

        final String expected = "[5.0, 2.0, 5.0]\n" +
                                "[3.0, 0.0, 5.0]\n" +
                                "[1.0, 4.0, 5.0]";

        assertEquals(expected, this.toString(costMatrix.getCosts()));
    }

    @Test
    public void testCostMatrixCreationForLonger() {
        final CostMatrix costMatrix = new CostMatrix(new FakeDistance(),
                this.originalDataset, this.longerDataset);

        final String expected = "[0.0, 3.0, 1.0, 11.0]\n" +
                                "[2.0, 1.0, 1.0, 9.0]\n" +
                                "[6.0, 3.0, 5.0, 5.0]\n" +
                                "[11.0, 11.0, 11.0, 11.0]";

        assertEquals(expected, this.toString(costMatrix.getCosts()));
    }

    private String toString(final double[][] matrix) {
        return Arrays.stream(matrix)
                .map(Arrays::toString)
                .collect(Collectors.joining(System.lineSeparator()));
    }
}
