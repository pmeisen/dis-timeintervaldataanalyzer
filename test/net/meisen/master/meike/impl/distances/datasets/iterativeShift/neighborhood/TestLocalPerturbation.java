package net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.Mapping;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

public class TestLocalPerturbation {
    @Test
    public void testOriginalIsShorter() {
        final double[][] costs = new double[][]{
                {1, 0, 1, 1, 1, 1},
                {1, 1, 1, 0, 1, 1},
                {1, 1, 1, 1, 1, 0},
                {1, 1, 1, 1, 0, 1},
                {1, 1, 1, 1, 1, 1},
                {1, 1, 1, 1, 1, 1}
        };
        final Dataset originalDataset = new Dataset(ImmutableList.of(new Interval(0, 0),
                new Interval(0, 0), new Interval(0, 0), new Interval(0, 0)));
        final Dataset otherDataset = new Dataset(ImmutableList.of(new Interval(0, 0),
                new Interval(0, 0), new Interval(0, 0), new Interval(0, 0), new Interval(0, 0), new Interval(0, 0)));

        final LocalPerturbation local = new LocalPerturbation();
        final CostMatrix costMatrix = CostMatrix.fromMatrix(costs, originalDataset, otherDataset);
        final Mapping originalMapping = Mapping.create(ImmutableList.of(0, 1, 2, 3, 4, 5), costMatrix);
        final Mapping newMapping = local.getNeighborStartingFrom(0, originalMapping);

        Assertions.assertArrayEquals(new Integer[]{1, 3, 2, 4, 0, 5},
                newMapping.getMappingIndices().toArray(new Integer[6]));
    }

    @Test
    public void testOriginalIsLonger() {
        final double[][] costs = new double[][] {
                {1, 1, 0, 1},
                {1, 1, 1, 1},
                {0, 1, 1, 1},
                {0, 0, 0, 1}
        };
        final Dataset originalDataset = new Dataset(ImmutableList.of(new Interval(0, 0),
                new Interval(0, 0), new Interval(0, 0), new Interval(0, 0)));
        final Dataset otherDataset = new Dataset(ImmutableList.of(new Interval(0, 0),
                new Interval(0, 0), new Interval(0, 0)));

        final LocalPerturbation local = new LocalPerturbation();
        final CostMatrix costMatrix = CostMatrix.fromMatrix(costs, originalDataset, otherDataset);
        final Mapping originalMapping = Mapping.create(ImmutableList.of(0, 1, 2, 3), costMatrix);
        final Mapping newMapping = local.getNeighborStartingFrom(2, originalMapping);

        Assertions.assertArrayEquals(new Integer[] {2, 1, 0, 3},
                newMapping.getMappingIndices().toArray(new Integer[4]));
    }

    @Test
    public void testSameLength() {
        final double[][] costs = new double[][] {
                {1, 0, 1, 1},
                {1, 1, 0, 1},
                {1, 1, 1, 0},
                {0, 1, 1, 1}
        };
        final Dataset originalDataset = new Dataset(ImmutableList.of(new Interval(0, 0),
                new Interval(0, 0), new Interval(0, 0), new Interval(0, 0)));
        final Dataset otherDataset = new Dataset(ImmutableList.of(new Interval(0, 0),
                new Interval(0, 0), new Interval(0, 0), new Interval(0, 0)));

        final LocalPerturbation local = new LocalPerturbation();
        final CostMatrix costMatrix = CostMatrix.fromMatrix(costs, originalDataset, otherDataset);
        final Mapping originalMapping = Mapping.create(ImmutableList.of(0, 1, 2, 3), costMatrix);
        final Mapping newMapping = local.getNeighborStartingFrom(2, originalMapping);

        Assertions.assertArrayEquals(new Integer[] {1, 2, 3, 0},
                newMapping.getMappingIndices().toArray(new Integer[4]));
    }

    @Test
    public void testNoChange() {
        final double[][] costs = new double[][] {
                {0, 1, 1, 1},
                {1, 0, 1, 1},
                {1, 1, 0, 1},
                {1, 1, 1, 0}
        };
        final Dataset originalDataset = new Dataset(ImmutableList.of(new Interval(0, 0),
                new Interval(0, 0), new Interval(0, 0), new Interval(0, 0)));
        final Dataset otherDataset = new Dataset(ImmutableList.of(new Interval(0, 0),
                new Interval(0, 0), new Interval(0, 0), new Interval(0, 0)));

        final LocalPerturbation local = new LocalPerturbation();
        final CostMatrix costMatrix = CostMatrix.fromMatrix(costs, originalDataset, otherDataset);
        final Mapping originalMapping = Mapping.create(ImmutableList.of(0, 1, 2, 3), costMatrix);
        final Mapping newMapping = local.getNeighborStartingFrom(2, originalMapping);

        Assertions.assertArrayEquals(new Integer[] {0, 1, 2, 3},
                newMapping.getMappingIndices().toArray(new Integer[4]));
    }
}
