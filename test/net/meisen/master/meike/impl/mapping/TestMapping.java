package net.meisen.master.meike.impl.mapping;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.exact.AbsoluteStartDistance;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.Assert.assertEquals;

public class TestMapping {
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

    @Nested
    class TestGetMappingCost {
        @Test
        public void isCorrectForSameLengthDataset() {
            final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, someDataset);

            final Mapping identityMapping = Mapping.create(ImmutableList.of(0, 1, 2, 3, 4), costMatrix);
            assertEquals(0, this.getMappingCost(identityMapping, 0), 0.000001);

            final Mapping offByOneMapping = Mapping.create(ImmutableList.of(1, 2, 3, 4, 0), costMatrix);
            assertEquals(8, this.getMappingCost(offByOneMapping, 0), 0.000001);
        }

        @Test
        public void isCorrectForLongerDataset() {
            final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, longerDataset);

            final Mapping identityMapping = Mapping.create(ImmutableList.of(0, 1, 2, 3, 4, 5), costMatrix);
            assertEquals(100, this.getMappingCost(identityMapping, 100), 0.000001);
        }

        @Test
        public void isCorrectForShorterDataset() {
            final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, shorterDataset);

            final Mapping identityMapping = Mapping.create(ImmutableList.of(0, 1, 2, 3, 4), costMatrix);
            assertEquals(200, this.getMappingCost(identityMapping, 100), 0.000001);
        }

        private double getMappingCost(final Mapping mapping, final double costForUnmapped) {
            return mapping.getMappingCosts().stream().mapToDouble(opt -> opt.orElse(costForUnmapped)).sum();
        }
    }

    @Nested
    class TestUnmappedIntervalsOfLargerDataset {
        @Test
        public void isCorrectForLongerDataset() {
            final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, longerDataset);

            final Mapping identityMapping = Mapping.create(ImmutableList.of(0, 1, 2, 3, 4, 5), costMatrix);
            final List<Interval> unmappedIntervals = identityMapping.getUnmappedIntervalsOfLargerDataset();
            assertEquals(1, unmappedIntervals.size());
            assertEquals(5L, unmappedIntervals.get(0).getStart());
        }

        @Test
        public void isCorrectForShorterDataset() {
            final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, shorterDataset);

            final Mapping identityMapping = Mapping.create(ImmutableList.of(0, 1, 2, 3, 4), costMatrix);
            final List<Interval> unmappedIntervals = identityMapping.getUnmappedIntervalsOfLargerDataset();
            assertEquals(2, unmappedIntervals.size());
            assertEquals(3L, unmappedIntervals.get(0).getStart());
            assertEquals(4L, unmappedIntervals.get(1).getStart());
        }

        @Test
        public void returnsNothingForSameLengthDataset() {
            final CostMatrix costMatrix = new CostMatrix(new AbsoluteStartDistance(), someDataset, someDataset);

            final Mapping identityMapping = Mapping.create(ImmutableList.of(0, 1, 2, 3, 4), costMatrix);
            final List<Interval> unmappedIntervals = identityMapping.getUnmappedIntervalsOfLargerDataset();
            assertEquals(0, unmappedIntervals.size());
        }
    }
}
