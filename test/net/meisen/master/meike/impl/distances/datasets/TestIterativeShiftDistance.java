package net.meisen.master.meike.impl.distances.datasets;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.intervals.EndDistance;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.distances.intervals.LengthDistance;
import net.meisen.master.meike.impl.distances.intervals.StartDistance;
import net.meisen.master.meike.impl.distances.intervals.WeightedSumDistance;
import net.meisen.master.meike.impl.matching.IDatasetMinCostMapper;
import net.meisen.master.meike.impl.matching.hungarian.KuhnMunkres;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;

public class TestIterativeShiftDistance {
    private final Dataset original = new Dataset(ImmutableList.of(
            new Interval(0L, 3L),
            new Interval(5L, 11L),
            new Interval(8L, 11L),
            new Interval(10L, 14L),
            new Interval(14L, 16L),
            new Interval(17L, 20L)));
    private final Dataset shiftedByTen = new Dataset(ImmutableList.of(
            new Interval(10L, 13L),
            new Interval(15L, 21L),
            new Interval(18L, 21L),
            new Interval(20L, 24L),
            new Interval(24L, 26L),
            new Interval(27L, 30L)));
    private final Dataset shiftedByTenPlusExtra = new Dataset(ImmutableList.of(
            new Interval(10L, 13L),
            new Interval(15L, 21L),
            new Interval(18L, 21L),
            new Interval(20L, 24L),
            new Interval(24L, 26L),
            new Interval(27L, 30L),
            new Interval(32L, 40L),
            new Interval(35L, 40L)));

    private IIntervalDistance createIntervalDistance() {
        final Map<IIntervalDistance, Double> distances = new HashMap<>();
        distances.put(new EndDistance(), 1.0);
        distances.put(new LengthDistance(), 1.0);
        distances.put(new StartDistance(), 1.0);
        return new WeightedSumDistance(distances);
    }

    @Test
    public void testShiftedCandidateGivesZeroDistance() {
        final IIntervalDistance distanceMeasure = this.createIntervalDistance();
        final IDatasetMinCostMapper matcher = KuhnMunkres.from(distanceMeasure);
        final IDatasetDistance datasetDistance =
                IterativeShiftDistance.from(matcher, distanceMeasure);

        final double bestDistance = datasetDistance.calculate(original, shiftedByTen);
        assertEquals(0.0, bestDistance, 0);
    }

    @Test
    public void testShiftedCandidateWithExtra() {
        final IIntervalDistance distanceMeasure = this.createIntervalDistance();
        final IDatasetMinCostMapper matcher = KuhnMunkres.from(distanceMeasure);
        final IDatasetDistance datasetDistance =
                IterativeShiftDistance.from(matcher, distanceMeasure);

        final double bestDistance = datasetDistance.calculate(original, shiftedByTenPlusExtra);
        assertEquals(9.793518518518518, bestDistance, 0.0001);
    }
}
