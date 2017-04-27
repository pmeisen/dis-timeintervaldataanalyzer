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

public class TestBestShiftDistance {
    private final Dataset original = new Dataset(ImmutableList.of(
            new Interval(0L, 4L),
            new Interval(2L, 7L),
            new Interval(9L, 12L),
            new Interval(5L, 50L),
            new Interval(10L, 12L)));
    private final Dataset shiftedByThree = new Dataset(ImmutableList.of(
            new Interval(3L, 7L),
            new Interval(5L, 10L),
            new Interval(12L, 15L),
            new Interval(8L, 53L),
            new Interval(13L, 15L)));

    private IDatasetMinCostMapper createKuhnMunkresMatcher() {
        final Map<IIntervalDistance, Double> distances = new HashMap<>();
        distances.put(new EndDistance(), 1.0);
        distances.put(new LengthDistance(), 1.0);
        distances.put(new StartDistance(), 1.0);
        return KuhnMunkres.from(new WeightedSumDistance(distances));
    }

    @Test
    public void testThatShiftedIntervalGivesZeroDistance() {
        final IDatasetMinCostMapper matcher = this.createKuhnMunkresMatcher();
        final IDatasetDistance distance = BestShiftDistance.from(matcher);

        final double bestDistance = distance.calculate(original, shiftedByThree);
        assertEquals(0.0, bestDistance, 0);
    }
}
