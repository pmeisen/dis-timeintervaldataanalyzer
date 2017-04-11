package net.meisen.master.meike.impl.distances.intervals;

import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;

public class TestWeightedSumDistance {

    private final Interval someInterval = new Interval(1L, 2L);
    private final Interval anotherInterval = new Interval(20L, 37L);

    private final Interval someEmptyInterval = new Interval(5L, 5L);
    private final Interval anotherEmptyInterval = new Interval(10L, 10L);

    @Test
    public void testWeightsAreUsedCorrectly() {
        final Map<IIntervalDistance, Double> distances = new HashMap<>();
        distances.put(new ConstantDistance(1), 1.0);
        distances.put(new ConstantDistance(10), 2.0);
        distances.put(new ConstantDistance(100), 3.0);
        final WeightedSumDistance distance = new WeightedSumDistance(distances);

        assertEquals(321.0, distance.calculate(this.someInterval, this.anotherInterval), 0);
    }

    @Test
    public void testCombinationOfAllWeightsGivesValidResultForEmptyIntervals() {
        final Map<IIntervalDistance, Double> distances = new HashMap<>();
        distances.put(new EndDistance(), 1.0);
        distances.put(new EndPositionDistance(), 1.0);
        distances.put(new GapDistance(), 1.0);
        distances.put(new IntersectionDistance(), 1.0);
        distances.put(new LengthDistance(), 1.0);
        distances.put(new StartDistance(), 1.0);
        distances.put(new StartPositionDistance(), 1.0);
        final WeightedSumDistance distance = new WeightedSumDistance(distances);

        assertEquals(1.0, distance.calculate(this.someEmptyInterval, this.someEmptyInterval), 0);
        assertEquals(5.0, distance.calculate(this.someEmptyInterval, this.anotherEmptyInterval), 0);
    }

    private class ConstantDistance implements IIntervalDistance {
        private final double result;

        public ConstantDistance(final double result) {
            this.result = result;
        }

        @Override
        public double calculate(Interval original, Interval other) {
            return this.result;
        }
    }
}
