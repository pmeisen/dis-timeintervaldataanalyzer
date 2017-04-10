package net.meisen.master.meike.impl.distances.intervals;

import org.junit.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.assertEquals;

public class TestWeightedSumDistance {

    private final Interval someInterval = new Interval(1L, 2L);
    private final Interval anotherInterval = new Interval(20L, 37L);

    @Test
    public void testWeightsAreUsedCorrectly() {
        final Map<IIntervalDistance, Double> distances = new HashMap<>();
        distances.put(new ConstantDistance(1), 1.0);
        distances.put(new ConstantDistance(10), 2.0);
        distances.put(new ConstantDistance(100), 3.0);
        final WeightedSumDistance distance = new WeightedSumDistance(distances);

        assertEquals(321.0, distance.calculate(this.someInterval, this.anotherInterval), 0);
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
