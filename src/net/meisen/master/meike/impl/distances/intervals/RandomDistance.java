package net.meisen.master.meike.impl.distances.intervals;

/**
 * Dummy distance implementation that just returns random values.
 */
public class RandomDistance implements IIntervalDistance {
    @Override
    public double calculate(Interval original, Interval other) {
        return Math.random();
    }
}
