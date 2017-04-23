package net.meisen.master.meike.impl.distances.intervals;

/**
 * Dummy distance that always returns the same constant value.
 */
public class ConstantDistance implements IIntervalDistance {
    @Override
    public double calculate(Interval original, Interval other) {
        return 1;
    }
}
