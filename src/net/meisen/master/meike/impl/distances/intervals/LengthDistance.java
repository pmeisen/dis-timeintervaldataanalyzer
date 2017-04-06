package net.meisen.master.meike.impl.distances.intervals;

/**
 * Allows calculating the distance value based on the length values of two
 * {@link Interval}s.
 */
public class LengthDistance implements IIntervalDistance {

    @Override
    public double calculate(final Interval original, final Interval other) {
        return 1 - (double) Math.min(original.getLength(), other.getLength()) /
                Math.max(original.getLength(), other.getLength());
    }
}
