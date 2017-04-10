package net.meisen.master.meike.impl.distances.intervals;

import static java.lang.Math.max;

/**
 * Allows calculating the distance value based on the length values of two
 * {@link Interval}s.
 */
public class LengthDistance implements IIntervalDistance {

    @Override
    public double calculate(final Interval original, final Interval other) {
        return 1 - (double) Math.min(original.getLength(), other.getLength()) /
                max(1.0, max(original.getLength(), other.getLength()));
    }
}
