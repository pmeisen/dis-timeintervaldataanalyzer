package net.meisen.master.meike.impl.distances.intervals;

import static java.lang.Math.min;

/**
 * Calculates a distance based on the ratio between two {@link Interval}s'
 * intersection length and their combined length.
 */
public class IntersectionDistance implements IIntervalDistance {

    @Override
    public double calculate(final Interval original, final Interval other) {
        return 1 - (double) original.getLengthOfIntersectionWith(other) /
                min(original.getLength(), other.getLength());
    }
}
