package net.meisen.master.meike.impl.distances.intervals;

import static java.lang.Math.max;

/**
 * Calculates a distance value based on the difference of the two
 * {@link Interval}s' end times.
 */
public class EndDistance implements IIntervalDistance {

    @Override
    public double calculate(final Interval original, final Interval other) {
        return (double) Math.abs(original.getEnd() - other.getEnd()) /
                max(1, original.getTotalLengthCombinedWith(other));
    }
}
