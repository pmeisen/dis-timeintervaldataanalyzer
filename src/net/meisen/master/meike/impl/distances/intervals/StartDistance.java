package net.meisen.master.meike.impl.distances.intervals;

/**
 * Calculates a distance value based on the difference of the two
 * {@link Interval}s' start times.
 */
public class StartDistance implements IIntervalDistance {

    @Override
    public double calculate(final Interval original, final Interval other) {
        return (double) Math.abs(original.getStart() - other.getStart()) /
                original.getTotalLengthCombinedWith(other);
    }
}
