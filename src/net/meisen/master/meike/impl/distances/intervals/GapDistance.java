package net.meisen.master.meike.impl.distances.intervals;


/**
 * Calculates a distance value based on the gap between the two
 * {@link Interval}s: If the intervals overlap, this distance is zero,
 * otherwise it is the distance between the end of the first interval and the
 * start of the later interval.
 */
public class GapDistance implements IIntervalDistance {

    @Override
    public double calculate(final Interval original, final Interval other) {
        return (double) original.getGapBetweenThisAnd(other) /
                original.getTotalLengthCombinedWith(other);
    }
}
