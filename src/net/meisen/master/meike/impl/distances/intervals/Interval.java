package net.meisen.master.meike.impl.distances.intervals;

import static java.lang.Math.max;
import static java.lang.Math.min;

/**
 * Wrapper around the start and end time of an interval that simplifies
 * calculating certain distance values.
 */
public class Interval {
    private final long start;
    private final long end;

    /**
     * Creates a new interval from the given {@code start} and {@code end}
     * values.
     *
     * @param start
     *           The start value of the interval; must be non-negative.
     * @param end
     *           The end value of the interval; must be at least {@code start}.
     */
    public Interval(final long start, final long end) {
        assert start >= 0;
        assert end >= start;

        this.start = start;
        this.end = end;
    }

    /**
     * @return the start time of this interval.
     */
    public long getStart() {
        return this.start;
    }

    /**
     * @return the end time of this interval.
     */
    public long getEnd() {
        return this.end;
    }

    /**
     * @return the length of this interval.
     */
    public long getLength() {
        return this.end - this.start;
    }

    /**
     * Calculates the length of the intersection of this interval with another
     * one.
     *
     * @param other
     *            Another interval with which to intersect this one
     * @return the length of the intersection of the two intervals
     */
    public long getLengthOfIntersectionWith(final Interval other) {
        return max(0, min(this.end, other.end) - max(this.start, other.start));
    }

    /**
     * Calculates the total time between the start of the first interval and
     * the end of the last interval.
     *
     * @param other
     *            Another interval with which to combine this one
     * @return the length of the time needed to complete both intervals
     */
    public long getTotalLengthCombinedWith(final Interval other) {
        return max(this.end, other.end) - min(this.start, other.start);
    }

    /**
     * Calculates the gap between this interval and the other one.
     *
     * @param other
     *            Another interval to which the distance is to be computed
     * @return the length of the gap between the two intervals
     */
    public long getGapBetweenThisAnd(final Interval other) {
        return max(0, max(this.start, other.start) - min(this.end, other.end));
    }
}
