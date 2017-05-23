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

    private long offset;

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
        this.offset = 0;
    }

    /**
     * @return the start time of this interval.
     */
    public long getStart() {
        return this.start + this.offset;
    }

    /**
     * @return the end time of this interval.
     */
    public long getEnd() {
        return this.end + this.offset;
    }

    /**
     * @return the length of this interval.
     */
    public long getLength() {
        return this.end - this.start;
    }

    public double getCentroid() {
        return (this.getEnd() + this.getStart()) / 2.0;
    }
    /**
     * Sets the offset of this interval to the given value.
     * @param offset
     *            the new offset value for the shift to use
     */
    public void setOffset(final long offset) {
        this.offset = offset;
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
        return max(0, min(this.getEnd(), other.getEnd()) - max(this.getStart(), other.getStart()));
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
        return max(this.getEnd(), other.getEnd()) - min(this.getStart(), other.getStart());
    }

    /**
     * Calculates the gap between this interval and the other one.
     *
     * @param other
     *            Another interval to which the distance is to be computed
     * @return the length of the gap between the two intervals
     */
    public long getGapBetweenThisAnd(final Interval other) {
        return max(0, max(this.getStart(), other.getStart()) - min(this.getEnd(), other.getEnd()));
    }

    @Override
    public String toString() {
        return '[' + millisToTimeString(this.start) +
                " - " + millisToTimeString(this.end) + ']';
    }

    private String millisToTimeString(final long milliseconds) {
        long second = (milliseconds / 1000) % 60;
        long minute = (milliseconds / (1000 * 60)) % 60;
        long hour = (milliseconds / (1000 * 60 * 60));

        return String.format("%02d:%02d:%02d", hour, minute, second);
    }
}
