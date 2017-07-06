package net.meisen.master.meike.impl.mapping.exact;

import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;

/**
 * Very simple distance measure to be used in the mapping tests.
 */
public class AbsoluteStartDistance implements IIntervalDistance {
    @Override
    public double calculate(Interval original, Interval other) {
        return Math.abs(original.getStart() - other.getStart());
    }
}