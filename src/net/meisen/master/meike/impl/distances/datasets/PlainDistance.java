package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.matching.IDatasetMinCostMatcher;

/**
 * Allows calculating the plain distance between two {@link Dataset}s without
 * modifying any of their {@link Interval}s in any way.
 */
public class PlainDistance implements IDatasetDistance {

    private final IDatasetMinCostMatcher matcher;

    private PlainDistance(final IDatasetMinCostMatcher matcher) {
        this.matcher = matcher;
    }

    public static PlainDistance from(final IDatasetMinCostMatcher matcher) {
        return new PlainDistance(matcher);
    }

    @Override
    public double calculate(final Dataset original, final Dataset other) {
        return this.matcher.calculateMinimumCost(original, other);
    }
}
