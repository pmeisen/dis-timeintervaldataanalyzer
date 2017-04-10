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

    /**
     * Creates a new instance using the given matcher for distance calculation.
     *
     * @param matcher
     *           the matcher to calculate the distance between two datasets;
     *           must not be {@code null}.
     * @return an instance of this class that uses the given matcher
     */
    public static PlainDistance from(final IDatasetMinCostMatcher matcher) {
        assert null != matcher;

        return new PlainDistance(matcher);
    }

    @Override
    public double calculate(final Dataset original, final Dataset other) {
        assert null != original;
        assert null != other;

        return this.matcher.calculateMinimumCost(original, other);
    }
}
