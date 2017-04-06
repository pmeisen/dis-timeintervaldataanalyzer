package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.master.meike.impl.distances.intervals.Interval;

/**
 * Allows calculating the plain distance between two {@link Dataset}s without
 * modifying any of their {@link Interval}s in any way.
 */
public class PlainDistance implements IDatasetDistance {

    @Override
    public double calculate(final Dataset original, final Dataset other) {
        // Todo
        return 0;
    }
}
