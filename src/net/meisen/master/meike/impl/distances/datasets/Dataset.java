package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.master.meike.impl.distances.intervals.Interval;

import java.util.Collection;
import java.util.Collections;

/**
 * Wrapper around a set of {@link Interval}s to facilitate operations on whole
 * datasets, e.g. calculating distances between two datasets.
 */
public class Dataset {
    private final Collection<Interval> intervals;

    /**
     * Creates a new dataset from the given {@link Interval}s.
     *
     * @param intervals
     *           The set of intervals this dataset consists of; must not be
     *           {@code null}.
     */
    public Dataset(final Collection<Interval> intervals) {
        assert null != intervals;

        this.intervals = intervals;
    }

    /**
     * @return an unmodifiable collection of all the intervals that make up this
     * dataset.
     */
    public Collection<Interval> getIntervals() {
        return Collections.unmodifiableCollection(this.intervals);
    }
}
