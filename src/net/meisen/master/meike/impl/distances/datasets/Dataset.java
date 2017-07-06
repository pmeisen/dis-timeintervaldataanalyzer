package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.master.meike.impl.distances.intervals.Interval;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Wrapper around a set of {@link Interval}s to facilitate operations on whole
 * datasets, e.g. calculating distances between two datasets.
 */
public class Dataset {
    private final List<Interval> intervals;
    private final String id;

    /**
     * Creates a new {@link Dataset} from the given {@link Interval}s.
     *
     * @param intervals
     *           The intervals that make up this dataset; must not be
     *           {@code null}.
     */
    public Dataset(final List<Interval> intervals, final String id) {
        assert null != intervals;
        assert null != id;

        this.intervals = intervals.stream()
                .sorted(Comparator.comparing(Interval::getCentroid))
                .collect(Collectors.toList());
        this.id = id;
    }

    /**
     * Creates a new {@link Dataset} from the given {@link Interval}s.
     *
     * @param intervals
     *           The intervals that make up this dataset; must not be
     *           {@code null}.
     */
    public Dataset(final List<Interval> intervals) {
        assert null != intervals;

        this.intervals = intervals;
        this.id = "unknown";
    }

    /**
     * @return an unmodifiable collection of all the intervals that make up this
     * dataset.
     */
    public List<Interval> getIntervals() {
        return Collections.unmodifiableList(this.intervals);
    }

    /**
     * @return the number of intervals contained in this dataset.
     */
    public int getNumberOfIntervals() {
        return this.intervals.size();
    }

    /**
     * Shifts all intervals of this dataset by the given value.
     *
     * @param offset
     *           the value to be used for shifting all the intervals
     */
    public void setOffset(final long offset) {
        for (final Interval interval : this.intervals) {
            interval.setOffset(offset);
        }
    }

    public String getId() {
        return this.id;
    }
}
