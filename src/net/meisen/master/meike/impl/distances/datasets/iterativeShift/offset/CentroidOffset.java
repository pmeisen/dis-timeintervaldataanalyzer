package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;

import java.util.List;

/**
 * Calculates an offset for the {@code other} {@link Dataset} by aligning the
 * centroids of both datasets.
 */
public class CentroidOffset implements IInitialOffsetCalculator {
    @Override
    public List<Long> calculate(final Dataset original, final Dataset other) {
        other.setOffset(0);

        return ImmutableList.of(this.getDistanceOfCentroids(original, other));
    }

    private double calculateCentroid(final Dataset dataset) {
        final double sumOfIntervalCentroids = dataset.getIntervals().stream()
                .mapToDouble(Interval::getCentroid)
                .sum();
        final long numberOfIntervals = dataset.getIntervals().size();
        return sumOfIntervalCentroids / numberOfIntervals;
    }

    private long getDistanceOfCentroids(final Dataset original, final Dataset other) {
        return Math.round(this.calculateCentroid(original)
                - this.calculateCentroid(other));
    }
}
