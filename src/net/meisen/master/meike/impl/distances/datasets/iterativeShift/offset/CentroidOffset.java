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
        original.setOffset(0);
        other.setOffset(0);

        return ImmutableList.of(Math.round(this.calculateCentroid(original)
                - this.calculateCentroid(other)));
    }

    private double calculateCentroid(final Dataset dataset) {
        double sumOfIntervalCentroids = dataset.getIntervals().stream()
                .mapToDouble(Interval::getCentroid)
                .sum();
        long numberOfIntervals = dataset.getIntervals().size();
        return sumOfIntervalCentroids / numberOfIntervals;
    }
}
