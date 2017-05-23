package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Calculates an offset for the {@code other} {@link Dataset} by aligning the
 * centroids of both datasets, resp. the mapped intervals.
 */
public class CentroidOffset implements IInitialOffsetCalculator, INextOffsetCalculator {
    @Override
    public List<Long> calculate(final Dataset original, final Dataset other) {
        return ImmutableList.of(Math.round(this.calculateCentroid(original)
                - this.calculateCentroid(other)));
    }

    /**
     * This calculates the median of the interval offsets and is thus different
     * from the centroid-distance of the whole datasets.
     */
    @Override
    public List<Long> calculate(final Mapping mapping) {
        List<Double> offsets = mapping.getPairs().stream()
                .map(p -> p.getKey().getCentroid() - p.getValue().getCentroid())
                .sorted()
                .collect(Collectors.toList());
        return ImmutableList.of(getMedian(offsets));
    }

    private long getMedian(List<Double> offsets) {
        int numberOfPairs = offsets.size();
        if (numberOfPairs % 2 == 0) {
            return Math.round((offsets.get(numberOfPairs / 2)
                    + offsets.get(numberOfPairs / 2 - 1)) / 2);
        } else {
            return Math.round(offsets.get(numberOfPairs / 2));
        }
    }

    private double calculateCentroid(final Dataset dataset) {
        double sumOfIntervalCentroids = dataset.getIntervals().stream()
                .mapToDouble(Interval::getCentroid)
                .sum();
        long numberOfIntervals = dataset.getIntervals().size();
        return sumOfIntervalCentroids / numberOfIntervals;
    }
}
