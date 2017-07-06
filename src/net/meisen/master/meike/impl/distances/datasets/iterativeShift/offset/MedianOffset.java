package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Super class for next offset calculators that take the median of some offsets.
 */
public class MedianOffset implements INextOffsetCalculator {
    private final boolean useStandard;
    private final boolean useLargest;
    private final boolean useSmallest;
    private final boolean useMostExtreme;
    private final boolean useLeastExtreme;

    private MedianOffset(final boolean useStandard, final boolean useLargest,
                         final boolean useSmallest, final boolean useMostExtreme,
                         final boolean useLeastExtreme) {
        this.useStandard = useStandard;
        this.useLargest = useLargest;
        this.useSmallest = useSmallest;
        this.useMostExtreme = useMostExtreme;
        this.useLeastExtreme = useLeastExtreme;
    }

    public static MedianOffset including(final boolean useStandard,
                                         final boolean useLargest,
                                         final boolean useSmallest,
                                         final boolean useMostExtreme,
                                         final boolean useLeastExtreme) {
        return new MedianOffset(useStandard, useLargest, useSmallest, useMostExtreme, useLeastExtreme);
    }

    public static MedianOffset usingAll() {
        return new MedianOffset(true, true, true, true, true);
    }

    @Override
    public List<Long> calculate(final Mapping mapping) {
        final List<Double> allCenterDistances = this.getCenterDistances(mapping);
        final int length = allCenterDistances.size();

        final List<Long> offsets = new LinkedList<>();
        if (this.useStandard) {
            offsets.add(this.getMedian(allCenterDistances));
        }
        if (this.useLargest) {
            offsets.add(this.getMedian(allCenterDistances.subList(0, length / 2)));
        }
        if (this.useSmallest) {
            offsets.add(this.getMedian(allCenterDistances.subList(length / 2, length)));
        }
        if (this.useMostExtreme) {
            offsets.add(this.getMedian(this.mostExtreme(allCenterDistances)));
        }
        if (this.useLeastExtreme) {
            offsets.add(this.getMedian(this.leastExtreme(allCenterDistances)));
        }
        return offsets.stream().map(o -> o + mapping.getOffset()).collect(Collectors.toList());
    }

    private List<Double> getCenterDistances(final Mapping mapping) {
        return mapping.getPairs().stream()
                .map(p -> p.getKey().getCentroid() - p.getValue().getCentroid())
                .sorted()
                .collect(Collectors.toList());
    }

    private List<Double> mostExtreme(final List<Double> values) {
        return values.stream()
                .sorted(Comparator.comparing(Math::abs))
                .skip(values.size() / 2)
                .collect(Collectors.toList());
    }

    private List<Double> leastExtreme(final List<Double> values) {
        return values.stream()
                .sorted(Comparator.comparing(Math::abs))
                .limit(values.size() / 2)
                .collect(Collectors.toList());
    }

    private long getMedian(final List<Double> offsets) {
        final int numberOfPairs = offsets.size();
        if (numberOfPairs % 2 == 0) {
            return Math.round((offsets.get(numberOfPairs / 2)
                    + offsets.get(numberOfPairs / 2 - 1)) / 2);
        } else {
            return Math.round(offsets.get(numberOfPairs / 2));
        }
    }
}
