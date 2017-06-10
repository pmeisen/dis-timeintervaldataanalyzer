package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Super class for next offset calculators that take the median of some offsets.
 */
public class MedianOffset implements INextOffsetCalculator {
    @Override
    public List<Long> calculate(final Mapping mapping) {
        final List<Double> allCenterDistances = this.getCenterDistances(mapping);
        final int length = allCenterDistances.size();

        return ImmutableList.of(
                this.getMedian(allCenterDistances),
                this.getMedian(allCenterDistances.subList(0, length / 2)),
                this.getMedian(allCenterDistances.subList(length / 2, length)),
                this.getMedian(this.mostExtreme(allCenterDistances)),
                this.getMedian(this.leastExtreme(allCenterDistances)));
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
        int numberOfPairs = offsets.size();
        if (numberOfPairs % 2 == 0) {
            return Math.round((offsets.get(numberOfPairs / 2)
                    + offsets.get(numberOfPairs / 2 - 1)) / 2);
        } else {
            return Math.round(offsets.get(numberOfPairs / 2));
        }
    }
}
