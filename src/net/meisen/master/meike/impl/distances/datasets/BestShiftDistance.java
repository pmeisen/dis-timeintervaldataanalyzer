package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.logging.ILogger;
import net.meisen.master.meike.impl.logging.SimpleConsoleLogger;
import net.meisen.master.meike.impl.matching.IDatasetMinCostMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Allows calculating the distance between two {@link Dataset}s considering
 * the possibility to shift all {@link Interval}s of one of them by a constant
 * offset. The algorithm considers all 4n^2 possible offset values in order of
 * increasing absolute value; if the application allows limiting the range of
 * possible offsets, a maximum valid offset can be configured beforehand.
 */
public class BestShiftDistance implements IDatasetDistance {

    public static final long UNLIMITED_OFFSET = -1;

    private final IDatasetMinCostMapper mapper;
    private final ILogger logger;

    private long maxOffset = UNLIMITED_OFFSET;

    private BestShiftDistance(final IDatasetMinCostMapper mapper) {
        this.mapper = mapper;
        this.logger = new SimpleConsoleLogger();
    }

    /**
     * Creates a new instance using the given mapper for distance calculation.
     *
     * @param mapper
     *          the mapper to calculate the distance between two datasets;
     *          must not be {@code null}.
     * @return an instance of this class that uses the given mapper
     */
    public static BestShiftDistance from(final IDatasetMinCostMapper mapper) {
        assert null != mapper;

        return new BestShiftDistance(mapper);
    }

    /**
     * Limits the offsets to try for finding the best shift to those within a
     * range of {@code maxOffset} from zero. Setting this to the default value
     * {@link BestShiftDistance#UNLIMITED_OFFSET} will remove any restrictions.
     *
     * @param maxOffset
     *          the new offset range to use
     */
    public void setMaxOffset(final long maxOffset) {
        assert 0 <= maxOffset || UNLIMITED_OFFSET == maxOffset;

        this.maxOffset = maxOffset;
    }

    @Override
    public double calculate(final Dataset original, final Dataset other) {
        assert null != original;
        assert null != other;

        double bestDistance = Double.MAX_VALUE;
        long bestOffset = 0;

        for (final long offset : this.getPossibleOffsets(original, other)) {
            final double distance =
                    this.calculateWithOffset(offset, original, other);
            if (distance < bestDistance) {
                bestDistance = distance;
                bestOffset = offset;
            }
        }

        this.logger.log("Best offset: \t" + bestOffset);

        return bestDistance;
    }

    private double calculateWithOffset(final long offset,
                                       final Dataset original,
                                       final Dataset other) {
        original.setOffset(offset);
        return this.mapper.calculateMinimumCostMapping(original, other).getCost();
    }

    /**
     * Each pair of intervals (p, q) from original x other creates four
     * candidates for the best offset. This method calculates all these
     * candidates.
     *
     * @param original
     *           the original dataset
     * @param other
     *           the dataset that is being compared to the original one
     * @return all values that might be the best offset, ordered ascendingly.
     */
    private List<Long> getPossibleOffsets(final Dataset original,
                                          final Dataset other) {
        final Set<Long> offsets = new HashSet<>();
        for (final Interval first : original.getIntervals()) {
            for (final Interval second: other.getIntervals()) {
                offsets.add(first.getStart() - second.getEnd());
                offsets.add(first.getEnd() - second.getEnd());
                offsets.add(first.getStart() - second.getStart());
                offsets.add(first.getEnd() - second.getStart());
            }
        }

        if (UNLIMITED_OFFSET == this.maxOffset) {
            return offsets.stream()
                    .sorted(Comparator.comparingLong(v -> Math.abs(0 - v)))
                    .collect(Collectors.toList());
        } else {
            return offsets.stream()
                    .filter(v -> -this.maxOffset <= v && v <= this.maxOffset)
                    .sorted(Comparator.comparingLong(v -> Math.abs(0 - v)))
                    .collect(Collectors.toList());
        }
    }
}
