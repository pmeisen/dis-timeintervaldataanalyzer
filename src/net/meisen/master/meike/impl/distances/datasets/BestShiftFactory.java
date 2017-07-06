package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;

public class BestShiftFactory implements ICalculatorFactory<BestShiftCalculator> {
    static final long UNLIMITED_OFFSET = -1;

    private final IMinCostMapper mapper;
    private final IIntervalDistance intervalDistance;
    private final ICostCalculator costCalculator;

    private long maxOffset = UNLIMITED_OFFSET;

    private BestShiftFactory(final IMinCostMapper mapper,
                             final IIntervalDistance intervalDistance,
                             final ICostCalculator costCalculator) {
        this.mapper = mapper;
        this.intervalDistance = intervalDistance;
        this.costCalculator = costCalculator;
    }

    public static BestShiftFactory from(final IMinCostMapper mapper,
                                        final IIntervalDistance intervalDistance,
                                        final ICostCalculator costCalculator) {
        assert null != mapper;
        assert null != intervalDistance;
        assert null != costCalculator;

        return new BestShiftFactory(mapper, intervalDistance, costCalculator);
    }

    @Override
    public BestShiftCalculator getDistanceCalculatorFor(
            final Dataset original, final Dataset candidate) {
        return BestShiftCalculator.createFor(original, candidate, this.mapper,
                this.intervalDistance, this.costCalculator, this.maxOffset);
    }

    /**
     * Limits the offsets to try for finding the best shift to those within a
     * range of {@code maxOffset} from zero. Setting this to the default value
     * {@link BestShiftFactory#UNLIMITED_OFFSET} will remove any restrictions.
     *
     * @param maxOffset
     *          the new offset range to use
     */
    public void setMaxOffset(final long maxOffset) {
        assert 0 <= maxOffset || UNLIMITED_OFFSET == maxOffset;

        this.maxOffset = maxOffset;
    }
}
