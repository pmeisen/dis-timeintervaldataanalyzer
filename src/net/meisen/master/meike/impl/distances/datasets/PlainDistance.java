package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;

/**
 * Allows calculating the plain distance between two {@link Dataset}s without
 * modifying any of their {@link Interval}s in any way.
 */
public class PlainDistance implements IDatasetDistance {

    private final IMinCostMapper mapper;
    private final IIntervalDistance intervalDistance;

    private PlainDistance(final IMinCostMapper mapper,
                          final IIntervalDistance intervalDistance) {
        this.mapper = mapper;
        this.intervalDistance = intervalDistance;
    }

    /**
     * Creates a new instance using the given mapper for distance calculation.
     *
     * @param mapper
     *           the mapper to calculate the distance between two datasets;
     *           must not be {@code null}.
     * @param intervalDistance
     *           a distance measure for pairs of intervals; must not be
     *           {@code null}.
     * @return an instance of this class that uses the given mapper
     */
    public static PlainDistance from(final IMinCostMapper mapper,
                                     final IIntervalDistance intervalDistance) {
        assert null != mapper;
        assert null != intervalDistance;

        return new PlainDistance(mapper, intervalDistance);
    }

    @Override
    public Mapping calculate(final Dataset original, final Dataset other) {
        assert null != original;
        assert null != other;

        original.setOffset(0);
        other.setOffset(0);

        final CostMatrix costMatrix =
                new CostMatrix(this.intervalDistance, original, other);
        return this.mapper.calculateMinimumCostMapping(costMatrix).withOffset(0);
    }
}
