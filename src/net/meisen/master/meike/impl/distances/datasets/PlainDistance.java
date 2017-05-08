package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.IDatasetMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;

/**
 * Allows calculating the plain distance between two {@link Dataset}s without
 * modifying any of their {@link Interval}s in any way.
 */
public class PlainDistance implements IDatasetDistance {

    private final IDatasetMinCostMapper mapper;

    private PlainDistance(final IDatasetMinCostMapper mapper) {
        this.mapper = mapper;
    }

    /**
     * Creates a new instance using the given mapper for distance calculation.
     *
     * @param mapper
     *           the mapper to calculate the distance between two datasets;
     *           must not be {@code null}.
     * @return an instance of this class that uses the given mapper
     */
    public static PlainDistance from(final IDatasetMinCostMapper mapper) {
        assert null != mapper;

        return new PlainDistance(mapper);
    }

    @Override
    public Mapping calculate(final Dataset original, final Dataset other) {
        assert null != original;
        assert null != other;

        return this.mapper.calculateMinimumCostMapping(original, other);
    }
}
