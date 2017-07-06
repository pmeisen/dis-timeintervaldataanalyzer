package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;

public class PlainFactory implements ICalculatorFactory<PlainCalculator> {
    private final IMinCostMapper mapper;
    private final IIntervalDistance intervalDistance;

    private PlainFactory(final IMinCostMapper mapper,
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
    public static PlainFactory from(final IMinCostMapper mapper,
                                    final IIntervalDistance intervalDistance) {
        assert null != mapper;
        assert null != intervalDistance;

        return new PlainFactory(mapper, intervalDistance);
    }

    @Override
    public PlainCalculator getDistanceCalculatorFor(
            final Dataset original, final Dataset candidate) {
        return PlainCalculator.createFor(original, candidate, this.mapper, this.intervalDistance);
    }
}
