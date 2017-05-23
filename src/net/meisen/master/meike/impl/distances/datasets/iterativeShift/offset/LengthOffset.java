package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Factories;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.LengthDistance;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.List;

/**
 * Calculates an offset based on a good mapping regarding a distance measure
 * that emphasizes the {@link LengthDistance}.
 */
public class LengthOffset implements IInitialOffsetCalculator {

    private static final IIntervalDistance lengthFocusedDistance =
            Factories.weightedDistance(1, 1, 10, 0, 0);

    private final IMinCostMapper mapper;
    private final INextOffsetCalculator offsetCalculator;

    private LengthOffset(final IMinCostMapper mapper,
                         final INextOffsetCalculator nextOffsetCalculator) {
        this.mapper = mapper;
        this.offsetCalculator = nextOffsetCalculator;
    }

    public static LengthOffset from(final IMinCostMapper mapper,
                                    final INextOffsetCalculator nextOffsetCalculator) {
        return new LengthOffset(mapper, nextOffsetCalculator);
    }

    @Override
    public List<Long> calculate(final Dataset original, final Dataset other) {
        final CostMatrix costMatrix =
                new CostMatrix(lengthFocusedDistance, original, other);
        final Mapping mapping =
                this.mapper.calculateMinimumCostMapping(costMatrix).withOffset(0);

        return this.offsetCalculator.calculate(mapping);
    }
}
