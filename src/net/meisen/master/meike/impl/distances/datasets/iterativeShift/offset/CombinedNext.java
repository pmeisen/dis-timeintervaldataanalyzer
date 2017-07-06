package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Simply combines the offsets returned by other offset calculators.
 */
public class CombinedNext implements INextOffsetCalculator {
    private final List<INextOffsetCalculator> offsetCalculators;

    private CombinedNext(final List<INextOffsetCalculator> offsetCalculators) {
        this.offsetCalculators = offsetCalculators;
    }

    public static CombinedNext from(final List<INextOffsetCalculator> others) {
        return new CombinedNext(others);
    }

    @Override
    public List<Long> calculate(Mapping mapping) {
        return this.offsetCalculators.stream()
                .flatMap(c -> c.calculate(mapping).stream())
                .distinct()
                .collect(Collectors.toList());
    }
}
