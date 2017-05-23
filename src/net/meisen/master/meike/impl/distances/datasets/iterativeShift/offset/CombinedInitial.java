package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import net.meisen.master.meike.impl.distances.datasets.Dataset;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Simply combines the offsets returned by other offset calculators.
 */
public class CombinedInitial implements IInitialOffsetCalculator {
    private final List<IInitialOffsetCalculator> offsetCalculators;

    private CombinedInitial(final List<IInitialOffsetCalculator> offsetCalculators) {
        this.offsetCalculators = offsetCalculators;
    }

    public static CombinedInitial from(final List<IInitialOffsetCalculator> others) {
        return new CombinedInitial(others);
    }

    @Override
    public List<Long> calculate(final Dataset original, final Dataset other) {
        return this.offsetCalculators.stream()
                .flatMap(c -> c.calculate(original, other).stream())
                .collect(Collectors.toList());
    }
}
