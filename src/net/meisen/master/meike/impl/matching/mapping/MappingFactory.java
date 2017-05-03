package net.meisen.master.meike.impl.matching.mapping;

import net.meisen.master.meike.impl.matching.CostMatrix;
import net.meisen.master.meike.impl.matching.costCalculation.ICostCalculator;

import java.util.List;

/**
 * Factory for creating instances of {@link Mapping}.
 */
public class MappingFactory {
    private final ICostCalculator costCalculator;

    private MappingFactory(final ICostCalculator costCalculator) {
        this.costCalculator = costCalculator;
    }

    public static MappingFactory from(final ICostCalculator costCalculator) {
        assert null != costCalculator;

        return new MappingFactory(costCalculator);
    }

    public Mapping create(final CostMatrix costMatrix,
                          final List<Integer> mappingIndices) {
        final double cost =
                this.costCalculator.calculateCost(costMatrix, mappingIndices);
        return Mapping.create(cost, mappingIndices);
    }
}
