package net.meisen.master.meike.impl.mapping.costCalculation;

import net.meisen.master.meike.impl.mapping.Mapping;

/**
 * Interface for classes that allow calculating the cost of a mapping.
 */
public interface ICostCalculator {
    /**
     * Calculates the cost of the given mapping.
     *
     * @param mapping
     *          the mapping for which the cost is to be calculated
     * @return the cost of the given mapping
     */
    double calculateCost(Mapping mapping);
}
