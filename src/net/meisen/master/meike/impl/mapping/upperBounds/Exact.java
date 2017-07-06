package net.meisen.master.meike.impl.mapping.upperBounds;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.ICalculatorFactory;
import net.meisen.master.meike.impl.distances.datasets.IDatasetDistanceCalculator;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;

/**
 * Exact upper bound using some {@link IDatasetDistanceCalculator} and
 * {@link ICostCalculator}.
 */
public class Exact implements IUpperBound {
    private final ICalculatorFactory distanceCalculatorFactory;
    private final ICostCalculator costCalculator;

    private Exact(final ICalculatorFactory distanceCalculatorFactory,
                  final ICostCalculator costCalculator) {
        this.distanceCalculatorFactory = distanceCalculatorFactory;
        this.costCalculator = costCalculator;
    }

    public static Exact from(final ICalculatorFactory distanceCalculatorFactory,
                             final ICostCalculator costCalculator) {
        assert null != distanceCalculatorFactory;
        assert null != costCalculator;

        return new Exact(distanceCalculatorFactory, costCalculator);
    }

    @Override
    public double calculate(Dataset original, Dataset other) {
        return this.costCalculator.calculateCost(this.distanceCalculatorFactory
                .getDistanceCalculatorFor(original, other).finalMapping());
    }
}
