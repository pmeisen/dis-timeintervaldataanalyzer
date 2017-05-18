package net.meisen.master.meike.impl.mapping.upperBounds;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.IDatasetDistance;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;

/**
 * Exact upper bound using some {@link IDatasetDistance} and
 * {@link ICostCalculator}.
 */
public class Exact implements IUpperBound {
    private final IDatasetDistance distance;
    private final ICostCalculator costCalculator;

    private Exact(final IDatasetDistance distance, final ICostCalculator costCalculator) {
        this.distance = distance;
        this.costCalculator = costCalculator;
    }

    public static Exact from(final IDatasetDistance distance, final ICostCalculator costCalculator) {
        assert null != distance;
        assert null != costCalculator;

        return new Exact(distance, costCalculator);
    }

    @Override
    public double calculate(Dataset original, Dataset other) {
        return this.costCalculator.calculateCost(this.distance.calculate(original, other));
    }
}
