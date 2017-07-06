package net.meisen.master.meike.impl.distances.datasets;

/**
 * Factory for creating instances of {@link IDatasetDistanceCalculator} for
 * pairs of {@link Dataset}s.
 */
public interface ICalculatorFactory<T extends IDatasetDistanceCalculator>  {
    T getDistanceCalculatorFor(final Dataset original, final Dataset candidate);
}
