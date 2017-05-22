package net.meisen.master.meike.impl.distances.intervals;

import java.util.Map;

/**
 * Calculates the weighted sum of a set of more basic distance values.
 */
public class WeightedSumDistance implements IIntervalDistance {
    private final Map<IIntervalDistance, Double> weightedDistances;

    /**
     * Creates a new instance of this weighted sum distance calculator based
     * on the given distance measures and their respective weights.
     *
     * @param weightedDistances
     *           A mapping of interval distances and weights to use for them;
     *           must not be {@code null} or contain negative weight values.
     */
    public WeightedSumDistance(final Map<IIntervalDistance, Double> weightedDistances) {
        assert null != weightedDistances;

        if (weightedDistances.values().stream().anyMatch(weight -> weight < 0)) {
            throw new IllegalArgumentException("Distance weights must not be negative.");
        }

        this.weightedDistances = weightedDistances;
    }

    @Override
    public double calculate(final Interval original, final Interval other) {
        return this.weightedDistances.entrySet().stream()
                .mapToDouble(entry -> entry.getKey().calculate(original, other)
                        * entry.getValue())
                .sum();
    }

    @Override
    public String toString() {
        String result = "";
        for (final Double weight : this.weightedDistances.values()) {
            result += Double.toString(weight);
            result += ", ";
        }
        return result;
    }
}
