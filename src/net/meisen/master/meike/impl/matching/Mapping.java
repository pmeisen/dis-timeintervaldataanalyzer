package net.meisen.master.meike.impl.matching;

import net.meisen.master.meike.impl.distances.datasets.Dataset;

/**
 * Wrapper around a mapping from one {@link Dataset} to another and its cost.
 * This can be a matching or any other arbitrary mapping.
 */
public class Mapping {
    public static final int INDEX_FOR_INTERVAL_WITHOUT_MATCH = -1;
    private final double cost;
    private final int[] mappingIndices;

    private Mapping(final double cost, final int[] mappingIndices) {
        this.cost = cost;
        this.mappingIndices = mappingIndices;
    }

    public static Mapping create(final double cost,
                                 final int[] mappingIndices) {
        assert 0 <= cost;
        assert null != mappingIndices;

        return new Mapping(cost, mappingIndices);
    }

    public double getCost() {
        return this.cost;
    }

    /**
     * Returns the indexes of the other dataset in the order they are mapped to
     * the intervals of the original dataset, i.e. {@code mapping[i] = j} means
     * that the i-th interval of the original dataset gets mapped to the j-th
     * interval of the other dataset. {@code mapping[i] = -1} means that the
     * i-th interval of the original dataset is not mapped to any interval of
     * the other dataset.
     *
     * @return a mapping from the indices of the first dataset's intervals to
     * the second dataset's intervals
     */
    public int[] getMappingIndices() {
        return this.mappingIndices;
    }
}
