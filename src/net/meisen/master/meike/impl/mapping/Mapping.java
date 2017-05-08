package net.meisen.master.meike.impl.mapping;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;

import java.util.List;

/**
 * Wrapper around a mapping from one {@link Dataset} to another and its cost.
 * This can be a matching or any other arbitrary mapping.
 */
public class Mapping {
    public static final int NOT_MAPPED = -1;

    private final double cost;
    private final List<Integer> mappingIndices;

    private Mapping(final double cost, final List<Integer> mappingIndices) {
        this.cost = cost;
        this.mappingIndices = ImmutableList.copyOf(mappingIndices);
    }

    static Mapping create(final double cost,
                                 final List<Integer> mappingIndices) {
        assert 0 <= cost;
        assert null != mappingIndices;

        return new Mapping(cost, mappingIndices);
    }

    /**
     * Given the index i of one of the intervals in the original dataset, this
     * method returns the index j of the interval in the other dataset which
     * interval i is mapped to. The return value {@code NOT_MAPPED} means
     * that the i-th interval of the original dataset is not mapped to any
     * interval of the other dataset.
     *
     * @param originalIndex
     *          the index of the original interval whose mapping partner's index
     *          is to be retrieved     *
     * @return the index of the interval in the other dataset which the i-th
     * interval in the original dataset is mapped to
     */
    public int getMappedIndex(final int originalIndex) {
        return this.mappingIndices.get(originalIndex);
    }

    /**
     * @return the cost of this mapping
     */
    public double getCost() {
        return this.cost;
    }
}
