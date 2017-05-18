package net.meisen.master.meike.impl.mapping.lowerBounds;

import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Lower bound obtained by ignoring the constraint that we need a matching.
 */
public class DoubleMatcher implements IMinCostMapper {
    private DoubleMatcher() {}

    /**
     * Creates a new instance of the lower bound implementation.
     *
     * @return an instance of the double matching lower bound implementation
     */
    public static DoubleMatcher create() {
        return new DoubleMatcher();
    }

    @Override
    public Mapping calculateMinimumCostMapping(final CostMatrix costMatrix) {
        assert null != costMatrix;

        return this.getRowMinimaMapping(costMatrix);
    }

    private Mapping getRowMinimaMapping(final CostMatrix costMatrix) {
        final double[][] costs = costMatrix.getCosts();
        final List<Integer> mappingIndices = Arrays.stream(costs)
                .map(row -> IntStream.range(0, row.length)
                        .reduce((i,j) -> row[i] < row[j] ? i : j)
                        .orElseThrow(() -> new IllegalStateException("Not supposed to happen.")))
                .collect(Collectors.toList());

        return Mapping.create(mappingIndices, costMatrix);
    }
}
