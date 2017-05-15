package net.meisen.master.meike.impl.mapping.exact;

import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementation of the Kuhn-Munkres algorithm.
 */
public class KuhnMunkres implements IMinCostMapper {
    private KuhnMunkres() {}

    /**
     * Creates a new instance of the Kuhn-Munkres implementation.
     *
     * @return an instance of the Kuhn-Munkres implementation
     */
    public static KuhnMunkres create() {
        return new KuhnMunkres();
    }

    @Override
    public Mapping calculateMinimumCostMapping(final CostMatrix costMatrix) {
        final double[][] costs = costMatrix.getCosts();
        final List<Integer> matchingIndices = Arrays
                .stream(new HungarianAlgorithm(costs).execute())
                .boxed()
                .collect(Collectors.toList());

        return Mapping.create(matchingIndices, costMatrix);
    }
}
