package net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

/**
 * Calculates the neighbors of a permutation by applying local perturbations.
 */
public class LocalPerturbation implements INeighborhood {
    private static final int NOT_SET = -1;

    @Override
    public List<Mapping> getNeighbors(final Mapping mapping,
                                      final Dataset original,
                                      final Dataset other) {
        return this.getStartIntervalIndices(mapping).stream()
                .map(i -> this.getNeighborStartingFrom(i, mapping))
                .collect(Collectors.toList());
    }

    Mapping getNeighborStartingFrom(final int startIndex,
                                    final Mapping mapping) {
        final double[][] costMatrix = mapping.getCostMatrix().getCosts();
        final int firstDatasetLength = mapping.getCostMatrix().getFirstDatasetLength();
        final List<Integer> originalMappingIndices = mapping.getMappingIndices();
        final Integer[] resultMappingIndices = this.initializeNewMapping(costMatrix.length);
        final Set<Integer> remainingMappingPartners = new HashSet<>(
                IntStream.range(0, costMatrix.length).boxed().collect(Collectors.toList()));

        int currentIndex = startIndex;
        while (resultMappingIndices[currentIndex] == NOT_SET && currentIndex < firstDatasetLength) {
            final int ind = currentIndex;
            final int partnerIndex = remainingMappingPartners.stream()
                    .sorted(Comparator.comparing(i -> costMatrix[ind][i]))
                    .findFirst()
                    .orElseThrow(() -> new IllegalStateException("Cannot happen"));
            resultMappingIndices[currentIndex] = partnerIndex;
            remainingMappingPartners.remove(partnerIndex);
            currentIndex = originalMappingIndices.indexOf(partnerIndex);
        }
        if (currentIndex >= firstDatasetLength) {
            resultMappingIndices[currentIndex] = originalMappingIndices.get(startIndex);
        }
        for (int i = 0; i < costMatrix.length; i++) {
            if (resultMappingIndices[i] == NOT_SET) {
                resultMappingIndices[i] = originalMappingIndices.get(i);
            }
        }

        return Mapping.create(Arrays.asList(resultMappingIndices), mapping.getCostMatrix());
    }

    private Integer[] initializeNewMapping(final int length) {
        final Integer[] mappingIndices = new Integer[length];
        for (int i = 0; i < mappingIndices.length; i++) {
            mappingIndices[i] = NOT_SET;
        }
        return mappingIndices;
    }

    private List<Integer> getStartIntervalIndices(final Mapping mapping) {
        return IntStream.range(0, mapping.getCostMatrix().getFirstDatasetLength())
                .boxed()
                .sorted(Comparator.comparing(i -> -mapping.getMappingCosts().get(i).orElse(Double.MAX_VALUE)))
                .limit(10)
                .collect(Collectors.toList());
    }
}
