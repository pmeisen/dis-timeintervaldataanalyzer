package net.meisen.master.meike.impl.knnSearch;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Wrapper around a collection of candidates during a k-nearest neighbors
 * search.
 */
class Candidates {
    private final int numberOfNearestNeighbors;
    private final List<BoundedDataset> candidates;

    private Candidates(final int numberOfNearestNeighbors) {
        this.numberOfNearestNeighbors = numberOfNearestNeighbors;
        this.candidates = new ArrayList<>();
    }

    /**
     * Creates a new instance of this class with the given size.
     *
     * @param numberOfNearestNeighbors
     *          the number of nearest neighbors to keep; must be positive.
     * @return an instance of this class
     */
    public static Candidates ofSize(final int numberOfNearestNeighbors) {
        assert 0 < numberOfNearestNeighbors;

        return new Candidates(numberOfNearestNeighbors);
    }

    /**
     * Adds the given {@link BoundedDataset} to this collection, possibly
     * kicking out another candidate that is not quite as close to the original
     * dataset.
     *
     * @param candidate
     *          the new candidate to be inserted
     */
    public void add(final BoundedDataset candidate) {
        assert null != candidate;

        this.candidates.add(candidate);
    }

    /**
     * Returns all {@link BoundedDataset}s that might still be contained in the
     * set of the k nearest neighbors of the original dataset: Only candidates
     * whose lower bounding values are smaller than the k-th smallest upper
     * bounding value are kept.
     *
     * @return a list of the remaining candidates, sorted by their lower
     * bounding values.
     */
    public List<BoundedDataset> getRemainingCandidates() {
        final double maxLowerBound = this.candidates.stream()
                .mapToDouble(BoundedDataset::getUpperBound)
                .sorted()
                .skip(this.numberOfNearestNeighbors - 1)
                .findFirst()
                .orElse(Double.MAX_VALUE);
        return this.candidates.stream()
                .filter(dataset -> dataset.getLowerBound() < maxLowerBound)
                .collect(Collectors.toList());
    }
}
