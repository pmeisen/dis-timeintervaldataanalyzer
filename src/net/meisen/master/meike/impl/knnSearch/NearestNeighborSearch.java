package net.meisen.master.meike.impl.knnSearch;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.mapping.lowerBounds.ILowerBound;
import net.meisen.master.meike.impl.mapping.upperBounds.IUpperBound;

import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.PriorityQueue;
import java.util.stream.Collectors;

/**
 * Main class for finding the k nearest neighbors to a given original dataset
 * out of a pool of candidate datasets.
 */
public class NearestNeighborSearch {
    private final List<ILowerBound> lowerBounds;
    private final List<IUpperBound> upperBounds;

    private NearestNeighborSearch(final List<ILowerBound> lowerBounds,
                                  final List<IUpperBound> upperBounds) {
        this.lowerBounds = lowerBounds;
        this.upperBounds = upperBounds;
    }

    /**
     * Create a new instance of the nearest neighborhood search using the given
     * bounds and exact distance.
     *
     * @param lowerBounds
     *          lower-bounding measures to reduce the set of candidates,
     *          sorted by efficiency of calculation
     * @param upperBounds
     *          upper-bounding measures, sorted by efficiency of calculation;
     *          the last entry should be the exact measure.
     * @return an instance of the nearest neighborhood search class
     */
    public static NearestNeighborSearch from(
            final List<ILowerBound> lowerBounds,
            final List<IUpperBound> upperBounds) {
        assert null != lowerBounds;
        assert null != upperBounds;
        assert lowerBounds.size() == upperBounds.size();

        return new NearestNeighborSearch(lowerBounds, upperBounds);
    }

    /**
     * Calculates the {@code numberOfNeighbors} neighbors out of the candidate
     * set that none of the other candidate {@link Dataset}s have a smaller
     * distance to the {@code original} {@link Dataset}.
     *
     * @param numberOfNeighbors
     *          the desired number k of nearest neighbors to find
     * @param original
     *          the original dataset to which to compare all others
     * @param candidates
     *          the candidate datasets from which to find nearest neighbors
     * @return a list of the k nearest neighbors of original
     */
    public List<BoundedDataset> find(final int numberOfNeighbors,
                                     final Dataset original,
                                     final Collection<Dataset> candidates) {

        Candidates remainingCandidates = this.initializeCandidates(candidates);

        for (int i = 0; i < this.lowerBounds.size(); i++) {
            this.applyLowerBound(original, remainingCandidates,
                    this.lowerBounds.get(i));
            this.applyUpperBound(original, remainingCandidates,
                    this.upperBounds.get(i), numberOfNeighbors);
        }

        return remainingCandidates.getRemainingCandidates().stream()
                .sorted(Comparator.comparing(BoundedDataset::getUpperBound))
                .limit(numberOfNeighbors)
                .collect(Collectors.toList());
    }

    private void applyLowerBound(final Dataset original,
                                 final Candidates candidates,
                                 final ILowerBound lowerBound) {
        for (final BoundedDataset candidate : candidates.getRemainingCandidates()) {
            candidate.setLowerBound(lowerBound.calculate(original, candidate.getDataset()));
        }
    }

    private void applyUpperBound(final Dataset original,
                                 final Candidates candidates,
                                 final IUpperBound upperBound,
                                 final int numberOfNeighbors) {
        final PriorityQueue<Double> boundValues = new PriorityQueue<>();
        for (final BoundedDataset candidate : candidates.getRemainingCandidates()) {
            final double maxValue = boundValues.stream()
                    .skip(numberOfNeighbors)
                    .findFirst()
                    .orElse(Double.MAX_VALUE);
            if (maxValue <= candidate.getLowerBound()) {
                return;
            }

            final double upperBoundValue = upperBound.calculate(original, candidate.getDataset());
            candidate.setUpperBound(upperBoundValue);
            boundValues.add(upperBoundValue);
        }
    }

    private Candidates initializeCandidates(final Collection<Dataset> candidateSet) {
        final Candidates candidates = Candidates.ofSize(candidateSet.size());

        for (final Dataset candidate : candidateSet) {
            candidates.add(BoundedDataset.from(candidate, 0, Double.MAX_VALUE));
        }

        return candidates;
    }
}
