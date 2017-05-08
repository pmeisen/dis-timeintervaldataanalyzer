package net.meisen.master.meike.impl.knnSearch;

import net.meisen.master.meike.impl.bounds.ILowerBound;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.IDatasetDistance;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.Collection;
import java.util.List;
import java.util.PriorityQueue;

/**
 * Main class for finding the k nearest neighbors to a given original dataset
 * out of a pool of candidate datasets.
 */
public class NearestNeighborSearch {
    private final ILowerBound lowerBound;
    private final IDatasetDistance exactDistance;

    private NearestNeighborSearch(final ILowerBound lowerBound,
                                  final IDatasetDistance exactDistance) {
        this.lowerBound = lowerBound;
        this.exactDistance = exactDistance;
    }

    /**
     * Create a new instance of the nearest neighborhood search using the given
     * lower bound and exact distance.
     *
     * @param lowerBound
     *          lower-bounding measure to reduce the set of candidates
     * @param exactDistance
     *          exact distance masure for calculating the distance between a
     *          candidate and the original dataset
     * @return an instance of the nearest neighborhood search class
     */
    public NearestNeighborSearch from(final ILowerBound lowerBound,
                                      final IDatasetDistance exactDistance) {
        assert null != lowerBound;
        assert null != exactDistance;

        return new NearestNeighborSearch(lowerBound, exactDistance);
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
    public List<Neighbor> find(final int numberOfNeighbors,
                               final Dataset original,
                               final Collection<Dataset> candidates) {
        final PriorityQueue<LowerBoundedDataset> queue =
                this.createQueue(original, candidates);

        final NearestNeighbors nearestNeighbors =
                this.createNearestNeighbors(original, numberOfNeighbors, queue);

        return nearestNeighbors.getNeighbors();
    }

    private PriorityQueue<LowerBoundedDataset> createQueue(
            final Dataset original, final Collection<Dataset> candidates) {
        final PriorityQueue<LowerBoundedDataset> queue = new PriorityQueue<>();
        for (final Dataset candidate : candidates) {
            queue.add(LowerBoundedDataset.from(candidate,
                    lowerBound.calculate(original, candidate)));
        }
        return queue;
    }

    private NearestNeighbors createNearestNeighbors(
            final Dataset original, int numberOfNeighbors,
            final PriorityQueue<LowerBoundedDataset> queue) {
        final NearestNeighbors nearestNeighbors = NearestNeighbors.ofSize(numberOfNeighbors);

        while (queue.peek().getLowerBound() < nearestNeighbors.getLargestDistance()) {
            final Dataset candidate = queue.poll().getDataset();
            final Mapping mapping = this.exactDistance.calculate(original, candidate);
            nearestNeighbors.add(Neighbor.from(candidate, mapping.getCost()));
        }

        return nearestNeighbors;
    }
}
