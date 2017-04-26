package net.meisen.master.meike.impl.knnSearch;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

/**
 * Data type for storing the candidates in a nearest neighborhood search.
 * New candidates can be added, existing candidates will be removed whenever
 * there are sufficient better neighbors.
 */
public class NearestNeighbors {
    private final int numberOfNearestNeighbors;
    private final TreeSet<Neighbor> nearestNeighbors;

    private NearestNeighbors(final int numberOfNearestNeighbors) {
        this.numberOfNearestNeighbors = numberOfNearestNeighbors;
        this.nearestNeighbors = new TreeSet<>();
    }

    /**
     * Creates a new instance of this class with the given size.
     *
     * @param numberOfNearestNeighbors
     *          the number of nearest neighbors to keep; must be positive.
     * @return an instance of this class
     */
    public static NearestNeighbors ofSize(final int numberOfNearestNeighbors) {
        assert 0 < numberOfNearestNeighbors;

        return new NearestNeighbors(numberOfNearestNeighbors);
    }

    /**
     * Adds the given {@link Neighbor} to this collection, possibly kicking out
     * another neighbor that is not quite as close to the original dataset.
     *
     * @param neighbor
     *          the new neighbor to be inserted
     */
    public void add(final Neighbor neighbor) {
        assert null != neighbor;

        this.nearestNeighbors.add(neighbor);
    }

    /**
     * Returns the distance of the worst neighbor that is still contained in
     * the knn neighborhood of the original dataset. Any datasets with a larger
     * distance to the original dataset will not be k-nearest neighbors.
     *
     * @return the distance of the worst neighbor contained in the knn
     * neighborhood or the maximal possible value if there are not yet enough
     * neighbors
     */
    public double getLargestDistance() {
        if (this.numberOfNearestNeighbors > this.nearestNeighbors.size()) {
            return Double.MAX_VALUE;
        } else {
            return this.getNeighbors().get(this.numberOfNearestNeighbors - 1)
                    .getDistanceToOriginal();
        }
    }

    /**
     * @return the currently best k nearest neighbors
     */
    public List<Neighbor> getNeighbors() {
        final List<Neighbor> neighbors = new ArrayList<>();
        final Iterator<Neighbor> iterator = this.nearestNeighbors.iterator();
        int index = 0;
        while (iterator.hasNext() && index < this.numberOfNearestNeighbors) {
            neighbors.add(iterator.next());
            index++;
        }
        return neighbors;
    }
}
