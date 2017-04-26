package net.meisen.master.meike.impl.knnSearch;

import net.meisen.master.meike.impl.distances.datasets.Dataset;

/**
 * Wrapper around a candidate {@link Dataset} and its distance to the original
 * {@link Dataset}.
 */
public class Neighbor implements Comparable<Neighbor> {
    private final Dataset dataset;
    private final double distanceToOriginal;

    private Neighbor(final Dataset dataset, final double distanceToOriginal) {
        this.dataset = dataset;
        this.distanceToOriginal = distanceToOriginal;
    }

    /**
     * Wraps the given {@link Dataset} and distance value into an instance of
     * the neighbor class.
     *
     * @param dataset
     *          the dataset that was compared to the original dataset; must not
     *          be {@code null}.
     * @param distanceToOriginal
     *          the calculated distance value to the original dataset; must not
     *          be negative.
     * @return a new instance of the neighbor class
     */
    public static Neighbor from(final Dataset dataset,
                                final double distanceToOriginal) {
        assert null != dataset;
        assert 0 <= distanceToOriginal;

        return new Neighbor(dataset, distanceToOriginal);
    }

    public double getDistanceToOriginal() {
        return this.distanceToOriginal;
    }

    public Dataset getDataset() {
        return this.dataset;
    }

    @Override
    public int compareTo(Neighbor other) {
        assert null != other;

        return (int)Math.signum(this.distanceToOriginal - other.distanceToOriginal);
    }
}
