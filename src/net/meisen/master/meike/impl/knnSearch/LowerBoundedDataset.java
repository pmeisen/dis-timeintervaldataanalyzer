package net.meisen.master.meike.impl.knnSearch;

import net.meisen.master.meike.impl.distances.datasets.Dataset;

/**
 * Wrapper around a {@link Dataset} and its best lower bound value.
 */
public class LowerBoundedDataset implements Comparable<LowerBoundedDataset> {
    private final Dataset dataset;
    private double lowerBound;

    private LowerBoundedDataset(final Dataset dataset, final double lowerBound) {
        this.dataset = dataset;
        this.lowerBound = lowerBound;
    }

    /**
     * Creates a new instance of this class from a given {@link Dataset} and
     * its lower bound value.
     *
     * @param dataset
     *          the dataset to wrap
     * @param lowerBound
     *          the lower bounding value of the dataset
     * @return a new lower-bounded dataset
     */
    public static LowerBoundedDataset from(final Dataset dataset,
                                           final double lowerBound) {
        assert null != dataset;
        assert 0 <= lowerBound;

        return new LowerBoundedDataset(dataset, lowerBound);
    }

    @Override
    public int compareTo(LowerBoundedDataset other) {
        assert null != other;

        return (int)Math.signum(this.lowerBound - other.lowerBound);
    }

    public Dataset getDataset() {
        return this.dataset;
    }

    public double getLowerBound() {
        return this.lowerBound;
    }
}
