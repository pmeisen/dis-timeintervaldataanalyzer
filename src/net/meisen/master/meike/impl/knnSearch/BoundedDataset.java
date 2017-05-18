package net.meisen.master.meike.impl.knnSearch;

import net.meisen.master.meike.impl.distances.datasets.Dataset;

/**
 * Wrapper around a {@link Dataset} and its best known bound values.
 */
public class BoundedDataset implements Comparable<BoundedDataset> {
    private final Dataset dataset;
    private double lowerBound;
    private double upperBound;

    private BoundedDataset(final Dataset dataset, final double lowerBound,
                           final double upperBound) {
        this.dataset = dataset;
        this.lowerBound = lowerBound;
        this.upperBound = upperBound;
    }

    /**
     * Creates a new instance of this class from a given {@link Dataset} and
     * its bounding values.
     *
     * @param dataset
     *          the dataset to wrap
     * @param lowerBound
     *          the lower bounding value of the dataset
     * @param upperBound
     *          the upper bounding value of the dataset
     * @return a new bounded dataset
     */
    public static BoundedDataset from(final Dataset dataset,
                                      final double lowerBound,
                                      final double upperBound) {
        assert null != dataset;
        assert 0 <= lowerBound;
        assert lowerBound <= upperBound;

        return new BoundedDataset(dataset, lowerBound, upperBound);
    }

    @Override
    public int compareTo(BoundedDataset other) {
        assert null != other;

        return (int)Math.signum(this.lowerBound - other.lowerBound);
    }

    public Dataset getDataset() {
        return this.dataset;
    }

    public double getLowerBound() {
        return this.lowerBound;
    }

    public double getUpperBound() {
        return this.upperBound;
    }

    public void setLowerBound(final double value) {
        assert value >= this.lowerBound;

        this.lowerBound = value;
    }

    public void setUpperBound(final double value) {
        assert value <= this.upperBound;

        this.upperBound = value;
    }
}
