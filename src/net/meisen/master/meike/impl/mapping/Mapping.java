package net.meisen.master.meike.impl.mapping;

import com.google.common.collect.ImmutableList;
import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

/**
 * Wrapper around a mapping from the {@link Interval}s of one {@link Dataset} to
 * the {@link Interval}s of another. This can be a matching or any other
 * arbitrary mapping.
 */
public class Mapping {
    private final List<Integer> mappingIndices;
    private final CostMatrix costMatrix;

    private Mapping(final List<Integer> mappingIndices,
                    final CostMatrix costMatrix) {
        this.mappingIndices = ImmutableList.copyOf(mappingIndices);
        this.costMatrix = costMatrix;
    }

    /**
     * Creates a mapping from the given cost matrix and mapping indices.
     *
     * @param mappingIndices
     *          list of indices indicating which interval from the first
     *          dataset is mapped to which interval of the second dataset
     * @param costMatrix
     *          the cost matrix which the mapping is based on
     * @return a new mapping instance
     */
    public static Mapping create(final List<Integer> mappingIndices,
                                 final CostMatrix costMatrix) {
        assert null != mappingIndices;
        assert null != costMatrix;
        assert mappingIndices.size() == costMatrix.getCosts().length;

        return new Mapping(mappingIndices, costMatrix);
    }

    /**
     * @return a list of the costs for all mapped pairs; a missing value
     * indicates that an interval was mapped to a dummy interval and thus
     * there is no real cost.
     */
    public List<Optional<Double>> getMappingCosts() {
        final int firstLength = this.costMatrix.getFirstDatasetLength();
        final int secondLength = this.costMatrix.getSecondDatasetLength();
        final List<Optional<Double>> result = new LinkedList<>();
        for (int i = 0; i < this.costMatrix.getCosts().length; i++) {
            if (i < firstLength) {
                final int j = this.mappingIndices.get(i);
                final Optional<Double> cost = j < secondLength
                        ? Optional.of(this.costMatrix.getCosts()[i][j])
                        : Optional.empty();
                result.add(cost);
            } else {
                result.add(Optional.empty());
            }
        }
        return result;
    }

    public List<Pair<Interval, Interval>> getPairs() {
        final List<Pair<Interval, Interval>> pairs = new LinkedList<>();
        for (int i = 0; i < this.costMatrix.getFirstDatasetLength(); i++) {
            final int mappedIndex = this.mappingIndices.get(i);
            if (mappedIndex < this.costMatrix.getSecondDatasetLength()) {
                pairs.add(new Pair<>(this.costMatrix.getFirstDataset().get(i),
                        this.costMatrix.getSecondDataset().get(mappedIndex)));
            }
        }
        return pairs;
    }

    /**
     * When the underlying datasets are of different lengths, some intervals
     * of the larger dataset are not part of the mapping. This method returns
     * all those intervals.
     */
    public List<Interval> getUnmappedIntervalsOfLargerDataset() {
        return this.firstDatasetIsShorter()
                ? this.getUnmappedIntervalsOfSecondDataset()
                : this.getUnmappedIntervalsOfFirstDataset();
    }

    /**
     * @return all intervals of the smaller dataset.
     */
    public List<Interval> getIntervalsOfSmallerDataset()  {
        return this.firstDatasetIsShorter()
                ? this.costMatrix.getFirstDataset()
                : this.costMatrix.getSecondDataset();
    }

    private boolean firstDatasetIsShorter() {
        return this.costMatrix.getFirstDatasetLength() < this.costMatrix.getSecondDatasetLength();
    }

    private List<Interval> getUnmappedIntervalsOfFirstDataset() {
        final int secondLength = this.costMatrix.getSecondDatasetLength();
        final List<Interval> unmappedIntervals = new LinkedList<>();
        for (int i = 0; i < this.costMatrix.getFirstDatasetLength(); i++) {
            if (this.mappingIndices.get(i) >= secondLength) {
                unmappedIntervals.add(this.costMatrix.getFirstDataset().get(i));
            }
        }
        return unmappedIntervals;
    }

    private List<Interval> getUnmappedIntervalsOfSecondDataset() {
        final List<Interval> secondIntervals = costMatrix.getSecondDataset();
        final List<Interval> secondDataset = new LinkedList<>(secondIntervals);
        for (int i = 0; i < this.costMatrix.getFirstDatasetLength(); i++) {
            secondDataset.remove(secondIntervals.get(this.mappingIndices.get(i)));
        }
        return secondDataset;
    }
}
