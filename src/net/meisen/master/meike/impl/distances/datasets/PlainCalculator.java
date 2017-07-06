package net.meisen.master.meike.impl.distances.datasets;

import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;

import java.util.Optional;

/**
 * Allows calculating the plain distance between two {@link Dataset}s without
 * modifying any of their {@link Interval}s in any way.
 */
public class PlainCalculator implements IDatasetDistanceCalculator {
    private final Dataset originalDataset;
    private final Dataset candidateDataset;
    private final IMinCostMapper minCostMapper;
    private final IIntervalDistance intervalDistance;

    private Mapping mapping;

    private PlainCalculator(final Dataset original,
                            final Dataset candidate,
                            final IMinCostMapper minCostMapper,
                            final IIntervalDistance intervalDistance) {
        this.originalDataset = original;
        this.candidateDataset = candidate;
        this.minCostMapper = minCostMapper;
        this.intervalDistance = intervalDistance;
    }

    static PlainCalculator createFor(final Dataset original,
                                     final Dataset candidate,
                                     final IMinCostMapper minCostMapper,
                                     final IIntervalDistance intervalDistance) {
        assert null != original;
        assert null != candidate;
        assert null != minCostMapper;
        assert null != intervalDistance;

        return new PlainCalculator(original, candidate, minCostMapper, intervalDistance);
    }

    private Mapping calculate() {
        this.originalDataset.setOffset(0);
        this.candidateDataset.setOffset(0);

        final CostMatrix costMatrix = new CostMatrix(
                this.intervalDistance, this.originalDataset, this.candidateDataset);
        return this.minCostMapper.calculateMinimumCostMapping(costMatrix).withOffset(0);
    }

    @Override
    public Optional<Mapping> nextMapping() {
        if (this.mapping == null) {
            this.mapping = this.calculate();
            return Optional.of(this.mapping);
        } else {
            return Optional.empty();
        }
    }

    @Override
    public Mapping finalMapping() {
        if (this.mapping == null) {
            this.mapping = this.calculate();
        }
        return this.mapping;
    }
}
