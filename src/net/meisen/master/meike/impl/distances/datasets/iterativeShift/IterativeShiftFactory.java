package net.meisen.master.meike.impl.distances.datasets.iterativeShift;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.datasets.ICalculatorFactory;
import net.meisen.master.meike.impl.distances.datasets.IDatasetDistanceCalculator;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.neighborhood.INeighborhood;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.IInitialOffsetCalculator;
import net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset.INextOffsetCalculator;
import net.meisen.master.meike.impl.distances.intervals.IIntervalDistance;
import net.meisen.master.meike.impl.logging.MappingLogger;
import net.meisen.master.meike.impl.mapping.IMinCostMapper;
import net.meisen.master.meike.impl.mapping.costCalculation.ICostCalculator;


public class IterativeShiftFactory implements ICalculatorFactory<IterativeShiftCalculator> {
    private final IMinCostMapper minCostMapper;
    private final ICostCalculator costCalculator;
    private final IIntervalDistance distanceMeasure;
    private final INextOffsetCalculator nextOffsetCalculator;
    private final IInitialOffsetCalculator initialOffsetCalculator;
    private final INeighborhood neighborhood;
    private MappingLogger mappingLogger =
            MappingLogger.createFor(0, "01.01.2017", "disabled");

    private IterativeShiftFactory(final IMinCostMapper minCostMapper,
                                  final ICostCalculator costCalculator,
                                  final IIntervalDistance distanceMeasure,
                                  final IInitialOffsetCalculator initialOffsetCalculator,
                                  final INextOffsetCalculator nextOffsetCalculator,
                                  final INeighborhood neighborhood) {
        this.minCostMapper = minCostMapper;
        this.costCalculator = costCalculator;
        this.distanceMeasure = distanceMeasure;
        this.initialOffsetCalculator = initialOffsetCalculator;
        this.nextOffsetCalculator = nextOffsetCalculator;
        this.neighborhood = neighborhood;
        this.mappingLogger.enabled = false;
    }

    public static IterativeShiftFactory from(final IMinCostMapper minCostMapper,
                                             final ICostCalculator costCalculator,
                                             final IIntervalDistance distanceMeasure,
                                             final IInitialOffsetCalculator initialOffsetCalculator,
                                             final INextOffsetCalculator nextOffsetCalculator,
                                             final INeighborhood neighborhood) {
        return new IterativeShiftFactory(minCostMapper, costCalculator, distanceMeasure,
                initialOffsetCalculator, nextOffsetCalculator, neighborhood);
    }

    public void setMappingLogger(final MappingLogger mappingLogger) {
        this.mappingLogger = mappingLogger;
    }

    public void disableMappingLogger() {
        this.mappingLogger.enabled = false;
    }

    @Override
    public IterativeShiftCalculator getDistanceCalculatorFor(
            final Dataset original, final Dataset candidate) {
        return IterativeShiftCalculator.createFor(original, candidate,
                minCostMapper, costCalculator, distanceMeasure, initialOffsetCalculator,
                nextOffsetCalculator, neighborhood, mappingLogger);
    }
}
