package net.meisen.master.meike.impl.distances.intervals;

/**
 * Calculates the value of a given {@link IIntervalDistance} for two intervals
 * by allowing to shift one of the intervals optimally.
 */
public class BestShiftDistance implements IIntervalDistance {
    private final IIntervalDistance originalDistance;

    private BestShiftDistance(final IIntervalDistance originalDistance) {
        this.originalDistance = originalDistance;
    }

    public static BestShiftDistance from(final IIntervalDistance originalDistance) {
        return new BestShiftDistance(originalDistance);
    }

    @Override
    public double calculate(final Interval original, final Interval other) {
        final long originalOffset = other.getOffset();

        other.setOffset(original.getStart() - other.getStart() + other.getOffset());
        final double startCost = this.originalDistance.calculate(original, other);

        other.setOffset(original.getEnd() - other.getEnd() + other.getOffset());
        final double endCost = this.originalDistance.calculate(original, other);

        other.setOffset(originalOffset);
        return Math.min(startCost, endCost);
    }
}
