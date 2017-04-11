package net.meisen.master.meike.impl.distances.intervals;

import static java.lang.Math.min;

/**
 * Calculates a distance value based on the relative position of the two
 * {@link Interval}s' start times. This distance value is not symmetric!
 */
public class StartPositionDistance implements IIntervalDistance {

    @Override
    public double calculate(Interval original, Interval other) {
        return original.getStart() >= other.getStart()
                ? min(1, original.getStart() - other.getStart())
                : 0;
    }
}
