package net.meisen.master.meike.impl.distances.intervals;

import static java.lang.Math.min;

/**
 * Calculates a distance value based on the relative position of the two
 * {@link Interval}s' end times. This distance value is not symmetric!
 */
public class EndPositionDistance implements IIntervalDistance {

    @Override
    public double calculate(Interval original, Interval other) {
        return original.getEnd() <= other.getEnd()
                ? min(1, other.getEnd() - original.getEnd())
                : 0;
    }
}
