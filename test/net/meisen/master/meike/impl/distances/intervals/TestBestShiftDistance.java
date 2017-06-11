package net.meisen.master.meike.impl.distances.intervals;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class TestBestShiftDistance {
    private final Interval original = new Interval(4L, 12L);
    private final Interval identical = new Interval(4L, 12L);
    private final Interval sameStartShorter = new Interval(4L, 8L);
    private final Interval sameStartLonger = new Interval(4L, 14L);
    private final Interval sameEndShorter = new Interval(10L, 12L);
    private final Interval sameEndLonger = new Interval(0L, 12L);
    private final Interval containingWholeInterval = new Interval(2L, 16L);
    private final Interval containingStart = new Interval(2L, 6L);
    private final Interval containingEnd = new Interval(9L, 17L);
    private final Interval contained = new Interval(6L, 8L);
    private final Interval beforeWithGap = new Interval(0L, 2L);
    private final Interval directlyBefore = new Interval(0L, 4L);
    private final Interval afterWithGap = new Interval(15L, 20L);
    private final Interval directlyAfter = new Interval(12L, 15L);
    private final Interval zeroLength = new Interval(3L, 3L);
    private final Interval zeroLengthOther = new Interval(4L, 4L);

    @Test
    public void testStartDistanceAlwaysZero() {
        final IIntervalDistance distance = BestShiftDistance.from(new StartDistance());

        assertEquals(0, distance.calculate(original, identical), 0);
        assertEquals(0, distance.calculate(original, sameStartShorter), 0);
        assertEquals(0, distance.calculate(original, sameStartLonger), 0);
        assertEquals(0, distance.calculate(original, sameEndShorter), 0);
        assertEquals(0, distance.calculate(original, sameEndLonger), 0);
        assertEquals(0, distance.calculate(original, containingWholeInterval), 0);
        assertEquals(0, distance.calculate(original, containingStart), 0);
        assertEquals(0, distance.calculate(original, containingEnd), 0);
        assertEquals(0, distance.calculate(original, contained), 0);
        assertEquals(0, distance.calculate(original, beforeWithGap), 0);
        assertEquals(0, distance.calculate(original, directlyBefore), 0);
        assertEquals(0, distance.calculate(original, afterWithGap), 0);
        assertEquals(0, distance.calculate(original, directlyAfter), 0);
        assertEquals(0, distance.calculate(zeroLength, zeroLengthOther), 0);
    }

    @Test
    public void testEndDistanceAlwaysZero() {
        final IIntervalDistance distance = BestShiftDistance.from(new EndDistance());

        assertEquals(0, distance.calculate(original, identical), 0);
        assertEquals(0, distance.calculate(original, sameStartShorter), 0);
        assertEquals(0, distance.calculate(original, sameStartLonger), 0);
        assertEquals(0, distance.calculate(original, sameEndShorter), 0);
        assertEquals(0, distance.calculate(original, sameEndLonger), 0);
        assertEquals(0, distance.calculate(original, containingWholeInterval), 0);
        assertEquals(0, distance.calculate(original, containingStart), 0);
        assertEquals(0, distance.calculate(original, containingEnd), 0);
        assertEquals(0, distance.calculate(original, contained), 0);
        assertEquals(0, distance.calculate(original, beforeWithGap), 0);
        assertEquals(0, distance.calculate(original, directlyBefore), 0);
        assertEquals(0, distance.calculate(original, afterWithGap), 0);
        assertEquals(0, distance.calculate(original, directlyAfter), 0);
        assertEquals(0, distance.calculate(zeroLength, zeroLengthOther), 0);
    }

    @Test
    public void testLengthDistanceNotAffected() {
        final LengthDistance lengthDistance = new LengthDistance();
        final BestShiftDistance bestShiftDistance = BestShiftDistance.from(lengthDistance);

        assertEquals(lengthDistance.calculate(original, identical), bestShiftDistance.calculate(original, identical), 0);
        assertEquals(lengthDistance.calculate(original, sameStartShorter), bestShiftDistance.calculate(original, sameStartShorter), 0);
        assertEquals(lengthDistance.calculate(original, sameStartLonger), bestShiftDistance.calculate(original, sameStartLonger), 0);
        assertEquals(lengthDistance.calculate(original, sameEndShorter), bestShiftDistance.calculate(original, sameEndShorter), 0);
        assertEquals(lengthDistance.calculate(original, sameEndLonger), bestShiftDistance.calculate(original, sameEndLonger), 0);
        assertEquals(lengthDistance.calculate(original, containingWholeInterval), bestShiftDistance.calculate(original, containingWholeInterval), 0);
        assertEquals(lengthDistance.calculate(original, containingStart), bestShiftDistance.calculate(original, containingStart), 0);
        assertEquals(lengthDistance.calculate(original, containingEnd), bestShiftDistance.calculate(original, containingEnd), 0);
        assertEquals(lengthDistance.calculate(original, contained), bestShiftDistance.calculate(original, contained), 0);
        assertEquals(lengthDistance.calculate(original, beforeWithGap), bestShiftDistance.calculate(original, beforeWithGap), 0);
        assertEquals(lengthDistance.calculate(original, directlyBefore), bestShiftDistance.calculate(original, directlyBefore), 0);
        assertEquals(lengthDistance.calculate(original, afterWithGap), bestShiftDistance.calculate(original, afterWithGap), 0);
        assertEquals(lengthDistance.calculate(original, directlyAfter), bestShiftDistance.calculate(original, directlyAfter), 0);
        assertEquals(lengthDistance.calculate(zeroLength, zeroLengthOther), bestShiftDistance.calculate(zeroLength, zeroLengthOther), 0);
    }

    @Test
    public void testOffsetDoesNotChange() {
        final BestShiftDistance bestShiftDistance = BestShiftDistance.from(new StartDistance());

        final Interval first = new Interval(4L, 12L);
        first.setOffset(5L);

        final Interval second = new Interval(19L, 21L);
        second.setOffset(-1L);

        final double cost = bestShiftDistance.calculate(first, second);
        assertEquals(0, cost, 0);
        assertEquals(5L, first.getOffset());
        assertEquals(-1L, second.getOffset());
    }
}
