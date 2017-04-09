package net.meisen.master.meike.impl.distances.intervals;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * Tests for the basic distance values between two {@link Interval}s.
 */
public class TestBasicDistances {

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

    @Test
    public void testEndDistance() {
        final EndDistance distance = new EndDistance();

        assertEquals(0, distance.calculate(original, identical), 0);
        assertEquals(4.0/8, distance.calculate(original, sameStartShorter), 0);
        assertEquals(2.0/10, distance.calculate(original, sameStartLonger), 0);
        assertEquals(0, distance.calculate(original, sameEndShorter), 0);
        assertEquals(0, distance.calculate(original, sameEndLonger), 0);
        assertEquals(4.0/14, distance.calculate(original, containingWholeInterval), 0);
        assertEquals(6.0/10, distance.calculate(original, containingStart), 0);
        assertEquals(5.0/13, distance.calculate(original, containingEnd), 0);
        assertEquals(4.0/8, distance.calculate(original, contained), 0);
        assertEquals(10.0/12, distance.calculate(original, beforeWithGap), 0);
        assertEquals(8.0/12, distance.calculate(original, directlyBefore), 0);
        assertEquals(8.0/16, distance.calculate(original, afterWithGap), 0);
        assertEquals(3.0/11, distance.calculate(original, directlyAfter), 0);
    }

    @Test
    public void testGapDistance() {
        final GapDistance distance = new GapDistance();

        assertEquals(0, distance.calculate(original, identical), 0);
        assertEquals(0, distance.calculate(original, sameStartShorter), 0);
        assertEquals(0, distance.calculate(original, sameStartLonger), 0);
        assertEquals(0, distance.calculate(original, sameEndShorter), 0);
        assertEquals(0, distance.calculate(original, sameEndLonger), 0);
        assertEquals(0, distance.calculate(original, containingWholeInterval), 0);
        assertEquals(0, distance.calculate(original, containingStart), 0);
        assertEquals(0, distance.calculate(original, containingEnd), 0);
        assertEquals(0, distance.calculate(original, contained), 0);
        assertEquals(2.0/12, distance.calculate(original, beforeWithGap), 0);
        assertEquals(0, distance.calculate(original, directlyBefore), 0);
        assertEquals(3.0/16, distance.calculate(original, afterWithGap), 0);
        assertEquals(0, distance.calculate(original, directlyAfter), 0);

    }

    @Test
    public void testIntersectionDistance() {
        final IntersectionDistance distance = new IntersectionDistance();

        assertEquals(0, distance.calculate(original, identical), 0);
        assertEquals(0, distance.calculate(original, sameStartShorter), 0);
        assertEquals(0, distance.calculate(original, sameStartLonger), 0);
        assertEquals(0, distance.calculate(original, sameEndShorter), 0);
        assertEquals(0, distance.calculate(original, sameEndLonger), 0);
        assertEquals(0, distance.calculate(original, containingWholeInterval), 0);
        assertEquals(1-2.0/4, distance.calculate(original, containingStart), 0);
        assertEquals(1-3.0/8, distance.calculate(original, containingEnd), 0);
        assertEquals(0, distance.calculate(original, contained), 0);
        assertEquals(1, distance.calculate(original, beforeWithGap), 0);
        assertEquals(1, distance.calculate(original, directlyBefore), 0);
        assertEquals(1, distance.calculate(original, afterWithGap), 0);
        assertEquals(1, distance.calculate(original, directlyAfter), 0);
    }

    @Test
    public void testLengthDistance() {
        final LengthDistance distance = new LengthDistance();

        assertEquals(0, distance.calculate(original, identical), 0);
        assertEquals(1-4.0/8, distance.calculate(original, sameStartShorter), 0);
        assertEquals(1-8.0/10, distance.calculate(original, sameStartLonger), 0);
        assertEquals(1-2.0/8, distance.calculate(original, sameEndShorter), 0);
        assertEquals(1-8.0/12, distance.calculate(original, sameEndLonger), 0);
        assertEquals(1-8.0/14, distance.calculate(original, containingWholeInterval), 0);
        assertEquals(1-4.0/8, distance.calculate(original, containingStart), 0);
        assertEquals(0, distance.calculate(original, containingEnd), 0);
        assertEquals(1-2.0/8, distance.calculate(original, contained), 0);
        assertEquals(1-2.0/8, distance.calculate(original, beforeWithGap), 0);
        assertEquals(1-4.0/8, distance.calculate(original, directlyBefore), 0);
        assertEquals(1-5.0/8, distance.calculate(original, afterWithGap), 0);
        assertEquals(1-3.0/8, distance.calculate(original, directlyAfter), 0);
    }

    @Test
    public void testStartDistance() {
        final StartDistance distance = new StartDistance();

        assertEquals(0, distance.calculate(original, identical), 0);
        assertEquals(0, distance.calculate(original, sameStartShorter), 0);
        assertEquals(0, distance.calculate(original, sameStartLonger), 0);
        assertEquals(6.0/8, distance.calculate(original, sameEndShorter), 0);
        assertEquals(4.0/12, distance.calculate(original, sameEndLonger), 0);
        assertEquals(2.0/14, distance.calculate(original, containingWholeInterval), 0);
        assertEquals(2.0/10, distance.calculate(original, containingStart), 0);
        assertEquals(5.0/13, distance.calculate(original, containingEnd), 0);
        assertEquals(2.0/8, distance.calculate(original, contained), 0);
        assertEquals(4.0/12, distance.calculate(original, beforeWithGap), 0);
        assertEquals(4.0/12, distance.calculate(original, directlyBefore), 0);
        assertEquals(11.0/16, distance.calculate(original, afterWithGap), 0);
        assertEquals(8.0/11, distance.calculate(original, directlyAfter), 0);
    }
}
