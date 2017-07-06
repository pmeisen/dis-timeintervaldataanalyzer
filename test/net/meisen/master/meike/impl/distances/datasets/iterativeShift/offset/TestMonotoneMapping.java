package net.meisen.master.meike.impl.distances.datasets.iterativeShift.offset;

import com.google.common.collect.ImmutableList;
import javafx.util.Pair;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.distances.intervals.Interval;
import net.meisen.master.meike.impl.distances.intervals.StartDistance;
import org.junit.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class TestMonotoneMapping {
    private final Dataset original = new Dataset(ImmutableList.of(
            new Interval(0, 2),
            new Interval(4, 6),
            new Interval(7, 9),
            new Interval(8, 12),
            new Interval(13, 15),
            new Interval(14, 16)));
    private final Dataset other = new Dataset(ImmutableList.of(
            new Interval(0, 2),
            new Interval(4, 6),
            new Interval(7, 9),
            new Interval(8, 12),
            new Interval(13, 15),
            new Interval(14, 16)));

    @Test
    public void testCorrectNumberOfListsAreGenerated() {
        final MonotoneMapping mapping = MonotoneMapping.fromDistance(new StartDistance(), 2);
        final List<List<Pair<Integer, Integer>>> firstTest = mapping.getIndexPairs(7, 4);
        final List<List<Pair<Integer, Integer>>> secondTest = mapping.getIndexPairs(6, 3);
        final List<List<Pair<Integer, Integer>>> thirdTest = mapping.getIndexPairs(6, 7);

        assertEquals(8, firstTest.size());
        assertEquals(8, secondTest.size());
        assertEquals(6, thirdTest.size());
    }
}
