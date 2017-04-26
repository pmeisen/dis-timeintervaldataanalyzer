package net.meisen.master.meike.impl.knnSearch;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;

public class TestNearestNeighbors {
    private final Dataset mockDataset = mock(Dataset.class);

    @Test
    public void testLargestDistance() {
        final NearestNeighbors nearestNeighbors = NearestNeighbors.ofSize(3);
        nearestNeighbors.add(Neighbor.from(mockDataset, 5.0));
        assertEquals(Double.MAX_VALUE, nearestNeighbors.getLargestDistance(), 0.00001);

        nearestNeighbors.add(Neighbor.from(mockDataset, 4.2));
        assertEquals(Double.MAX_VALUE, nearestNeighbors.getLargestDistance(), 0.00001);

        nearestNeighbors.add(Neighbor.from(mockDataset,1.2));
        assertEquals(5.0, nearestNeighbors.getLargestDistance(), 0.00001);

        nearestNeighbors.add(Neighbor.from(mockDataset, 2.3));
        assertEquals(4.2, nearestNeighbors.getLargestDistance(), 0.00001);

        nearestNeighbors.add(Neighbor.from(mockDataset, 2.5));
        assertEquals(2.5, nearestNeighbors.getLargestDistance(), 0.00001);
    }

    @Test
    public void testNearestNeighborRetrieval() {
        final NearestNeighbors nearestNeighbors = NearestNeighbors.ofSize(2);

        final Neighbor neighborOne = Neighbor.from(mockDataset, 5.0);
        nearestNeighbors.add(neighborOne);
        assertEquals(ImmutableList.of(neighborOne), nearestNeighbors.getNeighbors());

        final Neighbor neighborTwo = Neighbor.from(mockDataset, 4.0);
        nearestNeighbors.add(neighborTwo);
        assertEquals(ImmutableList.of(neighborTwo, neighborOne), nearestNeighbors.getNeighbors());

        final Neighbor neighborThree = Neighbor.from(mockDataset, 4.6);
        nearestNeighbors.add(neighborThree);
        assertEquals(ImmutableList.of(neighborTwo, neighborThree), nearestNeighbors.getNeighbors());

        final Neighbor neighborFour = Neighbor.from(mockDataset, 2.1);
        nearestNeighbors.add(neighborFour);
        assertEquals(ImmutableList.of(neighborFour, neighborTwo), nearestNeighbors.getNeighbors());

        final Neighbor neighborFive = Neighbor.from(mockDataset, 3.1);
        nearestNeighbors.add(neighborFive);
        assertEquals(ImmutableList.of(neighborFour, neighborFive), nearestNeighbors.getNeighbors());

    }
}
