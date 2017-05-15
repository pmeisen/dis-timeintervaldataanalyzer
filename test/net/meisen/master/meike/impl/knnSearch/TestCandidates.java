package net.meisen.master.meike.impl.knnSearch;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;
import org.junit.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

public class TestCandidates {
    private final BoundedDataset boundedDataset1 =
            BoundedDataset.from(mock(Dataset.class), 0, 5);
    private final BoundedDataset boundedDataset2 =
            BoundedDataset.from(mock(Dataset.class), 4, 5);
    private final BoundedDataset boundedDataset3 =
            BoundedDataset.from(mock(Dataset.class), 5, 9);
    private final BoundedDataset boundedDataset4 =
            BoundedDataset.from(mock(Dataset.class), 2, 4);
    private final BoundedDataset boundedDataset5 =
            BoundedDataset.from(mock(Dataset.class), 3, 3);
    private final BoundedDataset boundedDataset6 =
            BoundedDataset.from(mock(Dataset.class), 1, 3);

    @Test
    public void remainingCandidatesAreCorrect() {
        final Candidates candidates = Candidates.ofSize(3);

        candidates.add(boundedDataset1);
        assertEquals(ImmutableList.of(boundedDataset1),
                candidates.getRemainingCandidates());

        candidates.add(boundedDataset2);
        assertEquals(ImmutableList.of(boundedDataset1, boundedDataset2),
                candidates.getRemainingCandidates());

        candidates.add(boundedDataset3);
        assertEquals(ImmutableList.of(boundedDataset1, boundedDataset2, boundedDataset3),
                candidates.getRemainingCandidates());

        candidates.add(boundedDataset4);
        assertEquals(ImmutableList.of(boundedDataset1, boundedDataset4, boundedDataset2),
                candidates.getRemainingCandidates());

        candidates.add(boundedDataset5);
        assertEquals(ImmutableList.of(boundedDataset1, boundedDataset4, boundedDataset5, boundedDataset2),
                candidates.getRemainingCandidates());

        candidates.add(boundedDataset6);
        assertEquals(ImmutableList.of(boundedDataset1, boundedDataset6, boundedDataset4, boundedDataset5),
                candidates.getRemainingCandidates());

        boundedDataset1.setLowerBound(5);
        assertEquals(ImmutableList.of(boundedDataset6, boundedDataset4, boundedDataset5),
                candidates.getRemainingCandidates());
    }
}
