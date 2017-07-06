package net.meisen.master.meike.correctness;

import com.google.common.collect.ImmutableList;
import net.meisen.master.meike.impl.distances.datasets.Dataset;

import java.util.List;

/**
 * Utility class for wrapping the original dataset and some candidates.
 */
class Datasets {
    public final Dataset original;
    public final ImmutableList<Dataset> candidates;

    public Datasets(final Dataset original, final List<Dataset> candidates) {
        this.original = original;
        this.candidates = ImmutableList.copyOf(candidates);
    }
}
