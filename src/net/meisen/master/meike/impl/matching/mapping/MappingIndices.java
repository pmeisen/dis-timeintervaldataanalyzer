package net.meisen.master.meike.impl.matching.mapping;

import com.google.common.collect.ImmutableList;

import java.util.List;

/**
 * Created by meike on 30.04.17.
 */
public class MappingIndices {
    private final List<Integer> indices;

    private MappingIndices(final List<Integer> indices) {
        this.indices = ImmutableList.copyOf(indices);
    }
}
