package net.meisen.master.meike.impl.mapping.upperBounds;

import net.meisen.master.meike.impl.distances.datasets.Dataset;
import net.meisen.master.meike.impl.mapping.CostMatrix;
import net.meisen.master.meike.impl.mapping.IDatasetMinCostMapper;
import net.meisen.master.meike.impl.mapping.Mapping;

/**
 * Heuristic proposed by Amponsah, Otoo, Salhi and Quayson (AJOR 2016), based
 * on row and column penalties. The paper only roughly describes the
 */
public class Amponsah implements IDatasetMinCostMapper {
    @Override
    public Mapping calculateMinimumCostMapping(Dataset original, Dataset other) {
        // Todo
        return null;
    }

    @Override
    public Mapping calculateMinimumCostMapping(CostMatrix costMatrix) {
        // Todo
        return null;
    }
}
