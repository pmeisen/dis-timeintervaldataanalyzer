package net.meisen.dissertation.config.xslt.mock;

import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.FactDescriptorSet;
import net.meisen.dissertation.model.measures.BaseAggregationFunction;

/**
 * Simply mocked aggregation function for testing.
 * 
 * @author pmeisen
 * 
 */
public class MockAggregationFunction extends BaseAggregationFunction {

	@Override
	public String getName() {
		return "MOCK";
	}

	@Override
	public double aggregate(TidaIndex index, Bitmap bitmap,
			FactDescriptorSet descriptors) {
		return getDefaultValue();
	}

	@Override
	public double aggregate(TidaIndex index, Bitmap bitmap, double[] facts) {
		return getDefaultValue();
	}
}
