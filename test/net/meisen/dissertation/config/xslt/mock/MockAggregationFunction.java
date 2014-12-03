package net.meisen.dissertation.config.xslt.mock;

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
}
