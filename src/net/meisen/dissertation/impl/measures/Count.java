package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.measures.BaseAggregationFunction;

public class Count extends BaseAggregationFunction {
	private final static String name = "count";

	@Override
	public String getName() {
		return name;
	}
}
