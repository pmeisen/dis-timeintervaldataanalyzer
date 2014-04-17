package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.measures.BaseAggregationFunction;

public class Sum extends BaseAggregationFunction {
	private final static String name = "sum";

	@Override
	public String getName() {
		return name;
	}
}
