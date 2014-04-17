package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.measures.BaseAggregationFunction;

public class Median extends BaseAggregationFunction {
	private final static String name = "median";

	@Override
	public String getName() {
		return name;
	}
}
