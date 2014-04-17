package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.measures.BaseAggregationFunction;

public class Max extends BaseAggregationFunction {
	private final static String name = "max";

	@Override
	public String getName() {
		return name;
	}
}
