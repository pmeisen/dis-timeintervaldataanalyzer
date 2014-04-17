package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.measures.BaseAggregationFunction;

public class Min extends BaseAggregationFunction {
	private final static String name = "min";

	@Override
	public String getName() {
		return name;
	}
}
