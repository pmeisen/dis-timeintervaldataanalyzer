package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.measures.BaseAggregationFunction;

public class Mode extends BaseAggregationFunction {
	private final static String name = "mode";

	@Override
	public String getName() {
		return name;
	}
}
