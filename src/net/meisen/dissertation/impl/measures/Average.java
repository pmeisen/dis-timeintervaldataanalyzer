package net.meisen.dissertation.impl.measures;

import net.meisen.dissertation.model.measures.BaseAggregationFunction;

public class Average extends BaseAggregationFunction {
	private final static String name = "average";

	@Override
	public String getName() {
		return name;
	}
}
