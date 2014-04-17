package net.meisen.dissertation.impl.parser.query.select.measures;

import net.meisen.dissertation.model.measures.IAggregationFunction;

public class MeasureExpression {
	private final String descModelId;
	private final IAggregationFunction function;

	public MeasureExpression(final String descModelId,
			final IAggregationFunction aggFunc) {
		this.descModelId = descModelId;
		this.function = aggFunc;
	}

	public IAggregationFunction getFunction() {
		return function;
	}

	public String getDescriptorModelId() {
		return descModelId;
	}

	@Override
	public String toString() {
		return function + "(" + descModelId + ")";
	}
}
