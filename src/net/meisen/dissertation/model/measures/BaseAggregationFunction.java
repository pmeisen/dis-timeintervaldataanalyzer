package net.meisen.dissertation.model.measures;

public abstract class BaseAggregationFunction implements IAggregationFunction {

	@Override
	public IIntermediateResult aggregate(final IIntermediateResult prevResult) {
		return null;
	}
	
	@Override
	public String toString() {
		return getName();
	}
}
