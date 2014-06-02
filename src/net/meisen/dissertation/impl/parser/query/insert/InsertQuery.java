package net.meisen.dissertation.impl.parser.query.insert;

import java.util.List;

import net.meisen.dissertation.impl.parser.query.select.Interval;
import net.meisen.dissertation.impl.parser.query.select.IntervalType;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryResult;

public class InsertQuery implements IQuery {

	private String modelId;

	private IntervalType startIntervalType;
	private IntervalType endIntervalType;

	private List<String> descIds;

	@Override
	public String getModelId() {
		return modelId;
	}

	@Override
	public void setModelId(final String modelId) {
		this.modelId = modelId;
	}

	@Override
	public IQueryResult evaluate(final TidaModel model) {
		return null;
	}

	public void setStartIntervalType(final IntervalType intervalType) {
		this.startIntervalType = intervalType;
	}

	public void setEndIntervalType(final IntervalType intervalType) {
		this.endIntervalType = intervalType;
	}

	public IntervalType getStartIntervalType() {
		return startIntervalType;
	}

	public IntervalType getEndIntervalType() {
		return endIntervalType;
	}

	public int sizeOfDescriptors() {
		return descIds.size();
	}

	public void addData(final Interval<?> interval, final List<String> data) {

	}

	public void setDescriptorModels(final List<String> ids) {
		this.descIds = ids;
	}

	@Override
	public String toString() {
		return "(" + startIntervalType + ", " + endIntervalType + ", "
				+ descIds + ")";
	}
}
