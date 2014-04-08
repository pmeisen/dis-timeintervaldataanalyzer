package net.meisen.dissertation.impl.parser.query.select;

import java.util.List;

import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLeaf;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLogicEvaluator;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLogicTree;
import net.meisen.dissertation.impl.parser.query.select.logical.ITreeElement;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperator;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperatorNode;
import net.meisen.dissertation.model.data.MetaDataModel;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.descriptors.Descriptor;
import net.meisen.dissertation.model.descriptors.DescriptorModel;
import net.meisen.dissertation.model.indexes.BaseIndexFactory;
import net.meisen.dissertation.model.indexes.datarecord.TidaIndex;
import net.meisen.dissertation.model.indexes.datarecord.bitmap.Bitmap;
import net.meisen.dissertation.model.indexes.datarecord.slices.IndexDimensionSlice;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

public class SelectQuery implements IQuery {

	private final DescriptorLogicTree filter;

	private String modelId;
	private ResultType type;
	private Interval<?> interval;

	public SelectQuery() {
		filter = new DescriptorLogicTree();
	}

	public void setResultType(final ResultType type) {
		this.type = type;
	}

	public ResultType getResultType() {
		return type;
	}

	@Override
	public String toString() {
		return "select " + type + " in " + interval + " filter " + filter;
	}

	public Interval<?> getInterval() {
		return interval;
	}

	public void setInterval(final Interval<?> interval) {
		this.interval = interval;
	}

	public DescriptorLogicTree getFilter() {
		return filter;
	}

	public void optimize() {
		filter.optimize();
	}

	@Override
	public SelectQueryResult evaluate(final TidaModel model) {
		final DescriptorLogicEvaluator evaluator = new DescriptorLogicEvaluator(
				model);
		final SelectQueryResult queryResult = new SelectQueryResult();

		final Bitmap filterBitmap = evaluator.evaluateTree(filter);
		queryResult.setFilterResult(filterBitmap);

		return queryResult;
	}

	@Override
	public String getModelId() {
		return modelId;
	}

	/**
	 * Sets the {@code modelId} for the query.
	 * 
	 * @param modelId
	 *            the {@code modelId} of the query
	 */
	public void setModelId(final String modelId) {
		this.modelId = modelId;
	}

}
