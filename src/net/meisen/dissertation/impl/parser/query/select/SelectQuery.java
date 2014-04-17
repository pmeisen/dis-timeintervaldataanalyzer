package net.meisen.dissertation.impl.parser.query.select;

import net.meisen.dissertation.impl.parser.query.select.evaluator.SelectEvaluator;
import net.meisen.dissertation.impl.parser.query.select.evaluator.SelectResult;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLogicTree;
import net.meisen.dissertation.impl.parser.query.select.logical.GroupExpression;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.parser.query.IQuery;

public class SelectQuery implements IQuery {

	private final DescriptorLogicTree filter;
	private final GroupExpression group;

	private String modelId;
	private ResultType type;
	private Interval<?> interval;

	public SelectQuery() {
		filter = new DescriptorLogicTree();
		group = new GroupExpression();
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
	public SelectResult evaluate(final TidaModel model) {
		final SelectEvaluator evaluator = new SelectEvaluator(model);
		return evaluator.evaluate(this);
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
	
	/**
	 * Gets the defined {@code GroupExpresion} for the select query.
	 * 
	 * @return the defined {@code GroupExpresion}
	 */
	public GroupExpression getGroup() {
		return group;
	}
	
	public void getMeasures() {
		
	}
}
