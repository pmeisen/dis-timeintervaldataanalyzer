package net.meisen.dissertation.impl.parser.query.select;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.meisen.dissertation.impl.parser.query.select.evaluator.SelectEvaluator;
import net.meisen.dissertation.impl.parser.query.select.evaluator.SelectResult;
import net.meisen.dissertation.impl.parser.query.select.group.GroupExpression;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLogicTree;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.parser.query.IQuery;

/**
 * A {@code SelectQuery} is a {@code Query} used to retrieve data from the
 * underlying database.
 * 
 * @author pmeisen
 * 
 * @see IQuery
 * 
 */
public class SelectQuery implements IQuery {

	private final DescriptorLogicTree filter;
	private final GroupExpression group;
	private final List<DescriptorMathTree> measures;

	private String modelId;
	private ResultType type;
	private Interval<?> interval;

	/**
	 * Default constructor initializing the query.
	 */
	public SelectQuery() {
		filter = new DescriptorLogicTree();
		group = new GroupExpression();
		measures = new ArrayList<DescriptorMathTree>();
	}

	/**
	 * Sets the {@code ResultType}
	 * 
	 * @param type
	 */
	public void setResultType(final ResultType type) {
		this.type = type;
	}

	/**
	 * Gets the {@code ResultType} of the select statement.
	 * 
	 * @return the {@code ResultType}
	 */
	public ResultType getResultType() {
		return type;
	}

	@Override
	public String toString() {
		return "select " + type + " in " + interval + " filter " + filter;
	}

	/**
	 * Gets the interval for the statement.
	 * 
	 * @return the interval for the statement
	 */
	public Interval<?> getInterval() {
		return interval;
	}

	/**
	 * Sets the interval for the statement.
	 * 
	 * @param interval
	 *            the interval for the statement
	 */
	public void setInterval(final Interval<?> interval) {
		this.interval = interval;
	}

	/**
	 * Gets the filter defined for the select statement.
	 * 
	 * @return the filter defined for the select statement
	 */
	public DescriptorLogicTree getFilter() {
		return filter;
	}

	/**
	 * Optimizes the select query.
	 */
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

	/**
	 * Gets a list of the defined measures.
	 * 
	 * @return a list of the defined measures
	 */
	public List<DescriptorMathTree> getMeasures() {
		return Collections.unmodifiableList(measures);
	}

	/**
	 * Adds a measure to the select statement.
	 * 
	 * @param mathTree
	 *            the measure to be added
	 */
	public void addMeasure(final DescriptorMathTree mathTree) {
		measures.add(mathTree);
	}
}
