package net.meisen.dissertation.impl.parser.query.select;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import net.meisen.dissertation.impl.parser.query.DimensionSelector;
import net.meisen.dissertation.impl.parser.query.Interval;
import net.meisen.dissertation.impl.parser.query.select.evaluator.SelectEvaluator;
import net.meisen.dissertation.impl.parser.query.select.group.GroupExpression;
import net.meisen.dissertation.impl.parser.query.select.logical.DescriptorLogicTree;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.jdbc.protocol.QueryType;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.auth.permissions.Permission;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IResourceResolver;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;

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
	private SelectResultType type;
	private Interval<?> interval;
	private boolean transposed;
	private boolean idsOnly;
	private boolean count;
	private IntervalRelation intervalRelation;
	private DimensionSelector measureDimension;

	/**
	 * Default constructor initializing the query.
	 */
	public SelectQuery() {
		filter = new DescriptorLogicTree();
		group = new GroupExpression();
		measures = new ArrayList<DescriptorMathTree>();

		transposed = false;
		idsOnly = false;
		count = false;
		intervalRelation = null;
		measureDimension = null;
	}

	/**
	 * Sets the {@code ResultType}
	 * 
	 * @param type
	 */
	public void setResultType(final SelectResultType type) {
		this.type = type;
	}

	/**
	 * Gets the {@code ResultType} of the select statement.
	 * 
	 * @return the {@code ResultType}
	 */
	public SelectResultType getResultType() {
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
	public SelectResult evaluate(final IAuthManager authManager,
			final TidaModelHandler handler, final TidaModel model,
			final IResourceResolver resolver) throws ForwardedRuntimeException {
		final SelectEvaluator evaluator = new SelectEvaluator(model);
		return evaluator.evaluate(this);
	}

	@Override
	public String getModelId() {
		return modelId;
	}

	@Override
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
	 * Checks if the query has a measure using a function of the specified
	 * usage-type.
	 * 
	 * @param type
	 *            the type to be checked
	 * 
	 * @return {@code true} if a function uses the specified function (i.e.
	 *         {@link IAggregationFunction#getDefinedType()} is equal to the
	 *         specified {@code type}), otherwise {@code false}
	 */
	public boolean usesFunction(final Class<? extends IAggregationFunction> type) {

		// check the measures
		for (final DescriptorMathTree measure : measures) {
			if (measure.usesFunction(type)) {
				return true;
			}
		}

		// if not than return false
		return false;
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

	@Override
	public boolean expectsModel() {
		return true;
	}

	/**
	 * Specifies if the result of the selection should be transposed.
	 * 
	 * @param transposed
	 *            {@code true} if it should be transposed, otherwise
	 *            {@code false}
	 */
	public void setTransposed(final boolean transposed) {
		this.transposed = transposed;
	}

	/**
	 * Checks if the result should be transposed.
	 * 
	 * @return {@code true} if the result should be transposed, otherwise
	 *         {@code false}
	 */
	public boolean isTransposed() {
		return transposed;
	}

	/**
	 * Specifies if the result of the selection should just be the identifiers
	 * of records.
	 * 
	 * @param idsOnly
	 *            {@code true} if it should be just the identifiers, otherwise
	 *            {@code false}
	 */
	public void setIdsOnly(final boolean idsOnly) {
		this.idsOnly = idsOnly;
	}

	/**
	 * Checks if the result should be just the identifiers.
	 * 
	 * @return {@code true} if the result should be just the identifiers,
	 *         otherwise {@code false}
	 */
	public boolean isIdsOnly() {
		return idsOnly;
	}

	/**
	 * Specifies if the result of the selection should just be the count of
	 * records.
	 * 
	 * @param count
	 *            {@code true} if it should be just the count, otherwise
	 *            {@code false}
	 */
	public void setCount(final boolean count) {
		this.count = count;
	}

	/**
	 * Checks if the result should be just the count.
	 * 
	 * @return {@code true} if the result should be just the count, otherwise
	 *         {@code false}
	 */
	public boolean isCount() {
		return count;
	}

	/**
	 * Sets the {@code IntervalRelation} defined for the query.
	 * 
	 * @param relation
	 *            the {@code IntervalRelation} defined for the query
	 */
	public void setIntervalRelation(final IntervalRelation relation) {
		this.intervalRelation = relation;
	}

	/**
	 * Gets the {@code IntervalRelation} defined for the query, which might be
	 * {@code null} if none was defined at all.
	 * 
	 * @return the {@code IntervalRelation} defined for the query
	 */
	public IntervalRelation getIntervalRelation() {
		return intervalRelation;
	}

	@Override
	public QueryType getQueryType() {
		return QueryType.QUERY;
	}

	@Override
	public void enableIdCollection(final boolean enableIdCollection) {
		// ignore not supported
	}

	@Override
	public DefinedPermission[][] getNeededPermissions() {
		return new DefinedPermission[][] {
				new DefinedPermission[] { Permission.query.create(modelId) },
				new DefinedPermission[] { Permission.queryAll.create() } };
	}

	/**
	 * Gets the dimension specified for the measures to be selected.
	 * 
	 * @return the dimension specified for the measures to be selected
	 */
	public DimensionSelector getMeasureDimension() {
		return measureDimension;
	}

	/**
	 * Sets the dimension for the measures to be selected.
	 * 
	 * @param measureDimension
	 *            the dimension for the measures to be selected
	 */
	public void setMeasureDimension(final DimensionSelector measureDimension) {
		this.measureDimension = measureDimension;
	}
}
