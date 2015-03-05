package net.meisen.dissertation.impl.parser.query.select;

import java.util.ArrayList;
import java.util.Collection;
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

	private final List<DescriptorMathTree> measures;

	private GroupExpression group;
	private DescriptorLogicTree filter;

	private String modelId;
	private SelectResultType type;
	private Interval<?> interval;
	private boolean transposed;
	private boolean idsOnly;
	private boolean count;
	private IntervalRelation intervalRelation;
	private DimensionSelector measureDimension;
	private int limit;
	private int offset;

	/**
	 * Default constructor initializing the query.
	 */
	public SelectQuery() {
		measures = new ArrayList<DescriptorMathTree>();

		limit = -1;
		offset = 0;
		transposed = false;
		idsOnly = false;
		count = false;
		group = null;
		filter = null;
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
		return "select " + type + " in " + interval + " filter " + filter + " group " + group;
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
		if (filter == null) {
			filter = new DescriptorLogicTree();
		}

		return filter;
	}

	/**
	 * Sets the filter to be used.
	 * 
	 * @param filter
	 *            the filter to be used
	 */
	public void setFilter(final DescriptorLogicTree filter) {
		this.filter = filter;
	}

	/**
	 * Sets the group-expression to the one specified.
	 * 
	 * @param group
	 *            the group to be used
	 */
	public void setGroup(final GroupExpression group) {
		this.group = group;
	}

	/**
	 * Optimizes the select query.
	 */
	public void optimize() {
		if (filter != null) {
			filter.optimize();
		}
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
		if (group == null) {
			group = new GroupExpression();
		}

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
	 * Sets the specified measures, i.e. all other measures are removed.
	 * 
	 * @param measures
	 *            the measures to be set
	 */
	public void setMeasures(final Collection<DescriptorMathTree> measures) {
		this.measures.clear();
		this.addMeasures(measures);
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
		this.measures.add(mathTree);
	}

	/**
	 * Add all the specified measures to {@code this}.
	 * 
	 * @param measures
	 *            the measures to be added
	 */
	public void addMeasures(Collection<DescriptorMathTree> measures) {
		this.measures.addAll(measures);
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

	/**
	 * Gets the defined limit. The limit is {@code -1} if all records should be
	 * returned, i.e. no limit is defined.
	 * 
	 * @return the defined limit
	 */
	public int getLimit() {
		return limit;
	}

	/**
	 * Gets the defined offset. The offset defines the position before the first
	 * record, i.e. an offset of {@code 0} returns the first record and
	 * following.
	 * 
	 * @return the defined offset
	 */
	public int getOffset() {
		return offset;
	}

	/**
	 * Sets the limit of the query, i.e. the amount of records maximal returned.
	 * 
	 * @param limit
	 *            the limit
	 */
	public void setLimit(final int limit) {
		if (limit < 0) {
			this.limit = -1;
		} else {
			this.limit = limit;
		}
	}

	/**
	 * Defines the offset of the query, i.e. the position on which the retrieval
	 * should start.
	 * 
	 * @param offset
	 *            the offset of the query
	 */
	public void setOffset(final int offset) {
		if (offset < 0) {
			this.offset = 0;
		} else {
			this.offset = offset;
		}
	}

	/**
	 * Checks if a limit is defined for {@code this}.
	 * 
	 * @return {@code true} if a limit is defined, otherwise {@code false}
	 */
	public boolean hasLimit() {
		return this.offset > 0 || this.limit > -1;
	}

	/**
	 * Sets all the values of {@code this} to the values specified by the passed
	 * {@code query}.
	 * 
	 * @param query
	 *            the query to get the values from
	 */
	public void set(final SelectQuery query) {
		setModelId(query.getModelId());
		setResultType(query.getResultType());

		setLimit(query.getLimit());
		setOffset(query.getOffset());

		setFilter(query.getFilter());
		setGroup(query.getGroup());

		setCount(query.isCount());
		setIdsOnly(query.isIdsOnly());
		setTransposed(query.isTransposed());

		setInterval(query.getInterval());
		setIntervalRelation(query.getIntervalRelation());

		setMeasureDimension(query.getMeasureDimension());
		setMeasures(query.getMeasures());
	}
}
