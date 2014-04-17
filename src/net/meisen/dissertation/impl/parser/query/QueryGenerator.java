package net.meisen.dissertation.impl.parser.query;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import net.meisen.dissertation.exceptions.QueryParsingException;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarBaseListener;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompDescriptorEqualContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompGroupIgnoreContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprAggregateContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprCompContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprGroupContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprIntervalContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprSelectContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorAggrFunctionContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDateIntervalContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDescValueContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDescValueTupelContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDescriptorIdContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorIntIntervalContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorModelIdContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorSelectTypeContext;
import net.meisen.dissertation.impl.parser.query.select.DateIntervalValue;
import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;
import net.meisen.dissertation.impl.parser.query.select.Interval;
import net.meisen.dissertation.impl.parser.query.select.IntervalType;
import net.meisen.dissertation.impl.parser.query.select.LongIntervalValue;
import net.meisen.dissertation.impl.parser.query.select.ResultType;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.group.GroupExpression;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperator;
import net.meisen.dissertation.model.measures.AggregationFunctionHandler;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.genmisc.types.Strings;

import org.antlr.v4.runtime.ParserRuleContext;

/**
 * A generator to generate a {@code Query} from a {@code QueryGrammarParser}.
 * 
 * @see QueryGrammarParser
 * @see IQuery
 * 
 * @author pmeisen
 */
public class QueryGenerator extends QueryGrammarBaseListener {
	private final AggregationFunctionHandler aggFuncHandler;
	private final boolean optimize;

	private IQuery query;
	private boolean finalized = false;

	/**
	 * Generates a {@code QueryGenerator} which will trigger optimization after
	 * complete generation.
	 * 
	 * @param aggFuncHandler
	 *            the {@code AggregationFunctionHandler} used to resolve a used
	 *            aggregation, can be {@code null} if not functions should be
	 *            resolved
	 */
	public QueryGenerator(final AggregationFunctionHandler aggFuncHandler) {
		this(aggFuncHandler, true);
	}

	/**
	 * Generates a {@code QueryGenerator} which will optimize (i.e.
	 * {@code optimize} is {@code true}) or not optimize (i.e. {@code optimize}
	 * is {@code false}) the created query.
	 * 
	 * @param aggFuncHandler
	 *            the {@code AggregationFunctionHandler} used to resolve a used
	 *            aggregation, can be {@code null} if not functions should be
	 *            resolved
	 * @param optimize
	 *            {@code true} if the created query should be optimized,
	 *            otherwise {@code false}
	 */
	public QueryGenerator(final AggregationFunctionHandler aggFuncHandler,
			final boolean optimize) {
		this.aggFuncHandler = aggFuncHandler;
		this.optimize = optimize;
	}

	/**
	 * Create the {@code query} and mark the process to be in-progress, i.e.
	 * {@link #isFinalized()} returns {@code false}.
	 */
	@Override
	public void enterExprSelect(final ExprSelectContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new SelectQuery();
	}

	/**
	 * Checks if the generation is finalized, i.e. {@code true} is returned.
	 * 
	 * @return the generation is finalized (i.e. {@code true}), if not finalized
	 *         {@code false} is returned
	 */
	public boolean isFinalized() {
		return this.finalized;
	}

	@Override
	public void exitExprSelect(final ExprSelectContext ctx) {
		if (isOptimize()) {
			q(SelectQuery.class).optimize();
		}

		finalized = true;
	}

	@Override
	public void enterExprComp(final ExprCompContext ctx) {
		final LogicalOperator op = resolveLogicalOperator(ctx);

		if (op == null) {
			// do nothing
		} else {
			q(SelectQuery.class).getFilter().attach(op);
		}
	}

	@Override
	public void exitExprComp(final ExprCompContext ctx) {

		// if we reached the parent logic we don't have to move
		if (ctx.getParent() instanceof ExprSelectContext) {
			return;
		}

		// check the not
		final LogicalOperator op = resolveLogicalOperator(ctx);
		if (op == null) {
			// do nothing
		} else {
			q(SelectQuery.class).getFilter().moveUp();
		}
	}

	@Override
	public void exitCompDescriptorEqual(final CompDescriptorEqualContext ctx) {
		final String id = getDescriptorModelId(ctx.selectorDescriptorId());
		final String value = getDescValue(ctx.selectorDescValue());

		final DescriptorComperator descCmp = new DescriptorComperator(id, value);
		q(SelectQuery.class).getFilter().attach(descCmp);
	}

	@Override
	public void exitExprGroup(final ExprGroupContext ctx) {

		// validate the created group
		if (!q(SelectQuery.class).getGroup().isValid()) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1008, ctx.getText());
		}
	}

	@Override
	public void exitExprInterval(final ExprIntervalContext ctx) {

		// determine the types of the interval
		final IntervalType openType = resolveIntervalType(ctx
				.selectorOpenInterval());
		final IntervalType closeType = resolveIntervalType(ctx
				.selectorCloseInterval());

		// determine the values
		final SelectorDateIntervalContext dateCtx = ctx.selectorDateInterval();
		final SelectorIntIntervalContext intCtx = ctx.selectorIntInterval();

		// create the interval
		final Interval<?> interval;
		if (dateCtx != null) {
			final Date date1 = Dates.isDate(dateCtx.DATE(0).getText(),
					Dates.GENERAL_TIMEZONE);
			final Date date2 = Dates.isDate(dateCtx.DATE(1).getText(),
					Dates.GENERAL_TIMEZONE);
			interval = new Interval<Date>(new DateIntervalValue(date1),
					openType, new DateIntervalValue(date2), closeType);
		} else if (intCtx != null) {
			final long val1 = Long.parseLong(intCtx.INT(0).getText());
			final long val2 = Long.parseLong(intCtx.INT(1).getText());
			interval = new Interval<Long>(new LongIntervalValue(val1),
					openType, new LongIntervalValue(val2), closeType);
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1004, ctx.getText());
		}

		q(SelectQuery.class).setInterval(interval);
	}

	@Override
	public void exitSelectorAggrFunction(final SelectorAggrFunctionContext ctx) {
		final String descModelId = getDescriptorModelId(ctx
				.selectorDescriptorId());

		final String functionName = ctx.selectorAggrFunctionName().getText();
		final IAggregationFunction aggFunc = resolveAggregationFunction(functionName);
		
		q(SelectQuery.class).addMeasure(descModelId, aggFunc);
	}

	@Override
	public void exitSelectorSelectType(final SelectorSelectTypeContext ctx) {
		final ResultType type = resolveResultType(ctx);
		q(SelectQuery.class).setResultType(type);
	}

	@Override
	public void exitSelectorModelId(final SelectorModelIdContext ctx) {
		q(SelectQuery.class).setModelId(getModelId(ctx));
	}

	@Override
	public void enterExprAggregate(
			final QueryGrammarParser.ExprAggregateContext ctx) {
	}

	@Override
	public void exitExprAggregate(final ExprAggregateContext ctx) {
		final List<String> identifiers = new ArrayList<String>();

		// get all the defined identifiers
		for (final SelectorDescriptorIdContext ctxId : ctx
				.selectorDescriptorId()) {
			identifiers.add(getDescriptorModelId(ctxId));
		}

		// set the retrieved identifiers
		q(SelectQuery.class).getGroup().setDescriptors(identifiers);
	}

	@Override
	public void exitCompGroupIgnore(final CompGroupIgnoreContext ctx) {
		final GroupExpression groupExpr = q(SelectQuery.class).getGroup();
		for (final SelectorDescValueTupelContext descValueCtx : ctx
				.selectorDescValueTupel()) {

			// determine the values
			final List<String> values = new ArrayList<String>();
			for (final SelectorDescValueContext selectorDesc : descValueCtx
					.selectorDescValue()) {
				values.add(getDescValue(selectorDesc));
			}

			// add an exclusion
			groupExpr.addExclusion(values);
		}
	}

	/**
	 * Gets the {@code query} casted to the needed return type.
	 * 
	 * @return the {@code query} casted to the needed return type
	 */
	@SuppressWarnings("unchecked")
	protected <T extends IQuery> T q() {
		if (query == null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1006);
		}

		return (T) query;
	}

	/**
	 * Gets the {@code query} casted to the specified {@code clazz}.
	 * 
	 * @param clazz
	 *            the class to cast the current {@link IQuery} to
	 * 
	 * @return the casted type
	 */
	@SuppressWarnings("unchecked")
	protected <T extends IQuery> T q(final Class<T> clazz) {
		return (T) q();
	}

	/**
	 * Gets the query which was parsed.
	 * 
	 * @return the parsed {@code IQuery}
	 */
	public IQuery getQuery() {
		if (!finalized) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1007);
		}

		return query;
	}

	/**
	 * Gets the optimization setting, i.e. {@code true} is returned if the query
	 * is optimized, otherwise {@code false}.
	 * 
	 * @return the optimization setting, i.e. {@code true} is returned if the
	 *         query is optimized, otherwise {@code false}
	 */
	public boolean isOptimize() {
		return optimize;
	}

	/**
	 * Gets the defined descriptor value from the parsed string, i.e.
	 * {@code 'value' => value} or {@code 'v\\al\'ue' => v\al'ue}.
	 * 
	 * @param selectorDesc
	 *            the text to retrieve the descriptor value for
	 * @return the descriptors value
	 */
	protected String getDescValue(final SelectorDescValueContext selectorDesc) {
		if (selectorDesc.NULL_VALUE() != null) {
			return null;
		} else {

			// get the value the descriptor should have
			final String value = selectorDesc.DESC_VALUE().getText();
			return Strings.trimSequence(value, "'").replace("\\'", "'")
					.replace("\\\\", "\\");
		}
	}

	/**
	 * Gets the identifier defined by the specified
	 * {@code SelectorModelIdContext}.
	 * 
	 * @param ctx
	 *            the context to retrieve the identifier from
	 * 
	 * @return the identifier defined by the {@code SelectorModelIdContext}
	 */
	protected String getModelId(final SelectorModelIdContext ctx) {
		if (ctx.SIMPLE_ID() != null) {
			return ctx.SIMPLE_ID().getText();
		} else if (ctx.ENHANCED_ID() != null) {
			return ctx.ENHANCED_ID().getText();
		} else {
			return Strings.trimSequence(ctx.MARKED_ID().getText(), "\"");
		}
	}

	/**
	 * Gets the identifier defined by the specified
	 * {@code SelectorDescriptorIdContext}.
	 * 
	 * @param ctx
	 *            the context to retrieve the identifier from
	 * 
	 * @return the identifier defined by the {@code SelectorDescriptorIdContext}
	 */
	protected String getDescriptorModelId(final SelectorDescriptorIdContext ctx) {
		if (ctx.SIMPLE_ID() != null) {
			return ctx.SIMPLE_ID().getText();
		} else if (ctx.ENHANCED_ID() != null) {
			return ctx.ENHANCED_ID().getText();
		} else {
			return Strings.trimSequence(ctx.MARKED_ID().getText(), "\"");
		}
	}

	/**
	 * Resolves the name of the function to the concrete implementation of the
	 * function.
	 * 
	 * @param functionName
	 *            the name of the function
	 * 
	 * @return the instance of the {@code AggregationFunction}
	 * 
	 * @throws QueryParsingException
	 *             if the function cannot be resolved, more detailed the
	 *             {@code QueryParsingException} is wrapped within a
	 *             {@code ForwardedRuntimeException}
	 */
	protected IAggregationFunction resolveAggregationFunction(
			final String functionName) throws QueryParsingException {
		if (aggFuncHandler == null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1010, functionName);
		}

		final IAggregationFunction func = aggFuncHandler.resolve(functionName);
		if (func == null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1009, functionName);
		} else {
			return func;
		}
	}

	/**
	 * Resolves the {@code LogicalOperator} based on the specified context.
	 * 
	 * @param ctx
	 *            the context used to resolve the
	 * 
	 * @return the resolved {@code LogicalOperator}, can be {@code null} if it
	 *         cannot be resolved
	 */
	protected LogicalOperator resolveLogicalOperator(final ParserRuleContext ctx) {

		if (ctx.getToken(QueryGrammarParser.LOGICAL_AND, 0) != null) {
			return LogicalOperator.AND;
		} else if (ctx.getToken(QueryGrammarParser.LOGICAL_OR, 0) != null) {
			return LogicalOperator.OR;
		} else if (ctx.getToken(QueryGrammarParser.LOGICAL_NOT, 0) != null) {
			return LogicalOperator.NOT;
		} else {
			return null;
		}
	}

	/**
	 * Determine the type of the interval based on the passed context of the
	 * parser.
	 * 
	 * @param ctx
	 *            the context of the parser to be checked
	 * 
	 * @return the determined {@code IntervalType}
	 * 
	 * @throws QueryParsingException
	 *             if the {@code IntervalType} cannot be resolved, more detailed
	 *             the {@code QueryParsingException} is wrapped within a
	 *             {@code ForwardedRuntimeException}
	 */
	protected IntervalType resolveIntervalType(final ParserRuleContext ctx)
			throws QueryParsingException {
		if (ctx.getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0) != null) {
			return IntervalType.EXCLUDE;
		} else if (ctx.getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0) != null) {
			return IntervalType.EXCLUDE;
		} else if (ctx.getToken(QueryGrammarParser.BRACKET_SQUARE_OPENED, 0) != null) {
			return IntervalType.INCLUDE;
		} else if (ctx.getToken(QueryGrammarParser.BRACKET_SQUARE_CLOSED, 0) != null) {
			return IntervalType.INCLUDE;
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1002, ctx.getText());
		}
	}

	/**
	 * Determines the {@code ResultType} from the context.
	 * 
	 * @param ctx
	 *            the context of the parser to be checked
	 * 
	 * @return the resolved {@code ResultType}
	 * 
	 * @throws QueryParsingException
	 *             if the {@code IntervalType} cannot be resolved, more detailed
	 *             the {@code QueryParsingException} is wrapped within a
	 *             {@code ForwardedRuntimeException}
	 */
	protected ResultType resolveResultType(final SelectorSelectTypeContext ctx)
			throws QueryParsingException {

		if (ctx.getToken(QueryGrammarParser.TYPE_RECORDS, 0) != null) {
			return ResultType.RECORDS;
		} else if (ctx.getToken(QueryGrammarParser.TYPE_TIMESERIES, 0) != null) {
			return ResultType.TIMESERIES;
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1005, ctx.getText());
		}
	}
}
