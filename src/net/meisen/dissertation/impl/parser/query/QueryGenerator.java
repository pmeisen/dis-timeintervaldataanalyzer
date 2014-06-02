package net.meisen.dissertation.impl.parser.query;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import net.meisen.dissertation.exceptions.QueryParsingException;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarBaseListener;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompAggrFunctionContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompDescValueTupelContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompDescriptorEqualContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompDescriptorFormulaAtomContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompDescriptorFormulaContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompGroupIgnoreContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompMeasureAtomContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompMeasureContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.CompNamedMeasureContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprAggregateContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprCompContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprGroupContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprInsertContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprIntervalContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprSelectContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprStructureContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.ExprValuesContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorAggrFunctionNameContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorAliasContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDateIntervalContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDateIntervalWithNullContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDescValueContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDescriptorIdContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorIntIntervalContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorIntIntervalWithNullContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorIntervalDefContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorModelIdContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorSelectTypeContext;
import net.meisen.dissertation.impl.parser.query.insert.InsertQuery;
import net.meisen.dissertation.impl.parser.query.select.DateIntervalValue;
import net.meisen.dissertation.impl.parser.query.select.DescriptorComperator;
import net.meisen.dissertation.impl.parser.query.select.Interval;
import net.meisen.dissertation.impl.parser.query.select.IntervalType;
import net.meisen.dissertation.impl.parser.query.select.LongIntervalValue;
import net.meisen.dissertation.impl.parser.query.select.ResultType;
import net.meisen.dissertation.impl.parser.query.select.SelectQuery;
import net.meisen.dissertation.impl.parser.query.select.group.GroupExpression;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.ArithmeticOperator;
import net.meisen.dissertation.impl.parser.query.select.measures.DescriptorMathTree;
import net.meisen.dissertation.model.measures.AggregationFunctionHandler;
import net.meisen.dissertation.model.measures.IAggregationFunction;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Dates;
import net.meisen.general.genmisc.types.Strings;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;

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
	private DescriptorMathTree mathExpr = null;

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

	@Override
	public void enterExprInsert(final ExprInsertContext ctx) {
		if (this.query != null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1001);
		}

		this.query = new InsertQuery();
	}

	@Override
	public void exitExprInsert(final ExprInsertContext ctx) {
		finalized = true;
	}

	@Override
	public void exitExprStructure(final ExprStructureContext ctx) {
		final InsertQuery q = q(InsertQuery.class);

		// determine the specified interval-types
		final SelectorIntervalDefContext intervalDefCtx = ctx
				.selectorIntervalDef();
		final IntervalType[] types = resolveIntervalType(intervalDefCtx);
		q.setStartIntervalType(types[0]);
		q.setEndIntervalType(types[1]);

		final List<String> ids = new ArrayList<String>();
		for (final SelectorDescriptorIdContext descIdCtx : ctx
				.selectorDescriptorId()) {
			ids.add(getDescriptorModelId(descIdCtx));
		}

		q.setDescriptorModels(ids);

		System.out.println(q);
	}

	@Override
	public void exitExprValues(final ExprValuesContext ctx) {
		final InsertQuery q = q(InsertQuery.class);

		ctx.selectorDescValue();

		// determine the values
		final SelectorDateIntervalWithNullContext dateCtx = ctx
				.selectorDateIntervalWithNull();
		final SelectorIntIntervalWithNullContext intCtx = ctx
				.selectorIntIntervalWithNull();
		final Interval<?> interval = resolveInterval(ctx.getText(), dateCtx,
				intCtx, q.getStartIntervalType(), q.getEndIntervalType());

		final List<String> data = new ArrayList<String>(q.sizeOfDescriptors());
		for (final SelectorDescValueContext descValueCtx : ctx
				.selectorDescValue()) {
			data.add(getDescValue(descValueCtx));
		}

		// add the record
		q.addData(interval, data);

		System.out.println(interval + ": " + data);
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

	@Override
	public void exitExprSelect(final ExprSelectContext ctx) {
		if (isOptimize()) {
			q(SelectQuery.class).optimize();
		}

		finalized = true;
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
	public void enterCompNamedMeasure(final CompNamedMeasureContext ctx) {
		final String id = getAlias(ctx.selectorAlias());
		mathExpr = new DescriptorMathTree(id);
	}

	@Override
	public void exitCompNamedMeasure(final CompNamedMeasureContext ctx) {
		q(SelectQuery.class).addMeasure(mathExpr);
	}

	@Override
	public void enterCompMeasure(final CompMeasureContext ctx) {

		// add the function to the tree
		if (ctx.selectorSecondMathOperator() != null) {
			final ArithmeticOperator ao = resolveArithmeticOperator(ctx
					.selectorSecondMathOperator());
			mathExpr.attach(ao);
		}
	}

	@Override
	public void exitCompMeasure(final CompMeasureContext ctx) {

		// check if we had an operator and we have to move up
		if (ctx.selectorSecondMathOperator() != null) {
			mathExpr.moveUp();
		}
	}

	@Override
	public void enterCompMeasureAtom(final CompMeasureAtomContext ctx) {
		if (ctx.selectorFirstMathOperator() != null) {
			final ArithmeticOperator ao = resolveArithmeticOperator(ctx
					.selectorFirstMathOperator());
			mathExpr.attach(ao);
		} else if (ctx.compAggrFunction() != null) {
			final CompAggrFunctionContext aggFuncCtx = ctx.compAggrFunction();
			mathExpr.attach(resolveAggregationFunction(aggFuncCtx
					.selectorAggrFunctionName()));
		}
	}

	@Override
	public void exitCompMeasureAtom(final CompMeasureAtomContext ctx) {
		if (ctx.selectorFirstMathOperator() != null) {
			mathExpr.moveUp();
		} else if (ctx.compAggrFunction() != null) {
			mathExpr.moveUp();
		}
	}

	@Override
	public void enterCompDescriptorFormula(
			final CompDescriptorFormulaContext ctx) {
		if (ctx.selectorSecondMathOperator() != null) {
			final ArithmeticOperator ao = resolveArithmeticOperator(ctx
					.selectorSecondMathOperator());
			mathExpr.attach(ao);
		}
	}

	@Override
	public void exitCompDescriptorFormula(final CompDescriptorFormulaContext ctx) {
		if (ctx.selectorSecondMathOperator() != null) {
			mathExpr.moveUp();
		}
	}

	@Override
	public void enterCompDescriptorFormulaAtom(
			final CompDescriptorFormulaAtomContext ctx) {
		if (ctx.selectorFirstMathOperator() != null) {
			final ArithmeticOperator ao = resolveArithmeticOperator(ctx
					.selectorFirstMathOperator());
			mathExpr.attach(ao);
		} else if (ctx.selectorDescriptorId() != null) {
			mathExpr.attach(ctx.selectorDescriptorId().getText());
		}
	}

	@Override
	public void exitCompDescriptorFormulaAtom(
			final CompDescriptorFormulaAtomContext ctx) {
		if (ctx.selectorFirstMathOperator() != null) {
			mathExpr.moveUp();
		}
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
		final Interval<?> interval = resolveInterval(ctx.getText(), dateCtx,
				intCtx, openType, closeType);

		q(SelectQuery.class).setInterval(interval);
	}

	@Override
	public void exitSelectorSelectType(final SelectorSelectTypeContext ctx) {
		final ResultType type = resolveResultType(ctx);
		q(SelectQuery.class).setResultType(type);
	}

	@Override
	public void exitSelectorModelId(final SelectorModelIdContext ctx) {
		q(IQuery.class).setModelId(getModelId(ctx));
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
		for (final CompDescValueTupelContext descValueCtx : ctx
				.compDescValueTupel()) {

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
	 * Gets the alias defined by the specified {@code SelectorAliasContext}.
	 * 
	 * @param ctx
	 *            the context to retrieve the alias from
	 * 
	 * @return the alias defined by the {@code SelectorAliasContext}
	 */
	protected String getAlias(final SelectorAliasContext ctx) {
		if (ctx == null) {
			return UUID.randomUUID().toString();
		} else if (ctx.SIMPLE_ID() != null) {
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
	 * @param ctx
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
			final ParserRuleContext ctx) throws QueryParsingException {
		if (aggFuncHandler == null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1010, ctx == null ? null : ctx.getText());
		}

		if (ctx instanceof SelectorAggrFunctionNameContext) {
			final String funcName = ctx.getText();
			final IAggregationFunction func = aggFuncHandler.resolve(funcName);
			if (func == null) {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1009, funcName);
			} else {
				return func;
			}
		} else {
			throw new IllegalArgumentException("The context '" + ctx
					+ "' does not contain any aggregation function.");
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
	 * Resolves the {@code ArithmeticOperator} based on the specified context.
	 * 
	 * @param ctx
	 *            the context used to resolve the
	 * 
	 * @return the resolved {@code ArithmeticOperator}, can be {@code null} if
	 *         it cannot be resolved
	 */
	protected ArithmeticOperator resolveArithmeticOperator(
			final ParserRuleContext ctx) {
		if (ctx.getToken(QueryGrammarParser.MATH_PLUS, 0) != null) {
			return ArithmeticOperator.ADD;
		} else if (ctx.getToken(QueryGrammarParser.MATH_MINUS, 0) != null) {
			return ArithmeticOperator.MINUS;
		} else if (ctx.getToken(QueryGrammarParser.MATH_MULTIPLY, 0) != null) {
			return ArithmeticOperator.MULTIPLY;
		} else if (ctx.getToken(QueryGrammarParser.MATH_DIVISION, 0) != null) {
			return ArithmeticOperator.DIVIDE;
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
	 * Resolves the type of an interval for a {@code SelectorIntervalDefContext}
	 * .
	 * 
	 * @param ctx
	 *            the context to resolve the type from
	 * @return the resolved type
	 * 
	 * @see SelectorIntervalDefContext
	 */
	protected IntervalType[] resolveIntervalType(
			final SelectorIntervalDefContext ctx) {

		final IntervalType[] types = new IntervalType[2];

		// determine the start
		if (ctx.POS_START_INCL() != null) {
			types[0] = IntervalType.INCLUDE;
		} else if (ctx.POS_START_EXCL() != null) {
			types[0] = IntervalType.EXCLUDE;
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1011, ctx.getText());
		}

		// determine the end
		if (ctx.POS_END_INCL() != null) {
			types[1] = IntervalType.INCLUDE;
		} else if (ctx.POS_END_EXCL() != null) {
			types[1] = IntervalType.EXCLUDE;
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1011, ctx.getText());
		}

		return types;
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

	/**
	 * Resolves the {@code Interval} from the specified
	 * {@code ParserRuleContext}. Currently only the
	 * {@code SelectorDateIntervalContext}, the
	 * {@code SelectorDateIntervalWithNullContext}, the
	 * {@code SelectorIntIntervalContext}, and the
	 * {@code SelectorIntIntervalWithNullContext} is supported.
	 * 
	 * @param ctxText
	 *            the text of the global context, used for error messages
	 * @param dateCtx
	 *            the date context
	 * @param intCtx
	 *            the int context
	 * @param openType
	 *            the type of the interval's start
	 * @param closeType
	 *            the type of the interval's end
	 * 
	 * @return the resolved interval
	 * 
	 * @see SelectorDateIntervalContext
	 * @see SelectorDateIntervalWithNullContext
	 * @see SelectorIntIntervalContext
	 * @see SelectorIntIntervalWithNullContext
	 */
	protected Interval<?> resolveInterval(final String ctxText,
			final ParserRuleContext dateCtx, final ParserRuleContext intCtx,
			final IntervalType openType, final IntervalType closeType) {

		// create the interval
		final Interval<?> interval;
		if (dateCtx != null) {
			final Date dates[] = new Date[2];

			// parse depending on the type
			if (dateCtx instanceof SelectorDateIntervalContext) {
				final SelectorDateIntervalContext c = (SelectorDateIntervalContext) dateCtx;

				dates[0] = Dates.isDate(c.DATE(0).getText(),
						Dates.GENERAL_TIMEZONE);
				dates[1] = Dates.isDate(c.DATE(1).getText(),
						Dates.GENERAL_TIMEZONE);
			} else if (dateCtx instanceof SelectorDateIntervalWithNullContext) {
				final SelectorDateIntervalWithNullContext c = (SelectorDateIntervalWithNullContext) dateCtx;

				int i = 0;
				for (ParseTree o : c.children) {
					if (o instanceof TerminalNode) {
						final Token symbol = ((TerminalNode) o).getSymbol();

						if (QueryGrammarParser.NULL_VALUE == symbol.getType()) {
							dates[i] = null;
							i++;
						} else if (QueryGrammarParser.DATE == symbol.getType()) {
							dates[i] = Dates.isDate(symbol.getText(),
									Dates.GENERAL_TIMEZONE);
							i++;
						}
					}
				}
			} else {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1004, ctxText);
			}

			interval = new Interval<Date>(new DateIntervalValue(dates[0]),
					openType, new DateIntervalValue(dates[1]), closeType);
		} else if (intCtx != null) {
			final Long vals[] = new Long[2];

			// parse depending on the type
			if (intCtx instanceof SelectorIntIntervalContext) {
				final SelectorIntIntervalContext c = (SelectorIntIntervalContext) intCtx;

				vals[0] = Long.parseLong(c.INT(0).getText());
				vals[1] = Long.parseLong(c.INT(1).getText());
			} else if (intCtx instanceof SelectorIntIntervalWithNullContext) {
				final SelectorIntIntervalWithNullContext c = (SelectorIntIntervalWithNullContext) intCtx;

				int i = 0;
				for (ParseTree o : c.children) {
					if (o instanceof TerminalNode) {
						final Token symbol = ((TerminalNode) o).getSymbol();

						if (QueryGrammarParser.NULL_VALUE == symbol.getType()) {
							vals[i] = null;
							i++;
						} else if (QueryGrammarParser.INT == symbol.getType()) {
							vals[i] = Long.parseLong(symbol.getText());
							i++;
						}
					}
				}
			} else {
				throw new ForwardedRuntimeException(
						QueryParsingException.class, 1004, ctxText);
			}

			interval = new Interval<Long>(new LongIntervalValue(vals[0]),
					openType, new LongIntervalValue(vals[1]), closeType);
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1004, ctxText);
		}

		return interval;
	}
}
