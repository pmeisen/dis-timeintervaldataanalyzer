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
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDateIntervalContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDescValueContext;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser.SelectorDescValueTupelContext;
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
import net.meisen.dissertation.impl.parser.query.select.logical.GroupExpression;
import net.meisen.dissertation.impl.parser.query.select.logical.LogicalOperator;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.types.Strings;

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
	private final boolean optimize;

	private IQuery query;
	private boolean finalized = false;

	/**
	 * Generates a {@code QueryGenerator} which will trigger optimization after
	 * complete generation.
	 */
	public QueryGenerator() {
		this(true);
	}

	/**
	 * Generates a {@code QueryGenerator} which will optimize (i.e.
	 * {@code optimize} is {@code true}) or not optimize (i.e. {@code optimize}
	 * is {@code false}) the created query.
	 * 
	 * @param optimize
	 *            {@code true} if the created query should be optimized,
	 *            otherwise {@code false}
	 */
	public QueryGenerator(final boolean optimize) {
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
		final LogicalOperator op = LogicalOperator.resolve(ctx);

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
		final LogicalOperator op = LogicalOperator.resolve(ctx);
		if (op == null) {
			// do nothing
		} else {
			q(SelectQuery.class).getFilter().moveUp();
		}
	}

	@Override
	public void exitCompDescriptorEqual(final CompDescriptorEqualContext ctx) {
		final String id = ctx.IDENTIFIER().getText();
		final String value = getDescValue(ctx.selectorDescValue());

		final DescriptorComperator descCmp = new DescriptorComperator(id, value);
		q(SelectQuery.class).getFilter().attach(descCmp);
	}

	@Override
	public void exitExprGroup(final ExprGroupContext ctx) {

		// validate the created group
		if (!q(SelectQuery.class).getGroup().isValid()) {
			// TODO throw exception;
			throw new IllegalArgumentException("INVALID GROUP");
		}
	}

	@Override
	public void exitExprInterval(final ExprIntervalContext ctx) {

		// determine the types of the interval
		final IntervalType openType = IntervalType.resolve(ctx
				.selectorOpenInterval());
		final IntervalType closeType = IntervalType.resolve(ctx
				.selectorCloseInterval());

		// determine the values
		final SelectorDateIntervalContext dateCtx = ctx.selectorDateInterval();
		final SelectorIntIntervalContext intCtx = ctx.selectorIntInterval();

		// create the interval
		final Interval<?> interval;
		if (openType == null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1002, ctx.getText());
		} else if (closeType == null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1002, ctx.getText());
		} else if (dateCtx != null) {
			interval = new Interval<Date>(
					new DateIntervalValue(dateCtx.DATE(0)), openType,
					new DateIntervalValue(dateCtx.DATE(1)), closeType);
		} else if (intCtx != null) {
			interval = new Interval<Long>(new LongIntervalValue(intCtx.INT(0)),
					openType, new LongIntervalValue(intCtx.INT(1)), closeType);
		} else {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1004, ctx.getText());
		}

		q(SelectQuery.class).setInterval(interval);
	}

	@Override
	public void exitSelectorSelectType(final SelectorSelectTypeContext ctx) {
		final ResultType type = ResultType.resolve(ctx);

		if (type == null) {
			throw new ForwardedRuntimeException(QueryParsingException.class,
					1005, ctx.getText());
		} else {
			q(SelectQuery.class).setResultType(type);
		}
	}

	@Override
	public void exitSelectorModelId(final SelectorModelIdContext ctx) {
		q(SelectQuery.class).setModelId(ctx.IDENTIFIER().getText());
	}

	@Override
	public void enterExprAggregate(
			final QueryGrammarParser.ExprAggregateContext ctx) {
	}

	@Override
	public void exitExprAggregate(final ExprAggregateContext ctx) {
		final List<String> identifiers = new ArrayList<String>();

		// get all the defined identifiers
		for (final TerminalNode identifier : ctx.IDENTIFIER()) {
			identifiers.add(identifier.getText());
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
}
