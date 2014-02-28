package net.meisen.dissertation.parser.query;

import java.util.Date;

import net.meisen.dissertation.parser.query.generated.QueryGrammarBaseListener;
import net.meisen.dissertation.parser.query.generated.QueryGrammarParser;
import net.meisen.dissertation.parser.query.generated.QueryGrammarParser.CompDescriptorEqualContext;
import net.meisen.dissertation.parser.query.generated.QueryGrammarParser.ExprCompContext;
import net.meisen.dissertation.parser.query.generated.QueryGrammarParser.ExprIntervalContext;
import net.meisen.dissertation.parser.query.generated.QueryGrammarParser.ExprSelectContext;
import net.meisen.dissertation.parser.query.generated.QueryGrammarParser.SelectorDateIntervalContext;
import net.meisen.dissertation.parser.query.generated.QueryGrammarParser.SelectorIntIntervalContext;
import net.meisen.dissertation.parser.query.generated.QueryGrammarParser.SelectorSelectTypeContext;
import net.meisen.dissertation.parser.query.select.DateIntervalValue;
import net.meisen.dissertation.parser.query.select.DescriptorComperator;
import net.meisen.dissertation.parser.query.select.Interval;
import net.meisen.dissertation.parser.query.select.IntervalType;
import net.meisen.dissertation.parser.query.select.LongIntervalValue;
import net.meisen.dissertation.parser.query.select.ResultType;
import net.meisen.dissertation.parser.query.select.SelectQuery;
import net.meisen.dissertation.parser.query.select.logical.LogicalOperator;
import net.meisen.general.genmisc.types.Strings;

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
		if (!this.finalized) {
			// TODO add exception
			throw new IllegalStateException(
					"The parsing is still in progress or already finished.");
		} else if (this.query != null) {
			// TODO add exception
			throw new IllegalStateException(
					"Cannot parse multiple selects at once.");
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
		if (optimize) {
			q(SelectQuery.class).optimize();
		}

		finalized = true;
	}

	@Override
	public void enterExprComp(final ExprCompContext ctx) {
		final LogicalOperator op = LogicalOperator.resolve(ctx);

		if (op != null) {
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
		if (op != null) {
			q(SelectQuery.class).getFilter().moveUp();
		}
	}

	@Override
	public void exitCompDescriptorEqual(final CompDescriptorEqualContext ctx) {

		final DescriptorComperator descCmp = new DescriptorComperator();

		// get the used reference
		descCmp.setId(ctx.IDENTIFIER().getText());

		// get the value the descriptor should have
		descCmp.setValue(Strings.trimSequence(ctx.DESC_VALUE().getText(), "'")
				.replace("\\", ""));

		q(SelectQuery.class).getFilter().attach(descCmp);
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
		if (dateCtx != null) {
			interval = new Interval<Date>(
					new DateIntervalValue(dateCtx.DATE(0)), openType,
					new DateIntervalValue(dateCtx.DATE(1)), closeType);
		} else if (intCtx != null) {
			interval = new Interval<Long>(new LongIntervalValue(intCtx.INT(0)),
					openType, new LongIntervalValue(intCtx.INT(1)), closeType);
		} else {
			// TODO throw exception
			throw new IllegalStateException(
					"Unable to determine the interval values");
		}

		q(SelectQuery.class).setInterval(interval);
	}

	@Override
	public void exitSelectorSelectType(final SelectorSelectTypeContext ctx) {
		q(SelectQuery.class).setResultType(ResultType.resolve(ctx));
	}

	/**
	 * Gets the {@code query} casted to the needed return type.
	 * 
	 * @return the {@code query} casted to the needed return type
	 */
	@SuppressWarnings("unchecked")
	protected <T extends IQuery> T q() {
		if (query == null) {
			// TODO throw exception
			throw new NullPointerException("No query");
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
			// TODO throw exception
			throw new IllegalStateException("The query isn't finalized yet.");
		}

		return query;
	}
}
