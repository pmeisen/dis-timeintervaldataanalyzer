package net.meisen.dissertation.impl.parser.query;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.exceptions.QueryParsingException;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarLexer;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.measures.AggregationFunctionHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryFactory;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Factory to create {@code Query} instances for the specified string.
 * 
 * @author pmeisen
 * 
 */
public class QueryFactory implements IQueryFactory {

	/**
	 * {@code exceptionRegistry} used to fire exceptions.
	 */
	@Autowired
	@Qualifier(DefaultValues.EXCEPTIONREGISTRY_ID)
	protected IExceptionRegistry exceptionRegistry;

	/**
	 * The handler used to resolve aggregation-functions.
	 */
	@Autowired
	@Qualifier(DefaultValues.AGGREGATIONFUNCTIONHANDLER_ID)
	protected AggregationFunctionHandler aggFuncHandler;

	/**
	 * Parses the query using the specified optimization setting.
	 * 
	 * @param queryString
	 *            the string to be parsed
	 * @param optimize
	 *            {@code true} if the query should be optimized, otherwise
	 *            {@code false}
	 * 
	 * @return the parsed query
	 * 
	 * @throws QueryParsingException
	 *             if the parsing fails
	 */
	public IQuery parseQuery(final String queryString, final boolean optimize)
			throws QueryParsingException {

		try {
			// create a CharStream that reads from standard input
			final ANTLRInputStream input = new ANTLRInputStream(queryString);

			// create a lexer that feeds off of input CharStream
			final QueryGrammarLexer lexer = new QueryGrammarLexer(input);

			// create a buffer of tokens pulled from the lexer
			final CommonTokenStream tokens = new CommonTokenStream(lexer);

			// create a generator for queries
			final QueryGenerator generator = new QueryGenerator(aggFuncHandler,
					optimize);

			// create a walker which feeds the generator later
			final ParseTreeWalker walker = new ParseTreeWalker();

			// create a parser that feeds off the tokens buffer
			final QueryGrammarParser parser = new QueryGrammarParser(tokens);
			parser.removeErrorListeners();
			parser.addErrorListener(new BaseErrorListener() {

				@Override
				public void syntaxError(final Recognizer<?, ?> recognizer,
						final Object offendingSymbol, final int line,
						final int charPositionInLine, final String msg,
						final RecognitionException e) {
					throw new ForwardedRuntimeException(
							QueryParsingException.class, 1003, e, line,
							charPositionInLine, queryString, msg);
				}
			});

			// trigger the generator by a walker (post-parsing)
			walker.walk(generator, parser.exprSelect());

			// return the created query
			return generator.getQuery();

		} catch (final Exception e) {
			if (e instanceof ForwardedRuntimeException) {
				exceptionRegistry
						.throwRuntimeException((ForwardedRuntimeException) e);
			} else {
				exceptionRegistry.throwRuntimeException(
						QueryParsingException.class, 1000, e, queryString);
			}

			// unreachable code
			return null;
		}
	}

	@Override
	public IQuery parseQuery(final String queryString)
			throws QueryParsingException {
		return parseQuery(queryString, true);
	}

	@Override
	public IQueryResult evaluateQuery(final IQuery query,
			final TidaModelHandler handler) throws QueryEvaluationException {
		final String modelId = query.getModelId();

		// get the model
		final TidaModel model = handler.getTidaModel(modelId);
		if (model == null) {
			exceptionRegistry.throwRuntimeException(
					QueryEvaluationException.class, 1001, modelId);
		}

		try {
			return query.evaluate(model);
		} catch (final Exception e) {
			if (e instanceof ForwardedRuntimeException) {
				exceptionRegistry
						.throwRuntimeException((ForwardedRuntimeException) e);
			} else {
				exceptionRegistry.throwRuntimeException(
						QueryEvaluationException.class, 1000, e, query);
			}

			// unreachable code
			return null;
		}
	}
}
