package net.meisen.dissertation.impl.parser.query;

import net.meisen.dissertation.config.xslt.DefaultValues;
import net.meisen.dissertation.exceptions.PermissionException;
import net.meisen.dissertation.exceptions.QueryEvaluationException;
import net.meisen.dissertation.exceptions.QueryParsingException;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarLexer;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser;
import net.meisen.dissertation.model.auth.IAuthManager;
import net.meisen.dissertation.model.auth.permissions.DefinedPermission;
import net.meisen.dissertation.model.data.TidaModel;
import net.meisen.dissertation.model.handler.TidaModelHandler;
import net.meisen.dissertation.model.measures.AggregationFunctionHandler;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryFactory;
import net.meisen.dissertation.model.parser.query.IQueryResult;
import net.meisen.dissertation.model.parser.query.IResourceResolver;
import net.meisen.dissertation.server.CancellationException;
import net.meisen.general.genmisc.exceptions.ForwardedRuntimeException;
import net.meisen.general.genmisc.exceptions.registry.IExceptionRegistry;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

/**
 * Factory to create {@code Query} instances for the specified string.
 * 
 * @author pmeisen
 * 
 */
public class QueryFactory implements IQueryFactory {
	private final static Logger LOG = LoggerFactory
			.getLogger(QueryFactory.class);

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
	 * The handler used to resolve aggregation-functions.
	 */
	@Autowired
	@Qualifier(DefaultValues.AUTHMANAGER_ID)
	protected IAuthManager authManager;

	@Autowired
	@Qualifier(DefaultValues.MODELHANDLER_ID)
	private TidaModelHandler handler;

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
			walker.walk(generator, parser.root());

			// return the created query
			return generator.getQuery();

		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry.throwRuntimeException(e);
			return null;
		} catch (final RuntimeException e) {
			throw e;
		} catch (final Exception e) {
			exceptionRegistry.throwRuntimeException(
					QueryParsingException.class, 1000, e, queryString);
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T extends IQuery> T parseQuery(final String queryString)
			throws QueryParsingException {
		if (LOG.isTraceEnabled()) {
			if (queryString.contains("PASSWORD")) {
				LOG.trace("Parsing a query containing password information.");
			} else {
				LOG.trace("Parsing the query '" + queryString + "'.");
			}
		}

		return (T) parseQuery(queryString, true);
	}

	@SuppressWarnings("unchecked")
	@Override
	public <T extends IQueryResult> T evaluateQuery(final IQuery query,
			final IResourceResolver resolver) throws QueryEvaluationException,
			CancellationException {
		if (LOG.isTraceEnabled()) {
			LOG.trace("Evaluating the query '" + query + "'.");
		}

		// check the permissions
		checkPermission(query);

		// get the model if needed
		final TidaModel model;
		if (query.expectsModel()) {
			final String modelId = query.getModelId();
			model = handler.getTidaModel(modelId);

			if (model == null) {
				exceptionRegistry.throwRuntimeException(
						QueryEvaluationException.class, 1001, modelId);
			}
		} else {
			model = null;
		}

		// evaluate
		try {
			return (T) query.evaluate(authManager, handler, model, resolver);
		} catch (final ForwardedRuntimeException e) {
			exceptionRegistry
					.throwRuntimeException((ForwardedRuntimeException) e);
		} catch (final CancellationException e) {
			throw e;
		} catch (final RuntimeException e) {
			if (LOG.isErrorEnabled()) {
				LOG.error("Error occurred during evaluation.", e);
			}

			throw e;
		} catch (final Exception e) {
			if (LOG.isErrorEnabled()) {
				LOG.error("Unknown error occurred during evaluation.", e);
			}

			exceptionRegistry.throwRuntimeException(
					QueryEvaluationException.class, 1000, e, query);
		}

		// unreachable code
		return null;
	}

	/**
	 * Checks if the current user has the permission to process the specified
	 * {@code query}.
	 * 
	 * @param query
	 *            the query to be checked
	 * 
	 * @throws PermissionException
	 *             if the permission is not available
	 */
	protected void checkPermission(final IQuery query)
			throws PermissionException {
		final DefinedPermission[][] permSets = query.getNeededPermissions();
		if (!DefinedPermission.checkPermission(authManager, permSets)) {
			exceptionRegistry.throwRuntimeException(PermissionException.class,
					1000, DefinedPermission.toString(permSets));
		}
	}
}
