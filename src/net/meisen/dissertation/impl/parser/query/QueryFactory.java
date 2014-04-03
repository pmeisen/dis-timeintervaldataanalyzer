package net.meisen.dissertation.impl.parser.query;

import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarLexer;
import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser;
import net.meisen.dissertation.model.parser.query.IQuery;
import net.meisen.dissertation.model.parser.query.IQueryFactory;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

/**
 * Factory to create {@code Query} instances for the specified string.
 * 
 * @author pmeisen
 * 
 */
public class QueryFactory implements IQueryFactory {

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
	 */
	public IQuery parseQuery(final String queryString, final boolean optimize) {
		
		// create a CharStream that reads from standard input
		final ANTLRInputStream input = new ANTLRInputStream(queryString);

		// create a lexer that feeds off of input CharStream
		final QueryGrammarLexer lexer = new QueryGrammarLexer(input);

		// create a buffer of tokens pulled from the lexer
		final CommonTokenStream tokens = new CommonTokenStream(lexer);

		// create a parser that feeds off the tokens buffer
		final QueryGrammarParser parser = new QueryGrammarParser(tokens);

		// create a generator for queries
		final QueryGenerator generator = new QueryGenerator(optimize);

		// create a walker which feeds the generator later
		final ParseTreeWalker walker = new ParseTreeWalker();

		// trigger the generator by a walker (post-parsing)
		walker.walk(generator, parser.exprSelect());

		// return the created query
		return generator.getQuery();
	}

	@Override
	public IQuery parseQuery(final String queryString) {
		return parseQuery(queryString, true);
	}
}
