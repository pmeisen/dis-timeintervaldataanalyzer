package net.meisen.dissertation.parser.query;

import net.meisen.dissertation.parser.query.generated.QueryGrammarLexer;
import net.meisen.dissertation.parser.query.generated.QueryGrammarParser;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

public class QueryFactory {

	public IQuery parseQuery(final String queryString) {
		System.out.println(queryString);

		// create a CharStream that reads from standard input
		final ANTLRInputStream input = new ANTLRInputStream(queryString);

		// create a lexer that feeds off of input CharStream
		final QueryGrammarLexer lexer = new QueryGrammarLexer(input);

		// create a buffer of tokens pulled from the lexer
		final CommonTokenStream tokens = new CommonTokenStream(lexer);

		// create a parser that feeds off the tokens buffer
		final QueryGrammarParser parser = new QueryGrammarParser(tokens);

		// create a generator for queries
		final QueryGenerator generator = new QueryGenerator();

		// create a walker which feeds the generator later
		final ParseTreeWalker walker = new ParseTreeWalker();

		// trigger the generator by a walker (post-parsing)
		walker.walk(generator, parser.exprSelect());

		// return the created query
		return generator.getQuery();
	}
}
