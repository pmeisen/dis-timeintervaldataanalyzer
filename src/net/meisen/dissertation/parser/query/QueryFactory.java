package net.meisen.dissertation.parser.query;

import net.meisen.dissertation.parser.query.generated.QueryGrammarLexer;
import net.meisen.dissertation.parser.query.generated.QueryGrammarParser;
import net.meisen.dissertation.parser.query.generated.QueryGrammarParser.IntervalContext;
import net.meisen.dissertation.parser.query.generated.QueryGrammarParser.SelectContext;

import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

public class QueryFactory {

	public QueryFactory() {

	}

	public void parseQuery(final String query) {
		
		// create a CharStream that reads from standard input
		final ANTLRInputStream input = new ANTLRInputStream(query);

		// create a lexer that feeds off of input CharStream
		final QueryGrammarLexer lexer = new QueryGrammarLexer(input);

		// create a buffer of tokens pulled from the lexer
		final CommonTokenStream tokens = new CommonTokenStream(lexer);

		// create a parser that feeds off the tokens buffer
		final QueryGrammarParser parser = new QueryGrammarParser(tokens);
		
		// parse the stuff and get the result
		final SelectContext selectContext = parser.select();
		final IntervalContext intContext = selectContext.interval();

		System.out.println(selectContext.getTokens(QueryGrammarParser.TYPE_TIMELINES));
		System.out.println(intContext.getTokens(QueryGrammarParser.DATE));
		System.out.println(intContext.getTokens(QueryGrammarParser.INT));
		
		System.out.println(selectContext.toStringTree(parser));		
	}
}
