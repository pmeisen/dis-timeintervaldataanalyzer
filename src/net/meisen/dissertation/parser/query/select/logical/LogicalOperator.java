package net.meisen.dissertation.parser.query.select.logical;

import net.meisen.dissertation.parser.query.generated.QueryGrammarParser;

import org.antlr.v4.runtime.ParserRuleContext;

public enum LogicalOperator {
	AND, OR, NOT;

	public static LogicalOperator resolve(final ParserRuleContext ctx) {
		if (ctx.getToken(QueryGrammarParser.LOGICAL_AND, 0) != null) {
			return AND;
		} else if (ctx.getToken(QueryGrammarParser.LOGICAL_OR, 0) != null) {
			return OR;
		} else if (ctx.getToken(QueryGrammarParser.LOGICAL_NOT, 0) != null) {
			return NOT;
		} else {
			return null;
		}
	}
}
