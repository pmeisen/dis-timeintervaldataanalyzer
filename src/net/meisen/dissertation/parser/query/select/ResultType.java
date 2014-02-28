package net.meisen.dissertation.parser.query.select;

import net.meisen.dissertation.parser.query.generated.QueryGrammarParser;

import org.antlr.v4.runtime.ParserRuleContext;

public enum ResultType {
	TIMESERIES, RECORDS;

	public static ResultType resolve(final ParserRuleContext ctx) {

		if (ctx.getToken(QueryGrammarParser.TYPE_RECORDS, 0) != null) {
			return ResultType.RECORDS;
		} else if (ctx.getToken(QueryGrammarParser.TYPE_TIMESERIES, 0) != null) {
			return ResultType.TIMESERIES;
		} else {
			// TODO throw exception
			throw new IllegalStateException("Invalid ResultType");
		}
	}
}
