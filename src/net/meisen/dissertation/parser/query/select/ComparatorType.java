package net.meisen.dissertation.parser.query.select;

import net.meisen.dissertation.parser.query.generated.QueryGrammarParser;

import org.antlr.v4.runtime.ParserRuleContext;

public enum ComparatorType {
	EQUAL("=");

	private final String symbol;

	private ComparatorType(final String symbol) {
		this.symbol = symbol;
	}

	public static ComparatorType resolve(final ParserRuleContext ctx) {

		if (ctx.getToken(QueryGrammarParser.CMP_EQUAL, 0) != null) {
			return ComparatorType.EQUAL;
		} else {
			// TODO throw exception
			throw new IllegalStateException("Invalid ResultType");
		}
	}

	@Override
	public String toString() {
		return symbol;
	}
}
