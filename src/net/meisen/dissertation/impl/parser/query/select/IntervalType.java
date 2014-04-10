package net.meisen.dissertation.impl.parser.query.select;

import org.antlr.v4.runtime.ParserRuleContext;

import net.meisen.dissertation.impl.parser.query.generated.QueryGrammarParser;

public enum IntervalType {
	INCLUDE("[", "]"), EXCLUDE("(", ")");

	private final String open;
	private final String close;

	private IntervalType(final String open, final String close) {
		this.open = open;
		this.close = close;
	}

	@Override
	public String toString() {
		return open;
	}

	public String toString(final boolean asOpen) {
		return asOpen ? open : close;
	}
	
	public boolean isInclusive() {
		return INCLUDE.equals(this);
	}

	/**
	 * Determine the type of the interval based on the passed context of the
	 * parser.
	 * 
	 * @param ctx
	 *            the context of the parser to be checked
	 * 
	 * @return the determined {@code IntervalType}
	 */
	public static IntervalType resolve(final ParserRuleContext ctx) {
		if (ctx.getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0) != null) {
			return EXCLUDE;
		} else if (ctx.getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0) != null) {
			return EXCLUDE;
		} else if (ctx.getToken(QueryGrammarParser.BRACKET_SQUARE_OPENED, 0) != null) {
			return INCLUDE;
		} else if (ctx.getToken(QueryGrammarParser.BRACKET_SQUARE_CLOSED, 0) != null) {
			return INCLUDE;
		} else {
			return null;
		}
	}
}
