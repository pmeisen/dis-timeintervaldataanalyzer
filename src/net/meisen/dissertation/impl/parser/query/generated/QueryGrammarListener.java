// Generated from Y:\dis-timeintervaldataanalyzer\src\net\meisen\dissertation\impl\parser\query\generated\QueryGrammar.g4 by ANTLR 4.1

package net.meisen.dissertation.impl.parser.query.generated;

import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link QueryGrammarParser}.
 */
public interface QueryGrammarListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorIntInterval}.
	 * @param ctx the parse tree
	 */
	void enterSelectorIntInterval(@NotNull QueryGrammarParser.SelectorIntIntervalContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorIntInterval}.
	 * @param ctx the parse tree
	 */
	void exitSelectorIntInterval(@NotNull QueryGrammarParser.SelectorIntIntervalContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprInterval}.
	 * @param ctx the parse tree
	 */
	void enterExprInterval(@NotNull QueryGrammarParser.ExprIntervalContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprInterval}.
	 * @param ctx the parse tree
	 */
	void exitExprInterval(@NotNull QueryGrammarParser.ExprIntervalContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprComp}.
	 * @param ctx the parse tree
	 */
	void enterExprComp(@NotNull QueryGrammarParser.ExprCompContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprComp}.
	 * @param ctx the parse tree
	 */
	void exitExprComp(@NotNull QueryGrammarParser.ExprCompContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorSelectType}.
	 * @param ctx the parse tree
	 */
	void enterSelectorSelectType(@NotNull QueryGrammarParser.SelectorSelectTypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorSelectType}.
	 * @param ctx the parse tree
	 */
	void exitSelectorSelectType(@NotNull QueryGrammarParser.SelectorSelectTypeContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compDescriptorEqual}.
	 * @param ctx the parse tree
	 */
	void enterCompDescriptorEqual(@NotNull QueryGrammarParser.CompDescriptorEqualContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compDescriptorEqual}.
	 * @param ctx the parse tree
	 */
	void exitCompDescriptorEqual(@NotNull QueryGrammarParser.CompDescriptorEqualContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprLogic}.
	 * @param ctx the parse tree
	 */
	void enterExprLogic(@NotNull QueryGrammarParser.ExprLogicContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprLogic}.
	 * @param ctx the parse tree
	 */
	void exitExprLogic(@NotNull QueryGrammarParser.ExprLogicContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorCloseInterval}.
	 * @param ctx the parse tree
	 */
	void enterSelectorCloseInterval(@NotNull QueryGrammarParser.SelectorCloseIntervalContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorCloseInterval}.
	 * @param ctx the parse tree
	 */
	void exitSelectorCloseInterval(@NotNull QueryGrammarParser.SelectorCloseIntervalContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorOpenInterval}.
	 * @param ctx the parse tree
	 */
	void enterSelectorOpenInterval(@NotNull QueryGrammarParser.SelectorOpenIntervalContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorOpenInterval}.
	 * @param ctx the parse tree
	 */
	void exitSelectorOpenInterval(@NotNull QueryGrammarParser.SelectorOpenIntervalContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprAggregate}.
	 * @param ctx the parse tree
	 */
	void enterExprAggregate(@NotNull QueryGrammarParser.ExprAggregateContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprAggregate}.
	 * @param ctx the parse tree
	 */
	void exitExprAggregate(@NotNull QueryGrammarParser.ExprAggregateContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorDateInterval}.
	 * @param ctx the parse tree
	 */
	void enterSelectorDateInterval(@NotNull QueryGrammarParser.SelectorDateIntervalContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorDateInterval}.
	 * @param ctx the parse tree
	 */
	void exitSelectorDateInterval(@NotNull QueryGrammarParser.SelectorDateIntervalContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorModelId}.
	 * @param ctx the parse tree
	 */
	void enterSelectorModelId(@NotNull QueryGrammarParser.SelectorModelIdContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorModelId}.
	 * @param ctx the parse tree
	 */
	void exitSelectorModelId(@NotNull QueryGrammarParser.SelectorModelIdContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprSelect}.
	 * @param ctx the parse tree
	 */
	void enterExprSelect(@NotNull QueryGrammarParser.ExprSelectContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprSelect}.
	 * @param ctx the parse tree
	 */
	void exitExprSelect(@NotNull QueryGrammarParser.ExprSelectContext ctx);
}