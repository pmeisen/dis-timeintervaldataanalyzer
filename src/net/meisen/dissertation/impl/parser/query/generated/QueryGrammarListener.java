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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorDescriptorId}.
	 * @param ctx the parse tree
	 */
	void enterSelectorDescriptorId(@NotNull QueryGrammarParser.SelectorDescriptorIdContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorDescriptorId}.
	 * @param ctx the parse tree
	 */
	void exitSelectorDescriptorId(@NotNull QueryGrammarParser.SelectorDescriptorIdContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprGroup}.
	 * @param ctx the parse tree
	 */
	void enterExprGroup(@NotNull QueryGrammarParser.ExprGroupContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprGroup}.
	 * @param ctx the parse tree
	 */
	void exitExprGroup(@NotNull QueryGrammarParser.ExprGroupContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorAggrFunction}.
	 * @param ctx the parse tree
	 */
	void enterSelectorAggrFunction(@NotNull QueryGrammarParser.SelectorAggrFunctionContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorAggrFunction}.
	 * @param ctx the parse tree
	 */
	void exitSelectorAggrFunction(@NotNull QueryGrammarParser.SelectorAggrFunctionContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#compGroupIgnore}.
	 * @param ctx the parse tree
	 */
	void enterCompGroupIgnore(@NotNull QueryGrammarParser.CompGroupIgnoreContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compGroupIgnore}.
	 * @param ctx the parse tree
	 */
	void exitCompGroupIgnore(@NotNull QueryGrammarParser.CompGroupIgnoreContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorDescValue}.
	 * @param ctx the parse tree
	 */
	void enterSelectorDescValue(@NotNull QueryGrammarParser.SelectorDescValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorDescValue}.
	 * @param ctx the parse tree
	 */
	void exitSelectorDescValue(@NotNull QueryGrammarParser.SelectorDescValueContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorAggrFunctionName}.
	 * @param ctx the parse tree
	 */
	void enterSelectorAggrFunctionName(@NotNull QueryGrammarParser.SelectorAggrFunctionNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorAggrFunctionName}.
	 * @param ctx the parse tree
	 */
	void exitSelectorAggrFunctionName(@NotNull QueryGrammarParser.SelectorAggrFunctionNameContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprMeasure}.
	 * @param ctx the parse tree
	 */
	void enterExprMeasure(@NotNull QueryGrammarParser.ExprMeasureContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprMeasure}.
	 * @param ctx the parse tree
	 */
	void exitExprMeasure(@NotNull QueryGrammarParser.ExprMeasureContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorDescValueTupel}.
	 * @param ctx the parse tree
	 */
	void enterSelectorDescValueTupel(@NotNull QueryGrammarParser.SelectorDescValueTupelContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorDescValueTupel}.
	 * @param ctx the parse tree
	 */
	void exitSelectorDescValueTupel(@NotNull QueryGrammarParser.SelectorDescValueTupelContext ctx);

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
}