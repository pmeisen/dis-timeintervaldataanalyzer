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
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprAlive}.
	 * @param ctx the parse tree
	 */
	void enterExprAlive(@NotNull QueryGrammarParser.ExprAliveContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprAlive}.
	 * @param ctx the parse tree
	 */
	void exitExprAlive(@NotNull QueryGrammarParser.ExprAliveContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compStructureElement}.
	 * @param ctx the parse tree
	 */
	void enterCompStructureElement(@NotNull QueryGrammarParser.CompStructureElementContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compStructureElement}.
	 * @param ctx the parse tree
	 */
	void exitCompStructureElement(@NotNull QueryGrammarParser.CompStructureElementContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#root}.
	 * @param ctx the parse tree
	 */
	void enterRoot(@NotNull QueryGrammarParser.RootContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#root}.
	 * @param ctx the parse tree
	 */
	void exitRoot(@NotNull QueryGrammarParser.RootContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprSelectTimeSeries}.
	 * @param ctx the parse tree
	 */
	void enterExprSelectTimeSeries(@NotNull QueryGrammarParser.ExprSelectTimeSeriesContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprSelectTimeSeries}.
	 * @param ctx the parse tree
	 */
	void exitExprSelectTimeSeries(@NotNull QueryGrammarParser.ExprSelectTimeSeriesContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorIntIntervalWithNull}.
	 * @param ctx the parse tree
	 */
	void enterSelectorIntIntervalWithNull(@NotNull QueryGrammarParser.SelectorIntIntervalWithNullContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorIntIntervalWithNull}.
	 * @param ctx the parse tree
	 */
	void exitSelectorIntIntervalWithNull(@NotNull QueryGrammarParser.SelectorIntIntervalWithNullContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprLoad}.
	 * @param ctx the parse tree
	 */
	void enterExprLoad(@NotNull QueryGrammarParser.ExprLoadContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprLoad}.
	 * @param ctx the parse tree
	 */
	void exitExprLoad(@NotNull QueryGrammarParser.ExprLoadContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorFilePath}.
	 * @param ctx the parse tree
	 */
	void enterSelectorFilePath(@NotNull QueryGrammarParser.SelectorFilePathContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorFilePath}.
	 * @param ctx the parse tree
	 */
	void exitSelectorFilePath(@NotNull QueryGrammarParser.SelectorFilePathContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprLoadProperty}.
	 * @param ctx the parse tree
	 */
	void enterExprLoadProperty(@NotNull QueryGrammarParser.ExprLoadPropertyContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprLoadProperty}.
	 * @param ctx the parse tree
	 */
	void exitExprLoadProperty(@NotNull QueryGrammarParser.ExprLoadPropertyContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorSecondMathOperator}.
	 * @param ctx the parse tree
	 */
	void enterSelectorSecondMathOperator(@NotNull QueryGrammarParser.SelectorSecondMathOperatorContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorSecondMathOperator}.
	 * @param ctx the parse tree
	 */
	void exitSelectorSecondMathOperator(@NotNull QueryGrammarParser.SelectorSecondMathOperatorContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorIntValueOrNull}.
	 * @param ctx the parse tree
	 */
	void enterSelectorIntValueOrNull(@NotNull QueryGrammarParser.SelectorIntValueOrNullContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorIntValueOrNull}.
	 * @param ctx the parse tree
	 */
	void exitSelectorIntValueOrNull(@NotNull QueryGrammarParser.SelectorIntValueOrNullContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorBoolean}.
	 * @param ctx the parse tree
	 */
	void enterSelectorBoolean(@NotNull QueryGrammarParser.SelectorBooleanContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorBoolean}.
	 * @param ctx the parse tree
	 */
	void exitSelectorBoolean(@NotNull QueryGrammarParser.SelectorBooleanContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#compNamedMeasure}.
	 * @param ctx the parse tree
	 */
	void enterCompNamedMeasure(@NotNull QueryGrammarParser.CompNamedMeasureContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compNamedMeasure}.
	 * @param ctx the parse tree
	 */
	void exitCompNamedMeasure(@NotNull QueryGrammarParser.CompNamedMeasureContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprUnload}.
	 * @param ctx the parse tree
	 */
	void enterExprUnload(@NotNull QueryGrammarParser.ExprUnloadContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprUnload}.
	 * @param ctx the parse tree
	 */
	void exitExprUnload(@NotNull QueryGrammarParser.ExprUnloadContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compDescriptorFormulaAtom}.
	 * @param ctx the parse tree
	 */
	void enterCompDescriptorFormulaAtom(@NotNull QueryGrammarParser.CompDescriptorFormulaAtomContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compDescriptorFormulaAtom}.
	 * @param ctx the parse tree
	 */
	void exitCompDescriptorFormulaAtom(@NotNull QueryGrammarParser.CompDescriptorFormulaAtomContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorDateValueOrNull}.
	 * @param ctx the parse tree
	 */
	void enterSelectorDateValueOrNull(@NotNull QueryGrammarParser.SelectorDateValueOrNullContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorDateValueOrNull}.
	 * @param ctx the parse tree
	 */
	void exitSelectorDateValueOrNull(@NotNull QueryGrammarParser.SelectorDateValueOrNullContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprSelectRecords}.
	 * @param ctx the parse tree
	 */
	void enterExprSelectRecords(@NotNull QueryGrammarParser.ExprSelectRecordsContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprSelectRecords}.
	 * @param ctx the parse tree
	 */
	void exitExprSelectRecords(@NotNull QueryGrammarParser.ExprSelectRecordsContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compDescValueTupel}.
	 * @param ctx the parse tree
	 */
	void enterCompDescValueTupel(@NotNull QueryGrammarParser.CompDescValueTupelContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compDescValueTupel}.
	 * @param ctx the parse tree
	 */
	void exitCompDescValueTupel(@NotNull QueryGrammarParser.CompDescValueTupelContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorIntervalDef}.
	 * @param ctx the parse tree
	 */
	void enterSelectorIntervalDef(@NotNull QueryGrammarParser.SelectorIntervalDefContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorIntervalDef}.
	 * @param ctx the parse tree
	 */
	void exitSelectorIntervalDef(@NotNull QueryGrammarParser.SelectorIntervalDefContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprInsert}.
	 * @param ctx the parse tree
	 */
	void enterExprInsert(@NotNull QueryGrammarParser.ExprInsertContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprInsert}.
	 * @param ctx the parse tree
	 */
	void exitExprInsert(@NotNull QueryGrammarParser.ExprInsertContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compValueElement}.
	 * @param ctx the parse tree
	 */
	void enterCompValueElement(@NotNull QueryGrammarParser.CompValueElementContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compValueElement}.
	 * @param ctx the parse tree
	 */
	void exitCompValueElement(@NotNull QueryGrammarParser.CompValueElementContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorAlias}.
	 * @param ctx the parse tree
	 */
	void enterSelectorAlias(@NotNull QueryGrammarParser.SelectorAliasContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorAlias}.
	 * @param ctx the parse tree
	 */
	void exitSelectorAlias(@NotNull QueryGrammarParser.SelectorAliasContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprValues}.
	 * @param ctx the parse tree
	 */
	void enterExprValues(@NotNull QueryGrammarParser.ExprValuesContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprValues}.
	 * @param ctx the parse tree
	 */
	void exitExprValues(@NotNull QueryGrammarParser.ExprValuesContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compAggrFunction}.
	 * @param ctx the parse tree
	 */
	void enterCompAggrFunction(@NotNull QueryGrammarParser.CompAggrFunctionContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compAggrFunction}.
	 * @param ctx the parse tree
	 */
	void exitCompAggrFunction(@NotNull QueryGrammarParser.CompAggrFunctionContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorFirstMathOperator}.
	 * @param ctx the parse tree
	 */
	void enterSelectorFirstMathOperator(@NotNull QueryGrammarParser.SelectorFirstMathOperatorContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorFirstMathOperator}.
	 * @param ctx the parse tree
	 */
	void exitSelectorFirstMathOperator(@NotNull QueryGrammarParser.SelectorFirstMathOperatorContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compMeasureAtom}.
	 * @param ctx the parse tree
	 */
	void enterCompMeasureAtom(@NotNull QueryGrammarParser.CompMeasureAtomContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compMeasureAtom}.
	 * @param ctx the parse tree
	 */
	void exitCompMeasureAtom(@NotNull QueryGrammarParser.CompMeasureAtomContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorDateIntervalWithNull}.
	 * @param ctx the parse tree
	 */
	void enterSelectorDateIntervalWithNull(@NotNull QueryGrammarParser.SelectorDateIntervalWithNullContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorDateIntervalWithNull}.
	 * @param ctx the parse tree
	 */
	void exitSelectorDateIntervalWithNull(@NotNull QueryGrammarParser.SelectorDateIntervalWithNullContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprStructure}.
	 * @param ctx the parse tree
	 */
	void enterExprStructure(@NotNull QueryGrammarParser.ExprStructureContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprStructure}.
	 * @param ctx the parse tree
	 */
	void exitExprStructure(@NotNull QueryGrammarParser.ExprStructureContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compDescriptorFormula}.
	 * @param ctx the parse tree
	 */
	void enterCompDescriptorFormula(@NotNull QueryGrammarParser.CompDescriptorFormulaContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compDescriptorFormula}.
	 * @param ctx the parse tree
	 */
	void exitCompDescriptorFormula(@NotNull QueryGrammarParser.CompDescriptorFormulaContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorIntervalRelation}.
	 * @param ctx the parse tree
	 */
	void enterSelectorIntervalRelation(@NotNull QueryGrammarParser.SelectorIntervalRelationContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorIntervalRelation}.
	 * @param ctx the parse tree
	 */
	void exitSelectorIntervalRelation(@NotNull QueryGrammarParser.SelectorIntervalRelationContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprLoadSetProperty}.
	 * @param ctx the parse tree
	 */
	void enterExprLoadSetProperty(@NotNull QueryGrammarParser.ExprLoadSetPropertyContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprLoadSetProperty}.
	 * @param ctx the parse tree
	 */
	void exitExprLoadSetProperty(@NotNull QueryGrammarParser.ExprLoadSetPropertyContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compMeasure}.
	 * @param ctx the parse tree
	 */
	void enterCompMeasure(@NotNull QueryGrammarParser.CompMeasureContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compMeasure}.
	 * @param ctx the parse tree
	 */
	void exitCompMeasure(@NotNull QueryGrammarParser.CompMeasureContext ctx);
}