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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorIntIdList}.
	 * @param ctx the parse tree
	 */
	void enterSelectorIntIdList(@NotNull QueryGrammarParser.SelectorIntIdListContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorIntIdList}.
	 * @param ctx the parse tree
	 */
	void exitSelectorIntIdList(@NotNull QueryGrammarParser.SelectorIntIdListContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#compId}.
	 * @param ctx the parse tree
	 */
	void enterCompId(@NotNull QueryGrammarParser.CompIdContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compId}.
	 * @param ctx the parse tree
	 */
	void exitCompId(@NotNull QueryGrammarParser.CompIdContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprDelete}.
	 * @param ctx the parse tree
	 */
	void enterExprDelete(@NotNull QueryGrammarParser.ExprDeleteContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprDelete}.
	 * @param ctx the parse tree
	 */
	void exitExprDelete(@NotNull QueryGrammarParser.ExprDeleteContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprRemoveMultipleRoles}.
	 * @param ctx the parse tree
	 */
	void enterExprRemoveMultipleRoles(@NotNull QueryGrammarParser.ExprRemoveMultipleRolesContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprRemoveMultipleRoles}.
	 * @param ctx the parse tree
	 */
	void exitExprRemoveMultipleRoles(@NotNull QueryGrammarParser.ExprRemoveMultipleRolesContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#compDimMeasure}.
	 * @param ctx the parse tree
	 */
	void enterCompDimMeasure(@NotNull QueryGrammarParser.CompDimMeasureContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compDimMeasure}.
	 * @param ctx the parse tree
	 */
	void exitCompDimMeasure(@NotNull QueryGrammarParser.CompDimMeasureContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#compDimMathMeasure}.
	 * @param ctx the parse tree
	 */
	void enterCompDimMathMeasure(@NotNull QueryGrammarParser.CompDimMathMeasureContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compDimMathMeasure}.
	 * @param ctx the parse tree
	 */
	void exitCompDimMathMeasure(@NotNull QueryGrammarParser.CompDimMathMeasureContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compMathMeasure}.
	 * @param ctx the parse tree
	 */
	void enterCompMathMeasure(@NotNull QueryGrammarParser.CompMathMeasureContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compMathMeasure}.
	 * @param ctx the parse tree
	 */
	void exitCompMathMeasure(@NotNull QueryGrammarParser.CompMathMeasureContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#compNamedDimMathMeasure}.
	 * @param ctx the parse tree
	 */
	void enterCompNamedDimMathMeasure(@NotNull QueryGrammarParser.CompNamedDimMathMeasureContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compNamedDimMathMeasure}.
	 * @param ctx the parse tree
	 */
	void exitCompNamedDimMathMeasure(@NotNull QueryGrammarParser.CompNamedDimMathMeasureContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorIntValue}.
	 * @param ctx the parse tree
	 */
	void enterSelectorIntValue(@NotNull QueryGrammarParser.SelectorIntValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorIntValue}.
	 * @param ctx the parse tree
	 */
	void exitSelectorIntValue(@NotNull QueryGrammarParser.SelectorIntValueContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprGet}.
	 * @param ctx the parse tree
	 */
	void enterExprGet(@NotNull QueryGrammarParser.ExprGetContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprGet}.
	 * @param ctx the parse tree
	 */
	void exitExprGet(@NotNull QueryGrammarParser.ExprGetContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorLimit}.
	 * @param ctx the parse tree
	 */
	void enterSelectorLimit(@NotNull QueryGrammarParser.SelectorLimitContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorLimit}.
	 * @param ctx the parse tree
	 */
	void exitSelectorLimit(@NotNull QueryGrammarParser.SelectorLimitContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compDimAggrFunction}.
	 * @param ctx the parse tree
	 */
	void enterCompDimAggrFunction(@NotNull QueryGrammarParser.CompDimAggrFunctionContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compDimAggrFunction}.
	 * @param ctx the parse tree
	 */
	void exitCompDimAggrFunction(@NotNull QueryGrammarParser.CompDimAggrFunctionContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprAssignMultipleRoles}.
	 * @param ctx the parse tree
	 */
	void enterExprAssignMultipleRoles(@NotNull QueryGrammarParser.ExprAssignMultipleRolesContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprAssignMultipleRoles}.
	 * @param ctx the parse tree
	 */
	void exitExprAssignMultipleRoles(@NotNull QueryGrammarParser.ExprAssignMultipleRolesContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprUnloadSetProperty}.
	 * @param ctx the parse tree
	 */
	void enterExprUnloadSetProperty(@NotNull QueryGrammarParser.ExprUnloadSetPropertyContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprUnloadSetProperty}.
	 * @param ctx the parse tree
	 */
	void exitExprUnloadSetProperty(@NotNull QueryGrammarParser.ExprUnloadSetPropertyContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorNullValue}.
	 * @param ctx the parse tree
	 */
	void enterSelectorNullValue(@NotNull QueryGrammarParser.SelectorNullValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorNullValue}.
	 * @param ctx the parse tree
	 */
	void exitSelectorNullValue(@NotNull QueryGrammarParser.SelectorNullValueContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprWithRoles}.
	 * @param ctx the parse tree
	 */
	void enterExprWithRoles(@NotNull QueryGrammarParser.ExprWithRolesContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprWithRoles}.
	 * @param ctx the parse tree
	 */
	void exitExprWithRoles(@NotNull QueryGrammarParser.ExprWithRolesContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprWithPassword}.
	 * @param ctx the parse tree
	 */
	void enterExprWithPassword(@NotNull QueryGrammarParser.ExprWithPasswordContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprWithPassword}.
	 * @param ctx the parse tree
	 */
	void exitExprWithPassword(@NotNull QueryGrammarParser.ExprWithPasswordContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorValueList}.
	 * @param ctx the parse tree
	 */
	void enterSelectorValueList(@NotNull QueryGrammarParser.SelectorValueListContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorValueList}.
	 * @param ctx the parse tree
	 */
	void exitSelectorValueList(@NotNull QueryGrammarParser.SelectorValueListContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#compMathAggrFunction}.
	 * @param ctx the parse tree
	 */
	void enterCompMathAggrFunction(@NotNull QueryGrammarParser.CompMathAggrFunctionContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compMathAggrFunction}.
	 * @param ctx the parse tree
	 */
	void exitCompMathAggrFunction(@NotNull QueryGrammarParser.CompMathAggrFunctionContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compMemberEqual}.
	 * @param ctx the parse tree
	 */
	void enterCompMemberEqual(@NotNull QueryGrammarParser.CompMemberEqualContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compMemberEqual}.
	 * @param ctx the parse tree
	 */
	void exitCompMemberEqual(@NotNull QueryGrammarParser.CompMemberEqualContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorLowAggrFunctionName}.
	 * @param ctx the parse tree
	 */
	void enterSelectorLowAggrFunctionName(@NotNull QueryGrammarParser.SelectorLowAggrFunctionNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorLowAggrFunctionName}.
	 * @param ctx the parse tree
	 */
	void exitSelectorLowAggrFunctionName(@NotNull QueryGrammarParser.SelectorLowAggrFunctionNameContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprWithPermissions}.
	 * @param ctx the parse tree
	 */
	void enterExprWithPermissions(@NotNull QueryGrammarParser.ExprWithPermissionsContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprWithPermissions}.
	 * @param ctx the parse tree
	 */
	void exitExprWithPermissions(@NotNull QueryGrammarParser.ExprWithPermissionsContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprDrop}.
	 * @param ctx the parse tree
	 */
	void enterExprDrop(@NotNull QueryGrammarParser.ExprDropContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprDrop}.
	 * @param ctx the parse tree
	 */
	void exitExprDrop(@NotNull QueryGrammarParser.ExprDropContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprSetPassword}.
	 * @param ctx the parse tree
	 */
	void enterExprSetPassword(@NotNull QueryGrammarParser.ExprSetPasswordContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprSetPassword}.
	 * @param ctx the parse tree
	 */
	void exitExprSetPassword(@NotNull QueryGrammarParser.ExprSetPasswordContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#compDimMathMeasureAtom}.
	 * @param ctx the parse tree
	 */
	void enterCompDimMathMeasureAtom(@NotNull QueryGrammarParser.CompDimMathMeasureAtomContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compDimMathMeasureAtom}.
	 * @param ctx the parse tree
	 */
	void exitCompDimMathMeasureAtom(@NotNull QueryGrammarParser.CompDimMathMeasureAtomContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#compNamedLowMeasure}.
	 * @param ctx the parse tree
	 */
	void enterCompNamedLowMeasure(@NotNull QueryGrammarParser.CompNamedLowMeasureContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compNamedLowMeasure}.
	 * @param ctx the parse tree
	 */
	void exitCompNamedLowMeasure(@NotNull QueryGrammarParser.CompNamedLowMeasureContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprRevoke}.
	 * @param ctx the parse tree
	 */
	void enterExprRevoke(@NotNull QueryGrammarParser.ExprRevokeContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprRevoke}.
	 * @param ctx the parse tree
	 */
	void exitExprRevoke(@NotNull QueryGrammarParser.ExprRevokeContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprAssign}.
	 * @param ctx the parse tree
	 */
	void enterExprAssign(@NotNull QueryGrammarParser.ExprAssignContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprAssign}.
	 * @param ctx the parse tree
	 */
	void exitExprAssign(@NotNull QueryGrammarParser.ExprAssignContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#compGroupFilter}.
	 * @param ctx the parse tree
	 */
	void enterCompGroupFilter(@NotNull QueryGrammarParser.CompGroupFilterContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compGroupFilter}.
	 * @param ctx the parse tree
	 */
	void exitCompGroupFilter(@NotNull QueryGrammarParser.CompGroupFilterContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorValue}.
	 * @param ctx the parse tree
	 */
	void enterSelectorValue(@NotNull QueryGrammarParser.SelectorValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorValue}.
	 * @param ctx the parse tree
	 */
	void exitSelectorValue(@NotNull QueryGrammarParser.SelectorValueContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprSetBulkLoad}.
	 * @param ctx the parse tree
	 */
	void enterExprSetBulkLoad(@NotNull QueryGrammarParser.ExprSetBulkLoadContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprSetBulkLoad}.
	 * @param ctx the parse tree
	 */
	void exitExprSetBulkLoad(@NotNull QueryGrammarParser.ExprSetBulkLoadContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprGrant}.
	 * @param ctx the parse tree
	 */
	void enterExprGrant(@NotNull QueryGrammarParser.ExprGrantContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprGrant}.
	 * @param ctx the parse tree
	 */
	void exitExprGrant(@NotNull QueryGrammarParser.ExprGrantContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprRemove}.
	 * @param ctx the parse tree
	 */
	void enterExprRemove(@NotNull QueryGrammarParser.ExprRemoveContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprRemove}.
	 * @param ctx the parse tree
	 */
	void exitExprRemove(@NotNull QueryGrammarParser.ExprRemoveContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorDimAggrFunctionName}.
	 * @param ctx the parse tree
	 */
	void enterSelectorDimAggrFunctionName(@NotNull QueryGrammarParser.SelectorDimAggrFunctionNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorDimAggrFunctionName}.
	 * @param ctx the parse tree
	 */
	void exitSelectorDimAggrFunctionName(@NotNull QueryGrammarParser.SelectorDimAggrFunctionNameContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprRemoveSingleRole}.
	 * @param ctx the parse tree
	 */
	void enterExprRemoveSingleRole(@NotNull QueryGrammarParser.ExprRemoveSingleRoleContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprRemoveSingleRole}.
	 * @param ctx the parse tree
	 */
	void exitExprRemoveSingleRole(@NotNull QueryGrammarParser.ExprRemoveSingleRoleContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compGroupInclude}.
	 * @param ctx the parse tree
	 */
	void enterCompGroupInclude(@NotNull QueryGrammarParser.CompGroupIncludeContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compGroupInclude}.
	 * @param ctx the parse tree
	 */
	void exitCompGroupInclude(@NotNull QueryGrammarParser.CompGroupIncludeContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprAdd}.
	 * @param ctx the parse tree
	 */
	void enterExprAdd(@NotNull QueryGrammarParser.ExprAddContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprAdd}.
	 * @param ctx the parse tree
	 */
	void exitExprAdd(@NotNull QueryGrammarParser.ExprAddContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorOffset}.
	 * @param ctx the parse tree
	 */
	void enterSelectorOffset(@NotNull QueryGrammarParser.SelectorOffsetContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorOffset}.
	 * @param ctx the parse tree
	 */
	void exitSelectorOffset(@NotNull QueryGrammarParser.SelectorOffsetContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorMember}.
	 * @param ctx the parse tree
	 */
	void enterSelectorMember(@NotNull QueryGrammarParser.SelectorMemberContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorMember}.
	 * @param ctx the parse tree
	 */
	void exitSelectorMember(@NotNull QueryGrammarParser.SelectorMemberContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compGroupExclude}.
	 * @param ctx the parse tree
	 */
	void enterCompGroupExclude(@NotNull QueryGrammarParser.CompGroupExcludeContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compGroupExclude}.
	 * @param ctx the parse tree
	 */
	void exitCompGroupExclude(@NotNull QueryGrammarParser.CompGroupExcludeContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorDateValue}.
	 * @param ctx the parse tree
	 */
	void enterSelectorDateValue(@NotNull QueryGrammarParser.SelectorDateValueContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorDateValue}.
	 * @param ctx the parse tree
	 */
	void exitSelectorDateValue(@NotNull QueryGrammarParser.SelectorDateValueContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#compDimMeasureAtom}.
	 * @param ctx the parse tree
	 */
	void enterCompDimMeasureAtom(@NotNull QueryGrammarParser.CompDimMeasureAtomContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compDimMeasureAtom}.
	 * @param ctx the parse tree
	 */
	void exitCompDimMeasureAtom(@NotNull QueryGrammarParser.CompDimMeasureAtomContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprModify}.
	 * @param ctx the parse tree
	 */
	void enterExprModify(@NotNull QueryGrammarParser.ExprModifyContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprModify}.
	 * @param ctx the parse tree
	 */
	void exitExprModify(@NotNull QueryGrammarParser.ExprModifyContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#exprAssignSingleRole}.
	 * @param ctx the parse tree
	 */
	void enterExprAssignSingleRole(@NotNull QueryGrammarParser.ExprAssignSingleRoleContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#exprAssignSingleRole}.
	 * @param ctx the parse tree
	 */
	void exitExprAssignSingleRole(@NotNull QueryGrammarParser.ExprAssignSingleRoleContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compLowAggrFunction}.
	 * @param ctx the parse tree
	 */
	void enterCompLowAggrFunction(@NotNull QueryGrammarParser.CompLowAggrFunctionContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compLowAggrFunction}.
	 * @param ctx the parse tree
	 */
	void exitCompLowAggrFunction(@NotNull QueryGrammarParser.CompLowAggrFunctionContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#selectorMathAggrFunctionName}.
	 * @param ctx the parse tree
	 */
	void enterSelectorMathAggrFunctionName(@NotNull QueryGrammarParser.SelectorMathAggrFunctionNameContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#selectorMathAggrFunctionName}.
	 * @param ctx the parse tree
	 */
	void exitSelectorMathAggrFunctionName(@NotNull QueryGrammarParser.SelectorMathAggrFunctionNameContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compMathMeasureAtom}.
	 * @param ctx the parse tree
	 */
	void enterCompMathMeasureAtom(@NotNull QueryGrammarParser.CompMathMeasureAtomContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compMathMeasureAtom}.
	 * @param ctx the parse tree
	 */
	void exitCompMathMeasureAtom(@NotNull QueryGrammarParser.CompMathMeasureAtomContext ctx);

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
	 * Enter a parse tree produced by {@link QueryGrammarParser#compLowMeasureAtom}.
	 * @param ctx the parse tree
	 */
	void enterCompLowMeasureAtom(@NotNull QueryGrammarParser.CompLowMeasureAtomContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compLowMeasureAtom}.
	 * @param ctx the parse tree
	 */
	void exitCompLowMeasureAtom(@NotNull QueryGrammarParser.CompLowMeasureAtomContext ctx);

	/**
	 * Enter a parse tree produced by {@link QueryGrammarParser#compLowMeasure}.
	 * @param ctx the parse tree
	 */
	void enterCompLowMeasure(@NotNull QueryGrammarParser.CompLowMeasureContext ctx);
	/**
	 * Exit a parse tree produced by {@link QueryGrammarParser#compLowMeasure}.
	 * @param ctx the parse tree
	 */
	void exitCompLowMeasure(@NotNull QueryGrammarParser.CompLowMeasureContext ctx);
}