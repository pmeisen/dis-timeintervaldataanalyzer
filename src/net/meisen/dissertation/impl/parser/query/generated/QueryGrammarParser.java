// Generated from Y:\dis-timeintervaldataanalyzer\src\net\meisen\dissertation\impl\parser\query\generated\QueryGrammar.g4 by ANTLR 4.1

package net.meisen.dissertation.impl.parser.query.generated;

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;
import java.util.Iterator;
import java.util.ArrayList;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class QueryGrammarParser extends Parser {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		MARKED_ID=1, VALUE=2, NULL_VALUE=3, POS_START_INCL=4, POS_END_INCL=5, 
		POS_START_EXCL=6, POS_END_EXCL=7, FIELD_ID=8, STMT_GET=9, STMT_SELECT=10, 
		STMT_INSERT=11, STMT_DELETE=12, STMT_OPEN=13, STMT_LOAD=14, STMT_UNLOAD=15, 
		STMT_ALIVE=16, STMT_ADD=17, STMT_DROP=18, STMT_MODIFY=19, STMT_GRANT=20, 
		STMT_REVOKE=21, STMT_ASSIGN=22, STMT_REMOVE=23, PROP_AUTOLOAD=24, PROP_FORCE=25, 
		PROP_PASSWORD=26, PROP_BULKLOAD=27, TYPE_TIMESERIES=28, TYPE_RECORDS=29, 
		TYPE_MODELS=30, TYPE_MODEL=31, TYPE_VERSION=32, TYPE_PERMISSIONS=33, TYPE_ROLES=34, 
		TYPE_USERS=35, TYPE_PERMISSION=36, TYPE_ROLE=37, TYPE_USER=38, OP_FROM=39, 
		OP_OF=40, OP_ON=41, OP_TO=42, OP_IN=43, OP_INTO=44, OP_SET=45, OP_VALUES=46, 
		OP_ALIAS=47, OP_GROUPBY=48, OP_FILTERBY=49, OP_LIMIT=50, OP_TRANSPOSE=51, 
		OP_IDONLY=52, OP_WITH=53, IR_EQUALTO=54, IR_BEFORE=55, IR_AFTER=56, IR_MEETING=57, 
		IR_OVERLAPPING=58, IR_DURING=59, IR_WITHIN=60, IR_CONTAINING=61, IR_STARTINGWITH=62, 
		IR_FINISHINGWITH=63, LOGICAL_OR=64, LOGICAL_AND=65, LOGICAL_NOT=66, LOGICAL_INCLUDE=67, 
		LOGICAL_EXCLUDE=68, LOGICAL_TRUE=69, LOGICAL_FALSE=70, MATH_MULTIPLY=71, 
		MATH_DIVISION=72, MATH_PLUS=73, MATH_MINUS=74, AGGR_COUNTSTARTED=75, AGGR_COUNTFINISHED=76, 
		AGGR_COUNT=77, AGGR_SUM=78, AGGR_MIN=79, AGGR_MAX=80, AGGR_AVERAGE=81, 
		AGGR_MODE=82, AGGR_MEAN=83, AGGR_MEDIAN=84, CMP_EQUAL=85, BRACKET_ROUND_OPENED=86, 
		BRACKET_ROUND_CLOSED=87, BRACKET_SQUARE_OPENED=88, BRACKET_SQUARE_CLOSED=89, 
		BRACKET_CURLY_OPENED=90, BRACKET_CURLY_CLOSED=91, DATE=92, INT=93, SEPARATOR=94, 
		DIMSEPARATOR=95, SIMPLE_ID=96, ENHANCED_ID=97, WHITESPACE=98;
	public static final String[] tokenNames = {
		"<INVALID>", "MARKED_ID", "VALUE", "NULL_VALUE", "POS_START_INCL", "POS_END_INCL", 
		"POS_START_EXCL", "POS_END_EXCL", "FIELD_ID", "STMT_GET", "STMT_SELECT", 
		"STMT_INSERT", "STMT_DELETE", "STMT_OPEN", "STMT_LOAD", "STMT_UNLOAD", 
		"STMT_ALIVE", "STMT_ADD", "STMT_DROP", "STMT_MODIFY", "STMT_GRANT", "STMT_REVOKE", 
		"STMT_ASSIGN", "STMT_REMOVE", "PROP_AUTOLOAD", "PROP_FORCE", "PROP_PASSWORD", 
		"PROP_BULKLOAD", "TYPE_TIMESERIES", "TYPE_RECORDS", "TYPE_MODELS", "TYPE_MODEL", 
		"TYPE_VERSION", "TYPE_PERMISSIONS", "TYPE_ROLES", "TYPE_USERS", "TYPE_PERMISSION", 
		"TYPE_ROLE", "TYPE_USER", "OP_FROM", "OP_OF", "OP_ON", "OP_TO", "OP_IN", 
		"OP_INTO", "OP_SET", "OP_VALUES", "OP_ALIAS", "OP_GROUPBY", "OP_FILTERBY", 
		"OP_LIMIT", "OP_TRANSPOSE", "OP_IDONLY", "OP_WITH", "IR_EQUALTO", "IR_BEFORE", 
		"IR_AFTER", "IR_MEETING", "IR_OVERLAPPING", "IR_DURING", "IR_WITHIN", 
		"IR_CONTAINING", "IR_STARTINGWITH", "IR_FINISHINGWITH", "LOGICAL_OR", 
		"LOGICAL_AND", "LOGICAL_NOT", "LOGICAL_INCLUDE", "LOGICAL_EXCLUDE", "LOGICAL_TRUE", 
		"LOGICAL_FALSE", "'*'", "'/'", "'+'", "'-'", "AGGR_COUNTSTARTED", "AGGR_COUNTFINISHED", 
		"AGGR_COUNT", "AGGR_SUM", "AGGR_MIN", "AGGR_MAX", "AGGR_AVERAGE", "AGGR_MODE", 
		"AGGR_MEAN", "AGGR_MEDIAN", "'='", "'('", "')'", "'['", "']'", "'{'", 
		"'}'", "DATE", "INT", "','", "'.'", "SIMPLE_ID", "ENHANCED_ID", "WHITESPACE"
	};
	public static final int
		RULE_root = 0, RULE_exprAdd = 1, RULE_exprWithPassword = 2, RULE_exprWithPermissions = 3, 
		RULE_exprWithRoles = 4, RULE_exprDrop = 5, RULE_exprModify = 6, RULE_exprSetPassword = 7, 
		RULE_exprSetBulkLoad = 8, RULE_exprGrant = 9, RULE_exprRevoke = 10, RULE_exprAssign = 11, 
		RULE_exprAssignSingleRole = 12, RULE_exprAssignMultipleRoles = 13, RULE_exprRemove = 14, 
		RULE_exprRemoveSingleRole = 15, RULE_exprRemoveMultipleRoles = 16, RULE_exprGet = 17, 
		RULE_exprAlive = 18, RULE_exprLoad = 19, RULE_exprLoadSetProperty = 20, 
		RULE_exprUnload = 21, RULE_exprUnloadSetProperty = 22, RULE_exprInsert = 23, 
		RULE_exprStructure = 24, RULE_exprValues = 25, RULE_exprDelete = 26, RULE_exprSelect = 27, 
		RULE_exprSelectRecords = 28, RULE_exprSelectTimeSeries = 29, RULE_exprMeasure = 30, 
		RULE_exprInterval = 31, RULE_exprComp = 32, RULE_exprGroup = 33, RULE_exprAggregate = 34, 
		RULE_compNamedLowMeasure = 35, RULE_compNamedDimMathMeasure = 36, RULE_compMemberEqual = 37, 
		RULE_compDescriptorEqual = 38, RULE_compDescValueTupel = 39, RULE_compGroupInclude = 40, 
		RULE_compGroupExclude = 41, RULE_compGroupFilter = 42, RULE_compStructureElement = 43, 
		RULE_compValueElement = 44, RULE_compId = 45, RULE_compDimAggrFunction = 46, 
		RULE_compLowAggrFunction = 47, RULE_compMathAggrFunction = 48, RULE_compLowMeasure = 49, 
		RULE_compLowMeasureAtom = 50, RULE_compMathMeasure = 51, RULE_compMathMeasureAtom = 52, 
		RULE_compDimMeasure = 53, RULE_compDimMeasureAtom = 54, RULE_compDimMathMeasure = 55, 
		RULE_compDimMathMeasureAtom = 56, RULE_compDescriptorFormula = 57, RULE_compDescriptorFormulaAtom = 58, 
		RULE_selectorMember = 59, RULE_selectorModelId = 60, RULE_selectorDescriptorId = 61, 
		RULE_selectorAlias = 62, RULE_selectorDateInterval = 63, RULE_selectorOffset = 64, 
		RULE_selectorLimit = 65, RULE_selectorIntInterval = 66, RULE_selectorIntIdList = 67, 
		RULE_selectorDateIntervalWithNull = 68, RULE_selectorIntIntervalWithNull = 69, 
		RULE_selectorDateValue = 70, RULE_selectorIntValue = 71, RULE_selectorNullValue = 72, 
		RULE_selectorValue = 73, RULE_selectorOpenInterval = 74, RULE_selectorCloseInterval = 75, 
		RULE_selectorMathAggrFunctionName = 76, RULE_selectorDimAggrFunctionName = 77, 
		RULE_selectorLowAggrFunctionName = 78, RULE_selectorFirstMathOperator = 79, 
		RULE_selectorSecondMathOperator = 80, RULE_selectorIntervalDef = 81, RULE_selectorBoolean = 82, 
		RULE_selectorIntervalRelation = 83, RULE_selectorValueList = 84;
	public static final String[] ruleNames = {
		"root", "exprAdd", "exprWithPassword", "exprWithPermissions", "exprWithRoles", 
		"exprDrop", "exprModify", "exprSetPassword", "exprSetBulkLoad", "exprGrant", 
		"exprRevoke", "exprAssign", "exprAssignSingleRole", "exprAssignMultipleRoles", 
		"exprRemove", "exprRemoveSingleRole", "exprRemoveMultipleRoles", "exprGet", 
		"exprAlive", "exprLoad", "exprLoadSetProperty", "exprUnload", "exprUnloadSetProperty", 
		"exprInsert", "exprStructure", "exprValues", "exprDelete", "exprSelect", 
		"exprSelectRecords", "exprSelectTimeSeries", "exprMeasure", "exprInterval", 
		"exprComp", "exprGroup", "exprAggregate", "compNamedLowMeasure", "compNamedDimMathMeasure", 
		"compMemberEqual", "compDescriptorEqual", "compDescValueTupel", "compGroupInclude", 
		"compGroupExclude", "compGroupFilter", "compStructureElement", "compValueElement", 
		"compId", "compDimAggrFunction", "compLowAggrFunction", "compMathAggrFunction", 
		"compLowMeasure", "compLowMeasureAtom", "compMathMeasure", "compMathMeasureAtom", 
		"compDimMeasure", "compDimMeasureAtom", "compDimMathMeasure", "compDimMathMeasureAtom", 
		"compDescriptorFormula", "compDescriptorFormulaAtom", "selectorMember", 
		"selectorModelId", "selectorDescriptorId", "selectorAlias", "selectorDateInterval", 
		"selectorOffset", "selectorLimit", "selectorIntInterval", "selectorIntIdList", 
		"selectorDateIntervalWithNull", "selectorIntIntervalWithNull", "selectorDateValue", 
		"selectorIntValue", "selectorNullValue", "selectorValue", "selectorOpenInterval", 
		"selectorCloseInterval", "selectorMathAggrFunctionName", "selectorDimAggrFunctionName", 
		"selectorLowAggrFunctionName", "selectorFirstMathOperator", "selectorSecondMathOperator", 
		"selectorIntervalDef", "selectorBoolean", "selectorIntervalRelation", 
		"selectorValueList"
	};

	@Override
	public String getGrammarFileName() { return "QueryGrammar.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public ATN getATN() { return _ATN; }

	public QueryGrammarParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class RootContext extends ParserRuleContext {
		public ExprAliveContext exprAlive() {
			return getRuleContext(ExprAliveContext.class,0);
		}
		public ExprUnloadContext exprUnload() {
			return getRuleContext(ExprUnloadContext.class,0);
		}
		public ExprGrantContext exprGrant() {
			return getRuleContext(ExprGrantContext.class,0);
		}
		public ExprRevokeContext exprRevoke() {
			return getRuleContext(ExprRevokeContext.class,0);
		}
		public ExprAssignContext exprAssign() {
			return getRuleContext(ExprAssignContext.class,0);
		}
		public ExprInsertContext exprInsert() {
			return getRuleContext(ExprInsertContext.class,0);
		}
		public ExprDeleteContext exprDelete() {
			return getRuleContext(ExprDeleteContext.class,0);
		}
		public ExprSelectContext exprSelect() {
			return getRuleContext(ExprSelectContext.class,0);
		}
		public ExprDropContext exprDrop() {
			return getRuleContext(ExprDropContext.class,0);
		}
		public TerminalNode EOF() { return getToken(QueryGrammarParser.EOF, 0); }
		public ExprGetContext exprGet() {
			return getRuleContext(ExprGetContext.class,0);
		}
		public ExprModifyContext exprModify() {
			return getRuleContext(ExprModifyContext.class,0);
		}
		public ExprAddContext exprAdd() {
			return getRuleContext(ExprAddContext.class,0);
		}
		public ExprRemoveContext exprRemove() {
			return getRuleContext(ExprRemoveContext.class,0);
		}
		public ExprLoadContext exprLoad() {
			return getRuleContext(ExprLoadContext.class,0);
		}
		public RootContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_root; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterRoot(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitRoot(this);
		}
	}

	public final RootContext root() throws RecognitionException {
		RootContext _localctx = new RootContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_root);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(184);
			switch (_input.LA(1)) {
			case STMT_INSERT:
				{
				setState(170); exprInsert();
				}
				break;
			case STMT_DELETE:
				{
				setState(171); exprDelete();
				}
				break;
			case STMT_SELECT:
				{
				setState(172); exprSelect();
				}
				break;
			case STMT_OPEN:
			case STMT_LOAD:
				{
				setState(173); exprLoad();
				}
				break;
			case STMT_UNLOAD:
				{
				setState(174); exprUnload();
				}
				break;
			case STMT_ALIVE:
				{
				setState(175); exprAlive();
				}
				break;
			case STMT_GET:
				{
				setState(176); exprGet();
				}
				break;
			case STMT_ADD:
				{
				setState(177); exprAdd();
				}
				break;
			case STMT_DROP:
				{
				setState(178); exprDrop();
				}
				break;
			case STMT_MODIFY:
				{
				setState(179); exprModify();
				}
				break;
			case STMT_GRANT:
				{
				setState(180); exprGrant();
				}
				break;
			case STMT_REVOKE:
				{
				setState(181); exprRevoke();
				}
				break;
			case STMT_ASSIGN:
				{
				setState(182); exprAssign();
				}
				break;
			case STMT_REMOVE:
				{
				setState(183); exprRemove();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(186); match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprAddContext extends ParserRuleContext {
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public TerminalNode TYPE_ROLE() { return getToken(QueryGrammarParser.TYPE_ROLE, 0); }
		public TerminalNode STMT_ADD() { return getToken(QueryGrammarParser.STMT_ADD, 0); }
		public ExprWithPermissionsContext exprWithPermissions() {
			return getRuleContext(ExprWithPermissionsContext.class,0);
		}
		public ExprWithRolesContext exprWithRoles() {
			return getRuleContext(ExprWithRolesContext.class,0);
		}
		public TerminalNode TYPE_USER() { return getToken(QueryGrammarParser.TYPE_USER, 0); }
		public ExprWithPasswordContext exprWithPassword() {
			return getRuleContext(ExprWithPasswordContext.class,0);
		}
		public ExprAddContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprAdd; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprAdd(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprAdd(this);
		}
	}

	public final ExprAddContext exprAdd() throws RecognitionException {
		ExprAddContext _localctx = new ExprAddContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_exprAdd);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(188); match(STMT_ADD);
			setState(209);
			switch (_input.LA(1)) {
			case TYPE_USER:
				{
				setState(189); match(TYPE_USER);
				setState(190); match(VALUE);
				setState(191); exprWithPassword();
				setState(202);
				_la = _input.LA(1);
				if (_la==OP_WITH) {
					{
					setState(200);
					switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
					case 1:
						{
						setState(192); exprWithRoles();
						setState(194);
						_la = _input.LA(1);
						if (_la==OP_WITH) {
							{
							setState(193); exprWithPermissions();
							}
						}

						}
						break;

					case 2:
						{
						setState(196); exprWithPermissions();
						setState(198);
						_la = _input.LA(1);
						if (_la==OP_WITH) {
							{
							setState(197); exprWithRoles();
							}
						}

						}
						break;
					}
					}
				}

				}
				break;
			case TYPE_ROLE:
				{
				{
				setState(204); match(TYPE_ROLE);
				setState(205); match(VALUE);
				setState(207);
				_la = _input.LA(1);
				if (_la==OP_WITH) {
					{
					setState(206); exprWithPermissions();
					}
				}

				}
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprWithPasswordContext extends ParserRuleContext {
		public TerminalNode OP_WITH() { return getToken(QueryGrammarParser.OP_WITH, 0); }
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public TerminalNode PROP_PASSWORD() { return getToken(QueryGrammarParser.PROP_PASSWORD, 0); }
		public ExprWithPasswordContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprWithPassword; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprWithPassword(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprWithPassword(this);
		}
	}

	public final ExprWithPasswordContext exprWithPassword() throws RecognitionException {
		ExprWithPasswordContext _localctx = new ExprWithPasswordContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_exprWithPassword);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(211); match(OP_WITH);
			setState(212); match(PROP_PASSWORD);
			setState(213); match(VALUE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprWithPermissionsContext extends ParserRuleContext {
		public SelectorValueListContext selectorValueList() {
			return getRuleContext(SelectorValueListContext.class,0);
		}
		public TerminalNode OP_WITH() { return getToken(QueryGrammarParser.OP_WITH, 0); }
		public TerminalNode TYPE_PERMISSIONS() { return getToken(QueryGrammarParser.TYPE_PERMISSIONS, 0); }
		public ExprWithPermissionsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprWithPermissions; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprWithPermissions(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprWithPermissions(this);
		}
	}

	public final ExprWithPermissionsContext exprWithPermissions() throws RecognitionException {
		ExprWithPermissionsContext _localctx = new ExprWithPermissionsContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_exprWithPermissions);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(215); match(OP_WITH);
			setState(216); match(TYPE_PERMISSIONS);
			setState(217); selectorValueList();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprWithRolesContext extends ParserRuleContext {
		public SelectorValueListContext selectorValueList() {
			return getRuleContext(SelectorValueListContext.class,0);
		}
		public TerminalNode OP_WITH() { return getToken(QueryGrammarParser.OP_WITH, 0); }
		public TerminalNode TYPE_ROLES() { return getToken(QueryGrammarParser.TYPE_ROLES, 0); }
		public ExprWithRolesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprWithRoles; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprWithRoles(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprWithRoles(this);
		}
	}

	public final ExprWithRolesContext exprWithRoles() throws RecognitionException {
		ExprWithRolesContext _localctx = new ExprWithRolesContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_exprWithRoles);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(219); match(OP_WITH);
			setState(220); match(TYPE_ROLES);
			setState(221); selectorValueList();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprDropContext extends ParserRuleContext {
		public SelectorModelIdContext selectorModelId() {
			return getRuleContext(SelectorModelIdContext.class,0);
		}
		public TerminalNode TYPE_MODEL() { return getToken(QueryGrammarParser.TYPE_MODEL, 0); }
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public TerminalNode STMT_DROP() { return getToken(QueryGrammarParser.STMT_DROP, 0); }
		public TerminalNode TYPE_ROLE() { return getToken(QueryGrammarParser.TYPE_ROLE, 0); }
		public TerminalNode TYPE_USER() { return getToken(QueryGrammarParser.TYPE_USER, 0); }
		public ExprDropContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprDrop; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprDrop(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprDrop(this);
		}
	}

	public final ExprDropContext exprDrop() throws RecognitionException {
		ExprDropContext _localctx = new ExprDropContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_exprDrop);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(223); match(STMT_DROP);
			setState(228);
			switch (_input.LA(1)) {
			case TYPE_ROLE:
			case TYPE_USER:
				{
				{
				setState(224);
				_la = _input.LA(1);
				if ( !(_la==TYPE_ROLE || _la==TYPE_USER) ) {
				_errHandler.recoverInline(this);
				}
				consume();
				setState(225); match(VALUE);
				}
				}
				break;
			case TYPE_MODEL:
				{
				setState(226); match(TYPE_MODEL);
				setState(227); selectorModelId();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprModifyContext extends ParserRuleContext {
		public ExprSetBulkLoadContext exprSetBulkLoad() {
			return getRuleContext(ExprSetBulkLoadContext.class,0);
		}
		public SelectorModelIdContext selectorModelId() {
			return getRuleContext(SelectorModelIdContext.class,0);
		}
		public TerminalNode TYPE_MODEL() { return getToken(QueryGrammarParser.TYPE_MODEL, 0); }
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public TerminalNode STMT_MODIFY() { return getToken(QueryGrammarParser.STMT_MODIFY, 0); }
		public ExprSetPasswordContext exprSetPassword() {
			return getRuleContext(ExprSetPasswordContext.class,0);
		}
		public TerminalNode TYPE_USER() { return getToken(QueryGrammarParser.TYPE_USER, 0); }
		public ExprModifyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprModify; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprModify(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprModify(this);
		}
	}

	public final ExprModifyContext exprModify() throws RecognitionException {
		ExprModifyContext _localctx = new ExprModifyContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_exprModify);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(230); match(STMT_MODIFY);
			setState(238);
			switch (_input.LA(1)) {
			case TYPE_USER:
				{
				{
				setState(231); match(TYPE_USER);
				setState(232); match(VALUE);
				setState(233); exprSetPassword();
				}
				}
				break;
			case TYPE_MODEL:
				{
				{
				setState(234); match(TYPE_MODEL);
				setState(235); selectorModelId();
				setState(236); exprSetBulkLoad();
				}
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprSetPasswordContext extends ParserRuleContext {
		public TerminalNode OP_SET() { return getToken(QueryGrammarParser.OP_SET, 0); }
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public TerminalNode PROP_PASSWORD() { return getToken(QueryGrammarParser.PROP_PASSWORD, 0); }
		public TerminalNode CMP_EQUAL() { return getToken(QueryGrammarParser.CMP_EQUAL, 0); }
		public ExprSetPasswordContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprSetPassword; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprSetPassword(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprSetPassword(this);
		}
	}

	public final ExprSetPasswordContext exprSetPassword() throws RecognitionException {
		ExprSetPasswordContext _localctx = new ExprSetPasswordContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_exprSetPassword);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(240); match(OP_SET);
			setState(241); match(PROP_PASSWORD);
			setState(242); match(CMP_EQUAL);
			setState(243); match(VALUE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprSetBulkLoadContext extends ParserRuleContext {
		public TerminalNode OP_SET() { return getToken(QueryGrammarParser.OP_SET, 0); }
		public TerminalNode PROP_BULKLOAD() { return getToken(QueryGrammarParser.PROP_BULKLOAD, 0); }
		public SelectorBooleanContext selectorBoolean() {
			return getRuleContext(SelectorBooleanContext.class,0);
		}
		public TerminalNode CMP_EQUAL() { return getToken(QueryGrammarParser.CMP_EQUAL, 0); }
		public ExprSetBulkLoadContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprSetBulkLoad; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprSetBulkLoad(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprSetBulkLoad(this);
		}
	}

	public final ExprSetBulkLoadContext exprSetBulkLoad() throws RecognitionException {
		ExprSetBulkLoadContext _localctx = new ExprSetBulkLoadContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_exprSetBulkLoad);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(245); match(OP_SET);
			setState(246); match(PROP_BULKLOAD);
			setState(247); match(CMP_EQUAL);
			setState(248); selectorBoolean();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprGrantContext extends ParserRuleContext {
		public TerminalNode STMT_GRANT() { return getToken(QueryGrammarParser.STMT_GRANT, 0); }
		public TerminalNode OP_TO() { return getToken(QueryGrammarParser.OP_TO, 0); }
		public SelectorValueListContext selectorValueList() {
			return getRuleContext(SelectorValueListContext.class,0);
		}
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public TerminalNode TYPE_ROLE() { return getToken(QueryGrammarParser.TYPE_ROLE, 0); }
		public TerminalNode TYPE_PERMISSIONS() { return getToken(QueryGrammarParser.TYPE_PERMISSIONS, 0); }
		public TerminalNode TYPE_USER() { return getToken(QueryGrammarParser.TYPE_USER, 0); }
		public ExprGrantContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprGrant; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprGrant(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprGrant(this);
		}
	}

	public final ExprGrantContext exprGrant() throws RecognitionException {
		ExprGrantContext _localctx = new ExprGrantContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_exprGrant);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(250); match(STMT_GRANT);
			setState(251); match(TYPE_PERMISSIONS);
			setState(252); selectorValueList();
			setState(253); match(OP_TO);
			setState(254);
			_la = _input.LA(1);
			if ( !(_la==TYPE_ROLE || _la==TYPE_USER) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(255); match(VALUE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprRevokeContext extends ParserRuleContext {
		public SelectorValueListContext selectorValueList() {
			return getRuleContext(SelectorValueListContext.class,0);
		}
		public TerminalNode OP_FROM() { return getToken(QueryGrammarParser.OP_FROM, 0); }
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public TerminalNode TYPE_ROLE() { return getToken(QueryGrammarParser.TYPE_ROLE, 0); }
		public TerminalNode STMT_REVOKE() { return getToken(QueryGrammarParser.STMT_REVOKE, 0); }
		public TerminalNode TYPE_PERMISSIONS() { return getToken(QueryGrammarParser.TYPE_PERMISSIONS, 0); }
		public TerminalNode TYPE_USER() { return getToken(QueryGrammarParser.TYPE_USER, 0); }
		public ExprRevokeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprRevoke; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprRevoke(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprRevoke(this);
		}
	}

	public final ExprRevokeContext exprRevoke() throws RecognitionException {
		ExprRevokeContext _localctx = new ExprRevokeContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_exprRevoke);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(257); match(STMT_REVOKE);
			setState(258); match(TYPE_PERMISSIONS);
			setState(259); selectorValueList();
			setState(260); match(OP_FROM);
			setState(261);
			_la = _input.LA(1);
			if ( !(_la==TYPE_ROLE || _la==TYPE_USER) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(262); match(VALUE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprAssignContext extends ParserRuleContext {
		public TerminalNode OP_TO() { return getToken(QueryGrammarParser.OP_TO, 0); }
		public TerminalNode STMT_ASSIGN() { return getToken(QueryGrammarParser.STMT_ASSIGN, 0); }
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public ExprAssignSingleRoleContext exprAssignSingleRole() {
			return getRuleContext(ExprAssignSingleRoleContext.class,0);
		}
		public ExprAssignMultipleRolesContext exprAssignMultipleRoles() {
			return getRuleContext(ExprAssignMultipleRolesContext.class,0);
		}
		public TerminalNode TYPE_USER() { return getToken(QueryGrammarParser.TYPE_USER, 0); }
		public ExprAssignContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprAssign; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprAssign(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprAssign(this);
		}
	}

	public final ExprAssignContext exprAssign() throws RecognitionException {
		ExprAssignContext _localctx = new ExprAssignContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_exprAssign);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(264); match(STMT_ASSIGN);
			setState(267);
			switch (_input.LA(1)) {
			case TYPE_ROLE:
				{
				setState(265); exprAssignSingleRole();
				}
				break;
			case TYPE_ROLES:
				{
				setState(266); exprAssignMultipleRoles();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(269); match(OP_TO);
			setState(270); match(TYPE_USER);
			setState(271); match(VALUE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprAssignSingleRoleContext extends ParserRuleContext {
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public TerminalNode TYPE_ROLE() { return getToken(QueryGrammarParser.TYPE_ROLE, 0); }
		public ExprAssignSingleRoleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprAssignSingleRole; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprAssignSingleRole(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprAssignSingleRole(this);
		}
	}

	public final ExprAssignSingleRoleContext exprAssignSingleRole() throws RecognitionException {
		ExprAssignSingleRoleContext _localctx = new ExprAssignSingleRoleContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_exprAssignSingleRole);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(273); match(TYPE_ROLE);
			setState(274); match(VALUE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprAssignMultipleRolesContext extends ParserRuleContext {
		public SelectorValueListContext selectorValueList() {
			return getRuleContext(SelectorValueListContext.class,0);
		}
		public TerminalNode TYPE_ROLES() { return getToken(QueryGrammarParser.TYPE_ROLES, 0); }
		public ExprAssignMultipleRolesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprAssignMultipleRoles; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprAssignMultipleRoles(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprAssignMultipleRoles(this);
		}
	}

	public final ExprAssignMultipleRolesContext exprAssignMultipleRoles() throws RecognitionException {
		ExprAssignMultipleRolesContext _localctx = new ExprAssignMultipleRolesContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_exprAssignMultipleRoles);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(276); match(TYPE_ROLES);
			setState(277); selectorValueList();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprRemoveContext extends ParserRuleContext {
		public TerminalNode OP_FROM() { return getToken(QueryGrammarParser.OP_FROM, 0); }
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public ExprRemoveSingleRoleContext exprRemoveSingleRole() {
			return getRuleContext(ExprRemoveSingleRoleContext.class,0);
		}
		public TerminalNode STMT_REMOVE() { return getToken(QueryGrammarParser.STMT_REMOVE, 0); }
		public ExprRemoveMultipleRolesContext exprRemoveMultipleRoles() {
			return getRuleContext(ExprRemoveMultipleRolesContext.class,0);
		}
		public TerminalNode TYPE_USER() { return getToken(QueryGrammarParser.TYPE_USER, 0); }
		public ExprRemoveContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprRemove; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprRemove(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprRemove(this);
		}
	}

	public final ExprRemoveContext exprRemove() throws RecognitionException {
		ExprRemoveContext _localctx = new ExprRemoveContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_exprRemove);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(279); match(STMT_REMOVE);
			setState(282);
			switch (_input.LA(1)) {
			case TYPE_ROLE:
				{
				setState(280); exprRemoveSingleRole();
				}
				break;
			case TYPE_ROLES:
				{
				setState(281); exprRemoveMultipleRoles();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(284); match(OP_FROM);
			setState(285); match(TYPE_USER);
			setState(286); match(VALUE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprRemoveSingleRoleContext extends ParserRuleContext {
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public TerminalNode TYPE_ROLE() { return getToken(QueryGrammarParser.TYPE_ROLE, 0); }
		public ExprRemoveSingleRoleContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprRemoveSingleRole; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprRemoveSingleRole(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprRemoveSingleRole(this);
		}
	}

	public final ExprRemoveSingleRoleContext exprRemoveSingleRole() throws RecognitionException {
		ExprRemoveSingleRoleContext _localctx = new ExprRemoveSingleRoleContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_exprRemoveSingleRole);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(288); match(TYPE_ROLE);
			setState(289); match(VALUE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprRemoveMultipleRolesContext extends ParserRuleContext {
		public SelectorValueListContext selectorValueList() {
			return getRuleContext(SelectorValueListContext.class,0);
		}
		public TerminalNode TYPE_ROLES() { return getToken(QueryGrammarParser.TYPE_ROLES, 0); }
		public ExprRemoveMultipleRolesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprRemoveMultipleRoles; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprRemoveMultipleRoles(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprRemoveMultipleRoles(this);
		}
	}

	public final ExprRemoveMultipleRolesContext exprRemoveMultipleRoles() throws RecognitionException {
		ExprRemoveMultipleRolesContext _localctx = new ExprRemoveMultipleRolesContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_exprRemoveMultipleRoles);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(291); match(TYPE_ROLES);
			setState(292); selectorValueList();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprGetContext extends ParserRuleContext {
		public TerminalNode TYPE_USERS() { return getToken(QueryGrammarParser.TYPE_USERS, 0); }
		public TerminalNode TYPE_MODELS() { return getToken(QueryGrammarParser.TYPE_MODELS, 0); }
		public TerminalNode TYPE_VERSION() { return getToken(QueryGrammarParser.TYPE_VERSION, 0); }
		public TerminalNode STMT_GET() { return getToken(QueryGrammarParser.STMT_GET, 0); }
		public TerminalNode TYPE_PERMISSIONS() { return getToken(QueryGrammarParser.TYPE_PERMISSIONS, 0); }
		public TerminalNode TYPE_ROLES() { return getToken(QueryGrammarParser.TYPE_ROLES, 0); }
		public ExprGetContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprGet; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprGet(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprGet(this);
		}
	}

	public final ExprGetContext exprGet() throws RecognitionException {
		ExprGetContext _localctx = new ExprGetContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_exprGet);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(294); match(STMT_GET);
			setState(295);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << TYPE_MODELS) | (1L << TYPE_VERSION) | (1L << TYPE_PERMISSIONS) | (1L << TYPE_ROLES) | (1L << TYPE_USERS))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprAliveContext extends ParserRuleContext {
		public TerminalNode STMT_ALIVE() { return getToken(QueryGrammarParser.STMT_ALIVE, 0); }
		public ExprAliveContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprAlive; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprAlive(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprAlive(this);
		}
	}

	public final ExprAliveContext exprAlive() throws RecognitionException {
		ExprAliveContext _localctx = new ExprAliveContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_exprAlive);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(297); match(STMT_ALIVE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprLoadContext extends ParserRuleContext {
		public SelectorModelIdContext selectorModelId() {
			return getRuleContext(SelectorModelIdContext.class,0);
		}
		public TerminalNode OP_SET() { return getToken(QueryGrammarParser.OP_SET, 0); }
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public TerminalNode OP_FROM() { return getToken(QueryGrammarParser.OP_FROM, 0); }
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public TerminalNode STMT_LOAD() { return getToken(QueryGrammarParser.STMT_LOAD, 0); }
		public List<ExprLoadSetPropertyContext> exprLoadSetProperty() {
			return getRuleContexts(ExprLoadSetPropertyContext.class);
		}
		public TerminalNode STMT_OPEN() { return getToken(QueryGrammarParser.STMT_OPEN, 0); }
		public ExprLoadSetPropertyContext exprLoadSetProperty(int i) {
			return getRuleContext(ExprLoadSetPropertyContext.class,i);
		}
		public ExprLoadContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprLoad; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprLoad(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprLoad(this);
		}
	}

	public final ExprLoadContext exprLoad() throws RecognitionException {
		ExprLoadContext _localctx = new ExprLoadContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_exprLoad);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(299);
			_la = _input.LA(1);
			if ( !(_la==STMT_OPEN || _la==STMT_LOAD) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(303);
			switch (_input.LA(1)) {
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				{
				setState(300); selectorModelId();
				}
				break;
			case OP_FROM:
				{
				{
				setState(301); match(OP_FROM);
				setState(302); match(VALUE);
				}
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(314);
			_la = _input.LA(1);
			if (_la==OP_SET) {
				{
				setState(305); match(OP_SET);
				setState(306); exprLoadSetProperty();
				setState(311);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(307); match(SEPARATOR);
					setState(308); exprLoadSetProperty();
					}
					}
					setState(313);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprLoadSetPropertyContext extends ParserRuleContext {
		public TerminalNode PROP_AUTOLOAD() { return getToken(QueryGrammarParser.PROP_AUTOLOAD, 0); }
		public TerminalNode PROP_FORCE() { return getToken(QueryGrammarParser.PROP_FORCE, 0); }
		public SelectorBooleanContext selectorBoolean() {
			return getRuleContext(SelectorBooleanContext.class,0);
		}
		public TerminalNode CMP_EQUAL() { return getToken(QueryGrammarParser.CMP_EQUAL, 0); }
		public ExprLoadSetPropertyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprLoadSetProperty; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprLoadSetProperty(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprLoadSetProperty(this);
		}
	}

	public final ExprLoadSetPropertyContext exprLoadSetProperty() throws RecognitionException {
		ExprLoadSetPropertyContext _localctx = new ExprLoadSetPropertyContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_exprLoadSetProperty);
		try {
			setState(322);
			switch (_input.LA(1)) {
			case PROP_AUTOLOAD:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(316); match(PROP_AUTOLOAD);
				setState(317); match(CMP_EQUAL);
				setState(318); selectorBoolean();
				}
				}
				break;
			case PROP_FORCE:
				enterOuterAlt(_localctx, 2);
				{
				{
				setState(319); match(PROP_FORCE);
				setState(320); match(CMP_EQUAL);
				setState(321); selectorBoolean();
				}
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprUnloadContext extends ParserRuleContext {
		public SelectorModelIdContext selectorModelId() {
			return getRuleContext(SelectorModelIdContext.class,0);
		}
		public TerminalNode OP_SET() { return getToken(QueryGrammarParser.OP_SET, 0); }
		public TerminalNode STMT_UNLOAD() { return getToken(QueryGrammarParser.STMT_UNLOAD, 0); }
		public ExprUnloadSetPropertyContext exprUnloadSetProperty(int i) {
			return getRuleContext(ExprUnloadSetPropertyContext.class,i);
		}
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public List<ExprUnloadSetPropertyContext> exprUnloadSetProperty() {
			return getRuleContexts(ExprUnloadSetPropertyContext.class);
		}
		public ExprUnloadContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprUnload; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprUnload(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprUnload(this);
		}
	}

	public final ExprUnloadContext exprUnload() throws RecognitionException {
		ExprUnloadContext _localctx = new ExprUnloadContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_exprUnload);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(324); match(STMT_UNLOAD);
			setState(325); selectorModelId();
			setState(335);
			_la = _input.LA(1);
			if (_la==OP_SET) {
				{
				setState(326); match(OP_SET);
				setState(327); exprUnloadSetProperty();
				setState(332);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(328); match(SEPARATOR);
					setState(329); exprUnloadSetProperty();
					}
					}
					setState(334);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprUnloadSetPropertyContext extends ParserRuleContext {
		public TerminalNode PROP_AUTOLOAD() { return getToken(QueryGrammarParser.PROP_AUTOLOAD, 0); }
		public SelectorBooleanContext selectorBoolean() {
			return getRuleContext(SelectorBooleanContext.class,0);
		}
		public TerminalNode CMP_EQUAL() { return getToken(QueryGrammarParser.CMP_EQUAL, 0); }
		public ExprUnloadSetPropertyContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprUnloadSetProperty; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprUnloadSetProperty(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprUnloadSetProperty(this);
		}
	}

	public final ExprUnloadSetPropertyContext exprUnloadSetProperty() throws RecognitionException {
		ExprUnloadSetPropertyContext _localctx = new ExprUnloadSetPropertyContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_exprUnloadSetProperty);
		try {
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(337); match(PROP_AUTOLOAD);
			setState(338); match(CMP_EQUAL);
			setState(339); selectorBoolean();
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprInsertContext extends ParserRuleContext {
		public TerminalNode OP_VALUES() { return getToken(QueryGrammarParser.OP_VALUES, 0); }
		public SelectorModelIdContext selectorModelId() {
			return getRuleContext(SelectorModelIdContext.class,0);
		}
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public ExprValuesContext exprValues(int i) {
			return getRuleContext(ExprValuesContext.class,i);
		}
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public TerminalNode OP_INTO() { return getToken(QueryGrammarParser.OP_INTO, 0); }
		public List<ExprValuesContext> exprValues() {
			return getRuleContexts(ExprValuesContext.class);
		}
		public TerminalNode STMT_INSERT() { return getToken(QueryGrammarParser.STMT_INSERT, 0); }
		public ExprStructureContext exprStructure() {
			return getRuleContext(ExprStructureContext.class,0);
		}
		public ExprInsertContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprInsert; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprInsert(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprInsert(this);
		}
	}

	public final ExprInsertContext exprInsert() throws RecognitionException {
		ExprInsertContext _localctx = new ExprInsertContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_exprInsert);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(341); match(STMT_INSERT);
			setState(342); match(OP_INTO);
			setState(343); selectorModelId();
			setState(344); exprStructure();
			setState(345); match(OP_VALUES);
			setState(346); exprValues();
			setState(351);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(347); match(SEPARATOR);
				setState(348); exprValues();
				}
				}
				setState(353);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprStructureContext extends ParserRuleContext {
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public CompStructureElementContext compStructureElement(int i) {
			return getRuleContext(CompStructureElementContext.class,i);
		}
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public List<CompStructureElementContext> compStructureElement() {
			return getRuleContexts(CompStructureElementContext.class);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public ExprStructureContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprStructure; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprStructure(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprStructure(this);
		}
	}

	public final ExprStructureContext exprStructure() throws RecognitionException {
		ExprStructureContext _localctx = new ExprStructureContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_exprStructure);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(354); match(BRACKET_ROUND_OPENED);
			setState(355); compStructureElement();
			setState(360);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(356); match(SEPARATOR);
				setState(357); compStructureElement();
				}
				}
				setState(362);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(363); match(BRACKET_ROUND_CLOSED);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprValuesContext extends ParserRuleContext {
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public CompValueElementContext compValueElement(int i) {
			return getRuleContext(CompValueElementContext.class,i);
		}
		public List<CompValueElementContext> compValueElement() {
			return getRuleContexts(CompValueElementContext.class);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public ExprValuesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprValues; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprValues(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprValues(this);
		}
	}

	public final ExprValuesContext exprValues() throws RecognitionException {
		ExprValuesContext _localctx = new ExprValuesContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_exprValues);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(365); match(BRACKET_ROUND_OPENED);
			setState(366); compValueElement();
			setState(371);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(367); match(SEPARATOR);
				setState(368); compValueElement();
				}
				}
				setState(373);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(374); match(BRACKET_ROUND_CLOSED);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprDeleteContext extends ParserRuleContext {
		public SelectorModelIdContext selectorModelId() {
			return getRuleContext(SelectorModelIdContext.class,0);
		}
		public TerminalNode OP_FROM() { return getToken(QueryGrammarParser.OP_FROM, 0); }
		public TerminalNode STMT_DELETE() { return getToken(QueryGrammarParser.STMT_DELETE, 0); }
		public SelectorIntIdListContext selectorIntIdList() {
			return getRuleContext(SelectorIntIdListContext.class,0);
		}
		public ExprDeleteContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprDelete; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprDelete(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprDelete(this);
		}
	}

	public final ExprDeleteContext exprDelete() throws RecognitionException {
		ExprDeleteContext _localctx = new ExprDeleteContext(_ctx, getState());
		enterRule(_localctx, 52, RULE_exprDelete);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(376); match(STMT_DELETE);
			setState(377); selectorIntIdList();
			setState(378); match(OP_FROM);
			setState(379); selectorModelId();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprSelectContext extends ParserRuleContext {
		public ExprSelectRecordsContext exprSelectRecords() {
			return getRuleContext(ExprSelectRecordsContext.class,0);
		}
		public ExprSelectTimeSeriesContext exprSelectTimeSeries() {
			return getRuleContext(ExprSelectTimeSeriesContext.class,0);
		}
		public ExprSelectContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprSelect; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprSelect(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprSelect(this);
		}
	}

	public final ExprSelectContext exprSelect() throws RecognitionException {
		ExprSelectContext _localctx = new ExprSelectContext(_ctx, getState());
		enterRule(_localctx, 54, RULE_exprSelect);
		try {
			setState(383);
			switch ( getInterpreter().adaptivePredict(_input,20,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(381); exprSelectRecords();
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(382); exprSelectTimeSeries();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprSelectRecordsContext extends ParserRuleContext {
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public SelectorModelIdContext selectorModelId() {
			return getRuleContext(SelectorModelIdContext.class,0);
		}
		public ExprCompContext exprComp() {
			return getRuleContext(ExprCompContext.class,0);
		}
		public TerminalNode OP_FROM() { return getToken(QueryGrammarParser.OP_FROM, 0); }
		public SelectorIntervalRelationContext selectorIntervalRelation() {
			return getRuleContext(SelectorIntervalRelationContext.class,0);
		}
		public ExprIntervalContext exprInterval() {
			return getRuleContext(ExprIntervalContext.class,0);
		}
		public CompIdContext compId() {
			return getRuleContext(CompIdContext.class,0);
		}
		public TerminalNode SEPARATOR() { return getToken(QueryGrammarParser.SEPARATOR, 0); }
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public SelectorOffsetContext selectorOffset() {
			return getRuleContext(SelectorOffsetContext.class,0);
		}
		public TerminalNode AGGR_COUNT() { return getToken(QueryGrammarParser.AGGR_COUNT, 0); }
		public TerminalNode OP_IDONLY() { return getToken(QueryGrammarParser.OP_IDONLY, 0); }
		public TerminalNode STMT_SELECT() { return getToken(QueryGrammarParser.STMT_SELECT, 0); }
		public SelectorLimitContext selectorLimit() {
			return getRuleContext(SelectorLimitContext.class,0);
		}
		public TerminalNode OP_FILTERBY() { return getToken(QueryGrammarParser.OP_FILTERBY, 0); }
		public TerminalNode TYPE_RECORDS() { return getToken(QueryGrammarParser.TYPE_RECORDS, 0); }
		public TerminalNode OP_LIMIT() { return getToken(QueryGrammarParser.OP_LIMIT, 0); }
		public ExprSelectRecordsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprSelectRecords; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprSelectRecords(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprSelectRecords(this);
		}
	}

	public final ExprSelectRecordsContext exprSelectRecords() throws RecognitionException {
		ExprSelectRecordsContext _localctx = new ExprSelectRecordsContext(_ctx, getState());
		enterRule(_localctx, 56, RULE_exprSelectRecords);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(385); match(STMT_SELECT);
			setState(391);
			switch (_input.LA(1)) {
			case TYPE_RECORDS:
				{
				setState(386); match(TYPE_RECORDS);
				}
				break;
			case OP_IDONLY:
			case AGGR_COUNT:
				{
				setState(387);
				_la = _input.LA(1);
				if ( !(_la==OP_IDONLY || _la==AGGR_COUNT) ) {
				_errHandler.recoverInline(this);
				}
				consume();
				setState(388); match(BRACKET_ROUND_OPENED);
				setState(389); match(TYPE_RECORDS);
				setState(390); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(393); match(OP_FROM);
			setState(394); selectorModelId();
			setState(398);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << IR_EQUALTO) | (1L << IR_BEFORE) | (1L << IR_AFTER) | (1L << IR_MEETING) | (1L << IR_OVERLAPPING) | (1L << IR_DURING) | (1L << IR_WITHIN) | (1L << IR_CONTAINING) | (1L << IR_STARTINGWITH) | (1L << IR_FINISHINGWITH))) != 0)) {
				{
				setState(395); selectorIntervalRelation();
				setState(396); exprInterval();
				}
			}

			setState(405);
			_la = _input.LA(1);
			if (_la==OP_FILTERBY) {
				{
				setState(400); match(OP_FILTERBY);
				setState(403);
				switch (_input.LA(1)) {
				case MARKED_ID:
				case LOGICAL_NOT:
				case BRACKET_ROUND_OPENED:
				case SIMPLE_ID:
				case ENHANCED_ID:
					{
					setState(401); exprComp(0);
					}
					break;
				case FIELD_ID:
					{
					setState(402); compId();
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				}
			}

			setState(413);
			_la = _input.LA(1);
			if (_la==OP_LIMIT) {
				{
				setState(407); match(OP_LIMIT);
				setState(408); selectorOffset();
				setState(411);
				_la = _input.LA(1);
				if (_la==SEPARATOR) {
					{
					setState(409); match(SEPARATOR);
					setState(410); selectorLimit();
					}
				}

				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprSelectTimeSeriesContext extends ParserRuleContext {
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public SelectorModelIdContext selectorModelId() {
			return getRuleContext(SelectorModelIdContext.class,0);
		}
		public ExprGroupContext exprGroup() {
			return getRuleContext(ExprGroupContext.class,0);
		}
		public ExprCompContext exprComp() {
			return getRuleContext(ExprCompContext.class,0);
		}
		public TerminalNode OP_FROM() { return getToken(QueryGrammarParser.OP_FROM, 0); }
		public ExprIntervalContext exprInterval() {
			return getRuleContext(ExprIntervalContext.class,0);
		}
		public TerminalNode OP_OF() { return getToken(QueryGrammarParser.OP_OF, 0); }
		public TerminalNode OP_IN() { return getToken(QueryGrammarParser.OP_IN, 0); }
		public TerminalNode OP_GROUPBY() { return getToken(QueryGrammarParser.OP_GROUPBY, 0); }
		public ExprMeasureContext exprMeasure() {
			return getRuleContext(ExprMeasureContext.class,0);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public TerminalNode OP_TRANSPOSE() { return getToken(QueryGrammarParser.OP_TRANSPOSE, 0); }
		public TerminalNode STMT_SELECT() { return getToken(QueryGrammarParser.STMT_SELECT, 0); }
		public TerminalNode TYPE_TIMESERIES() { return getToken(QueryGrammarParser.TYPE_TIMESERIES, 0); }
		public TerminalNode OP_FILTERBY() { return getToken(QueryGrammarParser.OP_FILTERBY, 0); }
		public ExprSelectTimeSeriesContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprSelectTimeSeries; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprSelectTimeSeries(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprSelectTimeSeries(this);
		}
	}

	public final ExprSelectTimeSeriesContext exprSelectTimeSeries() throws RecognitionException {
		ExprSelectTimeSeriesContext _localctx = new ExprSelectTimeSeriesContext(_ctx, getState());
		enterRule(_localctx, 58, RULE_exprSelectTimeSeries);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(415); match(STMT_SELECT);
			setState(421);
			switch (_input.LA(1)) {
			case TYPE_TIMESERIES:
				{
				setState(416); match(TYPE_TIMESERIES);
				}
				break;
			case OP_TRANSPOSE:
				{
				setState(417); match(OP_TRANSPOSE);
				setState(418); match(BRACKET_ROUND_OPENED);
				setState(419); match(TYPE_TIMESERIES);
				setState(420); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(425);
			_la = _input.LA(1);
			if (_la==OP_OF) {
				{
				setState(423); match(OP_OF);
				setState(424); exprMeasure();
				}
			}

			setState(427); match(OP_FROM);
			setState(428); selectorModelId();
			setState(431);
			_la = _input.LA(1);
			if (_la==OP_IN) {
				{
				setState(429); match(OP_IN);
				setState(430); exprInterval();
				}
			}

			setState(435);
			_la = _input.LA(1);
			if (_la==OP_FILTERBY) {
				{
				setState(433); match(OP_FILTERBY);
				setState(434); exprComp(0);
				}
			}

			setState(439);
			_la = _input.LA(1);
			if (_la==OP_GROUPBY) {
				{
				setState(437); match(OP_GROUPBY);
				setState(438); exprGroup();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprMeasureContext extends ParserRuleContext {
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public List<CompNamedDimMathMeasureContext> compNamedDimMathMeasure() {
			return getRuleContexts(CompNamedDimMathMeasureContext.class);
		}
		public CompNamedLowMeasureContext compNamedLowMeasure() {
			return getRuleContext(CompNamedLowMeasureContext.class,0);
		}
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public CompNamedDimMathMeasureContext compNamedDimMathMeasure(int i) {
			return getRuleContext(CompNamedDimMathMeasureContext.class,i);
		}
		public SelectorMemberContext selectorMember() {
			return getRuleContext(SelectorMemberContext.class,0);
		}
		public TerminalNode OP_ON() { return getToken(QueryGrammarParser.OP_ON, 0); }
		public ExprMeasureContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprMeasure; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprMeasure(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprMeasure(this);
		}
	}

	public final ExprMeasureContext exprMeasure() throws RecognitionException {
		ExprMeasureContext _localctx = new ExprMeasureContext(_ctx, getState());
		enterRule(_localctx, 60, RULE_exprMeasure);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(460);
			switch ( getInterpreter().adaptivePredict(_input,34,_ctx) ) {
			case 1:
				{
				setState(441); compNamedLowMeasure();
				setState(446);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(442); match(SEPARATOR);
					setState(443); compNamedLowMeasure();
					}
					}
					setState(448);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
				break;

			case 2:
				{
				{
				setState(449); compNamedDimMathMeasure();
				setState(454);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(450); match(SEPARATOR);
					setState(451); compNamedDimMathMeasure();
					}
					}
					setState(456);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(457); match(OP_ON);
				setState(458); selectorMember();
				}
				}
				break;
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprIntervalContext extends ParserRuleContext {
		public SelectorIntIntervalContext selectorIntInterval() {
			return getRuleContext(SelectorIntIntervalContext.class,0);
		}
		public SelectorDateIntervalContext selectorDateInterval() {
			return getRuleContext(SelectorDateIntervalContext.class,0);
		}
		public SelectorOpenIntervalContext selectorOpenInterval() {
			return getRuleContext(SelectorOpenIntervalContext.class,0);
		}
		public SelectorCloseIntervalContext selectorCloseInterval() {
			return getRuleContext(SelectorCloseIntervalContext.class,0);
		}
		public ExprIntervalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprInterval; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprInterval(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprInterval(this);
		}
	}

	public final ExprIntervalContext exprInterval() throws RecognitionException {
		ExprIntervalContext _localctx = new ExprIntervalContext(_ctx, getState());
		enterRule(_localctx, 62, RULE_exprInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(462); selectorOpenInterval();
			setState(465);
			switch (_input.LA(1)) {
			case DATE:
				{
				setState(463); selectorDateInterval();
				}
				break;
			case INT:
				{
				setState(464); selectorIntInterval();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(467); selectorCloseInterval();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprCompContext extends ParserRuleContext {
		public int _p;
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public ExprCompContext exprComp(int i) {
			return getRuleContext(ExprCompContext.class,i);
		}
		public TerminalNode LOGICAL_NOT() { return getToken(QueryGrammarParser.LOGICAL_NOT, 0); }
		public TerminalNode LOGICAL_AND() { return getToken(QueryGrammarParser.LOGICAL_AND, 0); }
		public List<ExprCompContext> exprComp() {
			return getRuleContexts(ExprCompContext.class);
		}
		public CompDescriptorEqualContext compDescriptorEqual() {
			return getRuleContext(CompDescriptorEqualContext.class,0);
		}
		public CompMemberEqualContext compMemberEqual() {
			return getRuleContext(CompMemberEqualContext.class,0);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public TerminalNode LOGICAL_OR() { return getToken(QueryGrammarParser.LOGICAL_OR, 0); }
		public ExprCompContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public ExprCompContext(ParserRuleContext parent, int invokingState, int _p) {
			super(parent, invokingState);
			this._p = _p;
		}
		@Override public int getRuleIndex() { return RULE_exprComp; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprComp(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprComp(this);
		}
	}

	public final ExprCompContext exprComp(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		ExprCompContext _localctx = new ExprCompContext(_ctx, _parentState, _p);
		ExprCompContext _prevctx = _localctx;
		int _startState = 64;
		enterRecursionRule(_localctx, RULE_exprComp);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(478);
			switch ( getInterpreter().adaptivePredict(_input,36,_ctx) ) {
			case 1:
				{
				setState(470); match(LOGICAL_NOT);
				setState(471); exprComp(2);
				}
				break;

			case 2:
				{
				setState(472); compMemberEqual();
				}
				break;

			case 3:
				{
				setState(473); compDescriptorEqual();
				}
				break;

			case 4:
				{
				setState(474); match(BRACKET_ROUND_OPENED);
				setState(475); exprComp(0);
				setState(476); match(BRACKET_ROUND_CLOSED);
				}
				break;
			}
			_ctx.stop = _input.LT(-1);
			setState(485);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,37,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new ExprCompContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_exprComp);
					setState(480);
					if (!(1 >= _localctx._p)) throw new FailedPredicateException(this, "1 >= $_p");
					setState(481);
					_la = _input.LA(1);
					if ( !(_la==LOGICAL_OR || _la==LOGICAL_AND) ) {
					_errHandler.recoverInline(this);
					}
					consume();
					setState(482); exprComp(2);
					}
					} 
				}
				setState(487);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,37,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class ExprGroupContext extends ParserRuleContext {
		public ExprAggregateContext exprAggregate() {
			return getRuleContext(ExprAggregateContext.class,0);
		}
		public CompGroupExcludeContext compGroupExclude() {
			return getRuleContext(CompGroupExcludeContext.class,0);
		}
		public CompGroupIncludeContext compGroupInclude() {
			return getRuleContext(CompGroupIncludeContext.class,0);
		}
		public ExprGroupContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprGroup; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprGroup(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprGroup(this);
		}
	}

	public final ExprGroupContext exprGroup() throws RecognitionException {
		ExprGroupContext _localctx = new ExprGroupContext(_ctx, getState());
		enterRule(_localctx, 66, RULE_exprGroup);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(488); exprAggregate();
			setState(490);
			_la = _input.LA(1);
			if (_la==LOGICAL_INCLUDE) {
				{
				setState(489); compGroupInclude();
				}
			}

			setState(493);
			_la = _input.LA(1);
			if (_la==LOGICAL_EXCLUDE) {
				{
				setState(492); compGroupExclude();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprAggregateContext extends ParserRuleContext {
		public SelectorDescriptorIdContext selectorDescriptorId(int i) {
			return getRuleContext(SelectorDescriptorIdContext.class,i);
		}
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public SelectorMemberContext selectorMember(int i) {
			return getRuleContext(SelectorMemberContext.class,i);
		}
		public List<SelectorMemberContext> selectorMember() {
			return getRuleContexts(SelectorMemberContext.class);
		}
		public List<SelectorDescriptorIdContext> selectorDescriptorId() {
			return getRuleContexts(SelectorDescriptorIdContext.class);
		}
		public ExprAggregateContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprAggregate; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprAggregate(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprAggregate(this);
		}
	}

	public final ExprAggregateContext exprAggregate() throws RecognitionException {
		ExprAggregateContext _localctx = new ExprAggregateContext(_ctx, getState());
		enterRule(_localctx, 68, RULE_exprAggregate);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(497);
			switch ( getInterpreter().adaptivePredict(_input,40,_ctx) ) {
			case 1:
				{
				setState(495); selectorMember();
				}
				break;

			case 2:
				{
				setState(496); selectorDescriptorId();
				}
				break;
			}
			setState(506);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(499); match(SEPARATOR);
				setState(502);
				switch ( getInterpreter().adaptivePredict(_input,41,_ctx) ) {
				case 1:
					{
					setState(500); selectorMember();
					}
					break;

				case 2:
					{
					setState(501); selectorDescriptorId();
					}
					break;
				}
				}
				}
				setState(508);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompNamedLowMeasureContext extends ParserRuleContext {
		public SelectorAliasContext selectorAlias() {
			return getRuleContext(SelectorAliasContext.class,0);
		}
		public TerminalNode OP_ALIAS() { return getToken(QueryGrammarParser.OP_ALIAS, 0); }
		public CompLowMeasureContext compLowMeasure() {
			return getRuleContext(CompLowMeasureContext.class,0);
		}
		public CompNamedLowMeasureContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compNamedLowMeasure; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompNamedLowMeasure(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompNamedLowMeasure(this);
		}
	}

	public final CompNamedLowMeasureContext compNamedLowMeasure() throws RecognitionException {
		CompNamedLowMeasureContext _localctx = new CompNamedLowMeasureContext(_ctx, getState());
		enterRule(_localctx, 70, RULE_compNamedLowMeasure);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(509); compLowMeasure(0);
			setState(512);
			_la = _input.LA(1);
			if (_la==OP_ALIAS) {
				{
				setState(510); match(OP_ALIAS);
				setState(511); selectorAlias();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompNamedDimMathMeasureContext extends ParserRuleContext {
		public CompDimMathMeasureContext compDimMathMeasure() {
			return getRuleContext(CompDimMathMeasureContext.class,0);
		}
		public SelectorAliasContext selectorAlias() {
			return getRuleContext(SelectorAliasContext.class,0);
		}
		public TerminalNode OP_ALIAS() { return getToken(QueryGrammarParser.OP_ALIAS, 0); }
		public CompNamedDimMathMeasureContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compNamedDimMathMeasure; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompNamedDimMathMeasure(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompNamedDimMathMeasure(this);
		}
	}

	public final CompNamedDimMathMeasureContext compNamedDimMathMeasure() throws RecognitionException {
		CompNamedDimMathMeasureContext _localctx = new CompNamedDimMathMeasureContext(_ctx, getState());
		enterRule(_localctx, 72, RULE_compNamedDimMathMeasure);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(514); compDimMathMeasure(0);
			setState(517);
			_la = _input.LA(1);
			if (_la==OP_ALIAS) {
				{
				setState(515); match(OP_ALIAS);
				setState(516); selectorAlias();
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompMemberEqualContext extends ParserRuleContext {
		public SelectorValueContext selectorValue() {
			return getRuleContext(SelectorValueContext.class,0);
		}
		public TerminalNode CMP_EQUAL() { return getToken(QueryGrammarParser.CMP_EQUAL, 0); }
		public SelectorMemberContext selectorMember() {
			return getRuleContext(SelectorMemberContext.class,0);
		}
		public CompMemberEqualContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compMemberEqual; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompMemberEqual(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompMemberEqual(this);
		}
	}

	public final CompMemberEqualContext compMemberEqual() throws RecognitionException {
		CompMemberEqualContext _localctx = new CompMemberEqualContext(_ctx, getState());
		enterRule(_localctx, 74, RULE_compMemberEqual);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(519); selectorMember();
			setState(520); match(CMP_EQUAL);
			setState(521); selectorValue();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompDescriptorEqualContext extends ParserRuleContext {
		public SelectorValueContext selectorValue() {
			return getRuleContext(SelectorValueContext.class,0);
		}
		public TerminalNode CMP_EQUAL() { return getToken(QueryGrammarParser.CMP_EQUAL, 0); }
		public SelectorDescriptorIdContext selectorDescriptorId() {
			return getRuleContext(SelectorDescriptorIdContext.class,0);
		}
		public CompDescriptorEqualContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compDescriptorEqual; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompDescriptorEqual(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompDescriptorEqual(this);
		}
	}

	public final CompDescriptorEqualContext compDescriptorEqual() throws RecognitionException {
		CompDescriptorEqualContext _localctx = new CompDescriptorEqualContext(_ctx, getState());
		enterRule(_localctx, 76, RULE_compDescriptorEqual);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(523); selectorDescriptorId();
			setState(524); match(CMP_EQUAL);
			setState(525); selectorValue();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompDescValueTupelContext extends ParserRuleContext {
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public List<SelectorValueContext> selectorValue() {
			return getRuleContexts(SelectorValueContext.class);
		}
		public SelectorValueContext selectorValue(int i) {
			return getRuleContext(SelectorValueContext.class,i);
		}
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public CompDescValueTupelContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compDescValueTupel; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompDescValueTupel(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompDescValueTupel(this);
		}
	}

	public final CompDescValueTupelContext compDescValueTupel() throws RecognitionException {
		CompDescValueTupelContext _localctx = new CompDescValueTupelContext(_ctx, getState());
		enterRule(_localctx, 78, RULE_compDescValueTupel);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(527); match(BRACKET_ROUND_OPENED);
			setState(528); selectorValue();
			setState(533);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(529); match(SEPARATOR);
				setState(530); selectorValue();
				}
				}
				setState(535);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(536); match(BRACKET_ROUND_CLOSED);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompGroupIncludeContext extends ParserRuleContext {
		public CompGroupFilterContext compGroupFilter() {
			return getRuleContext(CompGroupFilterContext.class,0);
		}
		public TerminalNode LOGICAL_INCLUDE() { return getToken(QueryGrammarParser.LOGICAL_INCLUDE, 0); }
		public CompGroupIncludeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compGroupInclude; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompGroupInclude(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompGroupInclude(this);
		}
	}

	public final CompGroupIncludeContext compGroupInclude() throws RecognitionException {
		CompGroupIncludeContext _localctx = new CompGroupIncludeContext(_ctx, getState());
		enterRule(_localctx, 80, RULE_compGroupInclude);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(538); match(LOGICAL_INCLUDE);
			setState(539); compGroupFilter();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompGroupExcludeContext extends ParserRuleContext {
		public CompGroupFilterContext compGroupFilter() {
			return getRuleContext(CompGroupFilterContext.class,0);
		}
		public TerminalNode LOGICAL_EXCLUDE() { return getToken(QueryGrammarParser.LOGICAL_EXCLUDE, 0); }
		public CompGroupExcludeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compGroupExclude; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompGroupExclude(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompGroupExclude(this);
		}
	}

	public final CompGroupExcludeContext compGroupExclude() throws RecognitionException {
		CompGroupExcludeContext _localctx = new CompGroupExcludeContext(_ctx, getState());
		enterRule(_localctx, 82, RULE_compGroupExclude);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(541); match(LOGICAL_EXCLUDE);
			setState(542); compGroupFilter();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompGroupFilterContext extends ParserRuleContext {
		public TerminalNode BRACKET_CURLY_OPENED() { return getToken(QueryGrammarParser.BRACKET_CURLY_OPENED, 0); }
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public CompDescValueTupelContext compDescValueTupel(int i) {
			return getRuleContext(CompDescValueTupelContext.class,i);
		}
		public TerminalNode BRACKET_CURLY_CLOSED() { return getToken(QueryGrammarParser.BRACKET_CURLY_CLOSED, 0); }
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public List<CompDescValueTupelContext> compDescValueTupel() {
			return getRuleContexts(CompDescValueTupelContext.class);
		}
		public CompGroupFilterContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compGroupFilter; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompGroupFilter(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompGroupFilter(this);
		}
	}

	public final CompGroupFilterContext compGroupFilter() throws RecognitionException {
		CompGroupFilterContext _localctx = new CompGroupFilterContext(_ctx, getState());
		enterRule(_localctx, 84, RULE_compGroupFilter);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(544); match(BRACKET_CURLY_OPENED);
			setState(545); compDescValueTupel();
			setState(550);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(546); match(SEPARATOR);
				setState(547); compDescValueTupel();
				}
				}
				setState(552);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(553); match(BRACKET_CURLY_CLOSED);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompStructureElementContext extends ParserRuleContext {
		public SelectorIntervalDefContext selectorIntervalDef() {
			return getRuleContext(SelectorIntervalDefContext.class,0);
		}
		public SelectorDescriptorIdContext selectorDescriptorId() {
			return getRuleContext(SelectorDescriptorIdContext.class,0);
		}
		public CompStructureElementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compStructureElement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompStructureElement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompStructureElement(this);
		}
	}

	public final CompStructureElementContext compStructureElement() throws RecognitionException {
		CompStructureElementContext _localctx = new CompStructureElementContext(_ctx, getState());
		enterRule(_localctx, 86, RULE_compStructureElement);
		try {
			setState(557);
			switch (_input.LA(1)) {
			case POS_START_INCL:
			case POS_END_INCL:
			case POS_START_EXCL:
			case POS_END_EXCL:
				enterOuterAlt(_localctx, 1);
				{
				setState(555); selectorIntervalDef();
				}
				break;
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				enterOuterAlt(_localctx, 2);
				{
				setState(556); selectorDescriptorId();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompValueElementContext extends ParserRuleContext {
		public SelectorValueContext selectorValue() {
			return getRuleContext(SelectorValueContext.class,0);
		}
		public SelectorDateValueContext selectorDateValue() {
			return getRuleContext(SelectorDateValueContext.class,0);
		}
		public SelectorNullValueContext selectorNullValue() {
			return getRuleContext(SelectorNullValueContext.class,0);
		}
		public SelectorIntValueContext selectorIntValue() {
			return getRuleContext(SelectorIntValueContext.class,0);
		}
		public CompValueElementContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compValueElement; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompValueElement(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompValueElement(this);
		}
	}

	public final CompValueElementContext compValueElement() throws RecognitionException {
		CompValueElementContext _localctx = new CompValueElementContext(_ctx, getState());
		enterRule(_localctx, 88, RULE_compValueElement);
		try {
			setState(563);
			switch ( getInterpreter().adaptivePredict(_input,48,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(559); selectorNullValue();
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(560); selectorDateValue();
				}
				break;

			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(561); selectorIntValue();
				}
				break;

			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(562); selectorValue();
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompIdContext extends ParserRuleContext {
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public List<TerminalNode> INT() { return getTokens(QueryGrammarParser.INT); }
		public TerminalNode FIELD_ID() { return getToken(QueryGrammarParser.FIELD_ID, 0); }
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public TerminalNode CMP_EQUAL() { return getToken(QueryGrammarParser.CMP_EQUAL, 0); }
		public TerminalNode INT(int i) {
			return getToken(QueryGrammarParser.INT, i);
		}
		public CompIdContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compId; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompId(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompId(this);
		}
	}

	public final CompIdContext compId() throws RecognitionException {
		CompIdContext _localctx = new CompIdContext(_ctx, getState());
		enterRule(_localctx, 90, RULE_compId);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(565); match(FIELD_ID);
			setState(566); match(CMP_EQUAL);
			setState(567); match(INT);
			setState(572);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(568); match(SEPARATOR);
				setState(569); match(INT);
				}
				}
				setState(574);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompDimAggrFunctionContext extends ParserRuleContext {
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public SelectorDimAggrFunctionNameContext selectorDimAggrFunctionName() {
			return getRuleContext(SelectorDimAggrFunctionNameContext.class,0);
		}
		public CompDescriptorFormulaContext compDescriptorFormula() {
			return getRuleContext(CompDescriptorFormulaContext.class,0);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public CompDimAggrFunctionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compDimAggrFunction; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompDimAggrFunction(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompDimAggrFunction(this);
		}
	}

	public final CompDimAggrFunctionContext compDimAggrFunction() throws RecognitionException {
		CompDimAggrFunctionContext _localctx = new CompDimAggrFunctionContext(_ctx, getState());
		enterRule(_localctx, 92, RULE_compDimAggrFunction);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(575); selectorDimAggrFunctionName();
			setState(576); match(BRACKET_ROUND_OPENED);
			setState(577); compDescriptorFormula(0);
			setState(578); match(BRACKET_ROUND_CLOSED);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompLowAggrFunctionContext extends ParserRuleContext {
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public SelectorLowAggrFunctionNameContext selectorLowAggrFunctionName() {
			return getRuleContext(SelectorLowAggrFunctionNameContext.class,0);
		}
		public CompDescriptorFormulaContext compDescriptorFormula() {
			return getRuleContext(CompDescriptorFormulaContext.class,0);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public CompLowAggrFunctionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compLowAggrFunction; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompLowAggrFunction(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompLowAggrFunction(this);
		}
	}

	public final CompLowAggrFunctionContext compLowAggrFunction() throws RecognitionException {
		CompLowAggrFunctionContext _localctx = new CompLowAggrFunctionContext(_ctx, getState());
		enterRule(_localctx, 94, RULE_compLowAggrFunction);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(580); selectorLowAggrFunctionName();
			setState(581); match(BRACKET_ROUND_OPENED);
			setState(582); compDescriptorFormula(0);
			setState(583); match(BRACKET_ROUND_CLOSED);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompMathAggrFunctionContext extends ParserRuleContext {
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public CompLowMeasureContext compLowMeasure() {
			return getRuleContext(CompLowMeasureContext.class,0);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public SelectorMathAggrFunctionNameContext selectorMathAggrFunctionName() {
			return getRuleContext(SelectorMathAggrFunctionNameContext.class,0);
		}
		public CompMathAggrFunctionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compMathAggrFunction; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompMathAggrFunction(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompMathAggrFunction(this);
		}
	}

	public final CompMathAggrFunctionContext compMathAggrFunction() throws RecognitionException {
		CompMathAggrFunctionContext _localctx = new CompMathAggrFunctionContext(_ctx, getState());
		enterRule(_localctx, 96, RULE_compMathAggrFunction);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(585); selectorMathAggrFunctionName();
			setState(586); match(BRACKET_ROUND_OPENED);
			setState(587); compLowMeasure(0);
			setState(588); match(BRACKET_ROUND_CLOSED);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class CompLowMeasureContext extends ParserRuleContext {
		public int _p;
		public CompLowMeasureAtomContext compLowMeasureAtom() {
			return getRuleContext(CompLowMeasureAtomContext.class,0);
		}
		public CompLowMeasureContext compLowMeasure() {
			return getRuleContext(CompLowMeasureContext.class,0);
		}
		public SelectorSecondMathOperatorContext selectorSecondMathOperator() {
			return getRuleContext(SelectorSecondMathOperatorContext.class,0);
		}
		public CompLowMeasureContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public CompLowMeasureContext(ParserRuleContext parent, int invokingState, int _p) {
			super(parent, invokingState);
			this._p = _p;
		}
		@Override public int getRuleIndex() { return RULE_compLowMeasure; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompLowMeasure(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompLowMeasure(this);
		}
	}

	public final CompLowMeasureContext compLowMeasure(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		CompLowMeasureContext _localctx = new CompLowMeasureContext(_ctx, _parentState, _p);
		CompLowMeasureContext _prevctx = _localctx;
		int _startState = 98;
		enterRecursionRule(_localctx, RULE_compLowMeasure);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(591); compLowMeasureAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(599);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,50,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompLowMeasureContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compLowMeasure);
					setState(593);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(594); selectorSecondMathOperator();
					setState(595); compLowMeasureAtom(0);
					}
					} 
				}
				setState(601);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,50,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class CompLowMeasureAtomContext extends ParserRuleContext {
		public int _p;
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public List<CompLowMeasureAtomContext> compLowMeasureAtom() {
			return getRuleContexts(CompLowMeasureAtomContext.class);
		}
		public SelectorFirstMathOperatorContext selectorFirstMathOperator() {
			return getRuleContext(SelectorFirstMathOperatorContext.class,0);
		}
		public CompLowMeasureAtomContext compLowMeasureAtom(int i) {
			return getRuleContext(CompLowMeasureAtomContext.class,i);
		}
		public CompLowMeasureContext compLowMeasure() {
			return getRuleContext(CompLowMeasureContext.class,0);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public CompLowAggrFunctionContext compLowAggrFunction() {
			return getRuleContext(CompLowAggrFunctionContext.class,0);
		}
		public CompLowMeasureAtomContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public CompLowMeasureAtomContext(ParserRuleContext parent, int invokingState, int _p) {
			super(parent, invokingState);
			this._p = _p;
		}
		@Override public int getRuleIndex() { return RULE_compLowMeasureAtom; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompLowMeasureAtom(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompLowMeasureAtom(this);
		}
	}

	public final CompLowMeasureAtomContext compLowMeasureAtom(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		CompLowMeasureAtomContext _localctx = new CompLowMeasureAtomContext(_ctx, _parentState, _p);
		CompLowMeasureAtomContext _prevctx = _localctx;
		int _startState = 100;
		enterRecursionRule(_localctx, RULE_compLowMeasureAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(608);
			switch (_input.LA(1)) {
			case AGGR_COUNTSTARTED:
			case AGGR_COUNTFINISHED:
			case AGGR_COUNT:
			case AGGR_SUM:
			case AGGR_MIN:
			case AGGR_MAX:
			case AGGR_AVERAGE:
			case AGGR_MODE:
			case AGGR_MEAN:
			case AGGR_MEDIAN:
			case SIMPLE_ID:
				{
				setState(603); compLowAggrFunction();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(604); match(BRACKET_ROUND_OPENED);
				setState(605); compLowMeasure(0);
				setState(606); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(616);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,52,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompLowMeasureAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compLowMeasureAtom);
					setState(610);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(611); selectorFirstMathOperator();
					setState(612); compLowMeasureAtom(0);
					}
					} 
				}
				setState(618);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,52,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class CompMathMeasureContext extends ParserRuleContext {
		public int _p;
		public CompMathMeasureContext compMathMeasure() {
			return getRuleContext(CompMathMeasureContext.class,0);
		}
		public CompMathMeasureAtomContext compMathMeasureAtom() {
			return getRuleContext(CompMathMeasureAtomContext.class,0);
		}
		public SelectorSecondMathOperatorContext selectorSecondMathOperator() {
			return getRuleContext(SelectorSecondMathOperatorContext.class,0);
		}
		public CompMathMeasureContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public CompMathMeasureContext(ParserRuleContext parent, int invokingState, int _p) {
			super(parent, invokingState);
			this._p = _p;
		}
		@Override public int getRuleIndex() { return RULE_compMathMeasure; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompMathMeasure(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompMathMeasure(this);
		}
	}

	public final CompMathMeasureContext compMathMeasure(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		CompMathMeasureContext _localctx = new CompMathMeasureContext(_ctx, _parentState, _p);
		CompMathMeasureContext _prevctx = _localctx;
		int _startState = 102;
		enterRecursionRule(_localctx, RULE_compMathMeasure);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(620); compMathMeasureAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(628);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,53,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompMathMeasureContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compMathMeasure);
					setState(622);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(623); selectorSecondMathOperator();
					setState(624); compMathMeasureAtom(0);
					}
					} 
				}
				setState(630);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,53,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class CompMathMeasureAtomContext extends ParserRuleContext {
		public int _p;
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public CompMathAggrFunctionContext compMathAggrFunction() {
			return getRuleContext(CompMathAggrFunctionContext.class,0);
		}
		public CompMathMeasureContext compMathMeasure() {
			return getRuleContext(CompMathMeasureContext.class,0);
		}
		public CompMathMeasureAtomContext compMathMeasureAtom(int i) {
			return getRuleContext(CompMathMeasureAtomContext.class,i);
		}
		public SelectorFirstMathOperatorContext selectorFirstMathOperator() {
			return getRuleContext(SelectorFirstMathOperatorContext.class,0);
		}
		public List<CompMathMeasureAtomContext> compMathMeasureAtom() {
			return getRuleContexts(CompMathMeasureAtomContext.class);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public CompMathMeasureAtomContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public CompMathMeasureAtomContext(ParserRuleContext parent, int invokingState, int _p) {
			super(parent, invokingState);
			this._p = _p;
		}
		@Override public int getRuleIndex() { return RULE_compMathMeasureAtom; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompMathMeasureAtom(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompMathMeasureAtom(this);
		}
	}

	public final CompMathMeasureAtomContext compMathMeasureAtom(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		CompMathMeasureAtomContext _localctx = new CompMathMeasureAtomContext(_ctx, _parentState, _p);
		CompMathMeasureAtomContext _prevctx = _localctx;
		int _startState = 104;
		enterRecursionRule(_localctx, RULE_compMathMeasureAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(637);
			switch (_input.LA(1)) {
			case AGGR_COUNT:
			case AGGR_SUM:
			case AGGR_MIN:
			case AGGR_MAX:
			case AGGR_AVERAGE:
			case AGGR_MODE:
			case AGGR_MEAN:
			case AGGR_MEDIAN:
			case SIMPLE_ID:
				{
				setState(632); compMathAggrFunction();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(633); match(BRACKET_ROUND_OPENED);
				setState(634); compMathMeasure(0);
				setState(635); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(645);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,55,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompMathMeasureAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compMathMeasureAtom);
					setState(639);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(640); selectorFirstMathOperator();
					setState(641); compMathMeasureAtom(0);
					}
					} 
				}
				setState(647);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,55,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class CompDimMeasureContext extends ParserRuleContext {
		public int _p;
		public CompDimMeasureContext compDimMeasure() {
			return getRuleContext(CompDimMeasureContext.class,0);
		}
		public CompDimMeasureAtomContext compDimMeasureAtom() {
			return getRuleContext(CompDimMeasureAtomContext.class,0);
		}
		public SelectorSecondMathOperatorContext selectorSecondMathOperator() {
			return getRuleContext(SelectorSecondMathOperatorContext.class,0);
		}
		public CompDimMeasureContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public CompDimMeasureContext(ParserRuleContext parent, int invokingState, int _p) {
			super(parent, invokingState);
			this._p = _p;
		}
		@Override public int getRuleIndex() { return RULE_compDimMeasure; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompDimMeasure(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompDimMeasure(this);
		}
	}

	public final CompDimMeasureContext compDimMeasure(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		CompDimMeasureContext _localctx = new CompDimMeasureContext(_ctx, _parentState, _p);
		CompDimMeasureContext _prevctx = _localctx;
		int _startState = 106;
		enterRecursionRule(_localctx, RULE_compDimMeasure);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(649); compDimMeasureAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(657);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,56,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDimMeasureContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDimMeasure);
					setState(651);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(652); selectorSecondMathOperator();
					setState(653); compDimMeasureAtom(0);
					}
					} 
				}
				setState(659);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,56,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class CompDimMeasureAtomContext extends ParserRuleContext {
		public int _p;
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public CompDimMeasureContext compDimMeasure() {
			return getRuleContext(CompDimMeasureContext.class,0);
		}
		public List<CompDimMeasureAtomContext> compDimMeasureAtom() {
			return getRuleContexts(CompDimMeasureAtomContext.class);
		}
		public CompDimAggrFunctionContext compDimAggrFunction() {
			return getRuleContext(CompDimAggrFunctionContext.class,0);
		}
		public SelectorFirstMathOperatorContext selectorFirstMathOperator() {
			return getRuleContext(SelectorFirstMathOperatorContext.class,0);
		}
		public CompDimMeasureAtomContext compDimMeasureAtom(int i) {
			return getRuleContext(CompDimMeasureAtomContext.class,i);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public CompDimMeasureAtomContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public CompDimMeasureAtomContext(ParserRuleContext parent, int invokingState, int _p) {
			super(parent, invokingState);
			this._p = _p;
		}
		@Override public int getRuleIndex() { return RULE_compDimMeasureAtom; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompDimMeasureAtom(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompDimMeasureAtom(this);
		}
	}

	public final CompDimMeasureAtomContext compDimMeasureAtom(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		CompDimMeasureAtomContext _localctx = new CompDimMeasureAtomContext(_ctx, _parentState, _p);
		CompDimMeasureAtomContext _prevctx = _localctx;
		int _startState = 108;
		enterRecursionRule(_localctx, RULE_compDimMeasureAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(666);
			switch (_input.LA(1)) {
			case AGGR_COUNT:
			case AGGR_SUM:
			case AGGR_MIN:
			case AGGR_MAX:
			case AGGR_AVERAGE:
			case AGGR_MODE:
			case AGGR_MEAN:
			case AGGR_MEDIAN:
			case SIMPLE_ID:
				{
				setState(661); compDimAggrFunction();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(662); match(BRACKET_ROUND_OPENED);
				setState(663); compDimMeasure(0);
				setState(664); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(674);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,58,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDimMeasureAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDimMeasureAtom);
					setState(668);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(669); selectorFirstMathOperator();
					setState(670); compDimMeasureAtom(0);
					}
					} 
				}
				setState(676);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,58,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class CompDimMathMeasureContext extends ParserRuleContext {
		public int _p;
		public CompDimMathMeasureContext compDimMathMeasure() {
			return getRuleContext(CompDimMathMeasureContext.class,0);
		}
		public CompDimMathMeasureAtomContext compDimMathMeasureAtom() {
			return getRuleContext(CompDimMathMeasureAtomContext.class,0);
		}
		public SelectorSecondMathOperatorContext selectorSecondMathOperator() {
			return getRuleContext(SelectorSecondMathOperatorContext.class,0);
		}
		public CompDimMathMeasureContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public CompDimMathMeasureContext(ParserRuleContext parent, int invokingState, int _p) {
			super(parent, invokingState);
			this._p = _p;
		}
		@Override public int getRuleIndex() { return RULE_compDimMathMeasure; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompDimMathMeasure(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompDimMathMeasure(this);
		}
	}

	public final CompDimMathMeasureContext compDimMathMeasure(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		CompDimMathMeasureContext _localctx = new CompDimMathMeasureContext(_ctx, _parentState, _p);
		CompDimMathMeasureContext _prevctx = _localctx;
		int _startState = 110;
		enterRecursionRule(_localctx, RULE_compDimMathMeasure);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(678); compDimMathMeasureAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(686);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,59,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDimMathMeasureContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDimMathMeasure);
					setState(680);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(681); selectorSecondMathOperator();
					setState(682); compDimMathMeasureAtom(0);
					}
					} 
				}
				setState(688);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,59,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class CompDimMathMeasureAtomContext extends ParserRuleContext {
		public int _p;
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public CompDimMeasureContext compDimMeasure() {
			return getRuleContext(CompDimMeasureContext.class,0);
		}
		public CompMathMeasureContext compMathMeasure() {
			return getRuleContext(CompMathMeasureContext.class,0);
		}
		public SelectorFirstMathOperatorContext selectorFirstMathOperator() {
			return getRuleContext(SelectorFirstMathOperatorContext.class,0);
		}
		public CompDimMathMeasureAtomContext compDimMathMeasureAtom(int i) {
			return getRuleContext(CompDimMathMeasureAtomContext.class,i);
		}
		public List<CompDimMathMeasureAtomContext> compDimMathMeasureAtom() {
			return getRuleContexts(CompDimMathMeasureAtomContext.class);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public CompDimMathMeasureAtomContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public CompDimMathMeasureAtomContext(ParserRuleContext parent, int invokingState, int _p) {
			super(parent, invokingState);
			this._p = _p;
		}
		@Override public int getRuleIndex() { return RULE_compDimMathMeasureAtom; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompDimMathMeasureAtom(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompDimMathMeasureAtom(this);
		}
	}

	public final CompDimMathMeasureAtomContext compDimMathMeasureAtom(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		CompDimMathMeasureAtomContext _localctx = new CompDimMathMeasureAtomContext(_ctx, _parentState, _p);
		CompDimMathMeasureAtomContext _prevctx = _localctx;
		int _startState = 112;
		enterRecursionRule(_localctx, RULE_compDimMathMeasureAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(696);
			switch ( getInterpreter().adaptivePredict(_input,60,_ctx) ) {
			case 1:
				{
				setState(690); compMathMeasure(0);
				}
				break;

			case 2:
				{
				setState(691); compDimMeasure(0);
				}
				break;

			case 3:
				{
				setState(692); match(BRACKET_ROUND_OPENED);
				setState(693); compDimMathMeasureAtom(0);
				setState(694); match(BRACKET_ROUND_CLOSED);
				}
				break;
			}
			_ctx.stop = _input.LT(-1);
			setState(704);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,61,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDimMathMeasureAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDimMathMeasureAtom);
					setState(698);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(699); selectorFirstMathOperator();
					setState(700); compDimMathMeasureAtom(0);
					}
					} 
				}
				setState(706);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,61,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class CompDescriptorFormulaContext extends ParserRuleContext {
		public int _p;
		public CompDescriptorFormulaContext compDescriptorFormula() {
			return getRuleContext(CompDescriptorFormulaContext.class,0);
		}
		public SelectorSecondMathOperatorContext selectorSecondMathOperator() {
			return getRuleContext(SelectorSecondMathOperatorContext.class,0);
		}
		public CompDescriptorFormulaAtomContext compDescriptorFormulaAtom() {
			return getRuleContext(CompDescriptorFormulaAtomContext.class,0);
		}
		public CompDescriptorFormulaContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public CompDescriptorFormulaContext(ParserRuleContext parent, int invokingState, int _p) {
			super(parent, invokingState);
			this._p = _p;
		}
		@Override public int getRuleIndex() { return RULE_compDescriptorFormula; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompDescriptorFormula(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompDescriptorFormula(this);
		}
	}

	public final CompDescriptorFormulaContext compDescriptorFormula(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		CompDescriptorFormulaContext _localctx = new CompDescriptorFormulaContext(_ctx, _parentState, _p);
		CompDescriptorFormulaContext _prevctx = _localctx;
		int _startState = 114;
		enterRecursionRule(_localctx, RULE_compDescriptorFormula);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(708); compDescriptorFormulaAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(716);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,62,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDescriptorFormulaContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDescriptorFormula);
					setState(710);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(711); selectorSecondMathOperator();
					setState(712); compDescriptorFormulaAtom(0);
					}
					} 
				}
				setState(718);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,62,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class CompDescriptorFormulaAtomContext extends ParserRuleContext {
		public int _p;
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public CompDescriptorFormulaAtomContext compDescriptorFormulaAtom(int i) {
			return getRuleContext(CompDescriptorFormulaAtomContext.class,i);
		}
		public SelectorFirstMathOperatorContext selectorFirstMathOperator() {
			return getRuleContext(SelectorFirstMathOperatorContext.class,0);
		}
		public CompDescriptorFormulaContext compDescriptorFormula() {
			return getRuleContext(CompDescriptorFormulaContext.class,0);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public SelectorDescriptorIdContext selectorDescriptorId() {
			return getRuleContext(SelectorDescriptorIdContext.class,0);
		}
		public List<CompDescriptorFormulaAtomContext> compDescriptorFormulaAtom() {
			return getRuleContexts(CompDescriptorFormulaAtomContext.class);
		}
		public CompDescriptorFormulaAtomContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public CompDescriptorFormulaAtomContext(ParserRuleContext parent, int invokingState, int _p) {
			super(parent, invokingState);
			this._p = _p;
		}
		@Override public int getRuleIndex() { return RULE_compDescriptorFormulaAtom; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompDescriptorFormulaAtom(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompDescriptorFormulaAtom(this);
		}
	}

	public final CompDescriptorFormulaAtomContext compDescriptorFormulaAtom(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		CompDescriptorFormulaAtomContext _localctx = new CompDescriptorFormulaAtomContext(_ctx, _parentState, _p);
		CompDescriptorFormulaAtomContext _prevctx = _localctx;
		int _startState = 116;
		enterRecursionRule(_localctx, RULE_compDescriptorFormulaAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(725);
			switch (_input.LA(1)) {
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				{
				setState(720); selectorDescriptorId();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(721); match(BRACKET_ROUND_OPENED);
				setState(722); compDescriptorFormula(0);
				setState(723); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(733);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,64,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDescriptorFormulaAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDescriptorFormulaAtom);
					setState(727);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(728); selectorFirstMathOperator();
					setState(729); compDescriptorFormulaAtom(0);
					}
					} 
				}
				setState(735);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,64,_ctx);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			unrollRecursionContexts(_parentctx);
		}
		return _localctx;
	}

	public static class SelectorMemberContext extends ParserRuleContext {
		public List<TerminalNode> SIMPLE_ID() { return getTokens(QueryGrammarParser.SIMPLE_ID); }
		public List<TerminalNode> DIMSEPARATOR() { return getTokens(QueryGrammarParser.DIMSEPARATOR); }
		public TerminalNode SIMPLE_ID(int i) {
			return getToken(QueryGrammarParser.SIMPLE_ID, i);
		}
		public TerminalNode MARKED_ID(int i) {
			return getToken(QueryGrammarParser.MARKED_ID, i);
		}
		public TerminalNode DIMSEPARATOR(int i) {
			return getToken(QueryGrammarParser.DIMSEPARATOR, i);
		}
		public List<TerminalNode> MARKED_ID() { return getTokens(QueryGrammarParser.MARKED_ID); }
		public TerminalNode ENHANCED_ID(int i) {
			return getToken(QueryGrammarParser.ENHANCED_ID, i);
		}
		public List<TerminalNode> ENHANCED_ID() { return getTokens(QueryGrammarParser.ENHANCED_ID); }
		public SelectorMemberContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorMember; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorMember(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorMember(this);
		}
	}

	public final SelectorMemberContext selectorMember() throws RecognitionException {
		SelectorMemberContext _localctx = new SelectorMemberContext(_ctx, getState());
		enterRule(_localctx, 118, RULE_selectorMember);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(736);
			_la = _input.LA(1);
			if ( !(_la==MARKED_ID || _la==SIMPLE_ID || _la==ENHANCED_ID) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(737); match(DIMSEPARATOR);
			setState(738);
			_la = _input.LA(1);
			if ( !(_la==MARKED_ID || _la==SIMPLE_ID || _la==ENHANCED_ID) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(739); match(DIMSEPARATOR);
			setState(740);
			_la = _input.LA(1);
			if ( !(_la==MARKED_ID || _la==SIMPLE_ID || _la==ENHANCED_ID) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorModelIdContext extends ParserRuleContext {
		public TerminalNode SIMPLE_ID() { return getToken(QueryGrammarParser.SIMPLE_ID, 0); }
		public TerminalNode MARKED_ID() { return getToken(QueryGrammarParser.MARKED_ID, 0); }
		public TerminalNode ENHANCED_ID() { return getToken(QueryGrammarParser.ENHANCED_ID, 0); }
		public SelectorModelIdContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorModelId; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorModelId(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorModelId(this);
		}
	}

	public final SelectorModelIdContext selectorModelId() throws RecognitionException {
		SelectorModelIdContext _localctx = new SelectorModelIdContext(_ctx, getState());
		enterRule(_localctx, 120, RULE_selectorModelId);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(742);
			_la = _input.LA(1);
			if ( !(_la==MARKED_ID || _la==SIMPLE_ID || _la==ENHANCED_ID) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorDescriptorIdContext extends ParserRuleContext {
		public TerminalNode SIMPLE_ID() { return getToken(QueryGrammarParser.SIMPLE_ID, 0); }
		public TerminalNode MARKED_ID() { return getToken(QueryGrammarParser.MARKED_ID, 0); }
		public TerminalNode ENHANCED_ID() { return getToken(QueryGrammarParser.ENHANCED_ID, 0); }
		public SelectorDescriptorIdContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorDescriptorId; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorDescriptorId(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorDescriptorId(this);
		}
	}

	public final SelectorDescriptorIdContext selectorDescriptorId() throws RecognitionException {
		SelectorDescriptorIdContext _localctx = new SelectorDescriptorIdContext(_ctx, getState());
		enterRule(_localctx, 122, RULE_selectorDescriptorId);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(744);
			_la = _input.LA(1);
			if ( !(_la==MARKED_ID || _la==SIMPLE_ID || _la==ENHANCED_ID) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorAliasContext extends ParserRuleContext {
		public TerminalNode SIMPLE_ID() { return getToken(QueryGrammarParser.SIMPLE_ID, 0); }
		public TerminalNode MARKED_ID() { return getToken(QueryGrammarParser.MARKED_ID, 0); }
		public TerminalNode ENHANCED_ID() { return getToken(QueryGrammarParser.ENHANCED_ID, 0); }
		public SelectorAliasContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorAlias; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorAlias(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorAlias(this);
		}
	}

	public final SelectorAliasContext selectorAlias() throws RecognitionException {
		SelectorAliasContext _localctx = new SelectorAliasContext(_ctx, getState());
		enterRule(_localctx, 124, RULE_selectorAlias);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(746);
			_la = _input.LA(1);
			if ( !(_la==MARKED_ID || _la==SIMPLE_ID || _la==ENHANCED_ID) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorDateIntervalContext extends ParserRuleContext {
		public List<TerminalNode> DATE() { return getTokens(QueryGrammarParser.DATE); }
		public TerminalNode SEPARATOR() { return getToken(QueryGrammarParser.SEPARATOR, 0); }
		public TerminalNode DATE(int i) {
			return getToken(QueryGrammarParser.DATE, i);
		}
		public SelectorDateIntervalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorDateInterval; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorDateInterval(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorDateInterval(this);
		}
	}

	public final SelectorDateIntervalContext selectorDateInterval() throws RecognitionException {
		SelectorDateIntervalContext _localctx = new SelectorDateIntervalContext(_ctx, getState());
		enterRule(_localctx, 126, RULE_selectorDateInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(748); match(DATE);
			setState(749); match(SEPARATOR);
			setState(750); match(DATE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorOffsetContext extends ParserRuleContext {
		public TerminalNode INT() { return getToken(QueryGrammarParser.INT, 0); }
		public SelectorOffsetContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorOffset; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorOffset(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorOffset(this);
		}
	}

	public final SelectorOffsetContext selectorOffset() throws RecognitionException {
		SelectorOffsetContext _localctx = new SelectorOffsetContext(_ctx, getState());
		enterRule(_localctx, 128, RULE_selectorOffset);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(752); match(INT);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorLimitContext extends ParserRuleContext {
		public TerminalNode INT() { return getToken(QueryGrammarParser.INT, 0); }
		public SelectorLimitContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorLimit; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorLimit(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorLimit(this);
		}
	}

	public final SelectorLimitContext selectorLimit() throws RecognitionException {
		SelectorLimitContext _localctx = new SelectorLimitContext(_ctx, getState());
		enterRule(_localctx, 130, RULE_selectorLimit);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(754); match(INT);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorIntIntervalContext extends ParserRuleContext {
		public List<TerminalNode> INT() { return getTokens(QueryGrammarParser.INT); }
		public TerminalNode SEPARATOR() { return getToken(QueryGrammarParser.SEPARATOR, 0); }
		public TerminalNode INT(int i) {
			return getToken(QueryGrammarParser.INT, i);
		}
		public SelectorIntIntervalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorIntInterval; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorIntInterval(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorIntInterval(this);
		}
	}

	public final SelectorIntIntervalContext selectorIntInterval() throws RecognitionException {
		SelectorIntIntervalContext _localctx = new SelectorIntIntervalContext(_ctx, getState());
		enterRule(_localctx, 132, RULE_selectorIntInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(756); match(INT);
			setState(757); match(SEPARATOR);
			setState(758); match(INT);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorIntIdListContext extends ParserRuleContext {
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public List<TerminalNode> INT() { return getTokens(QueryGrammarParser.INT); }
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public TerminalNode INT(int i) {
			return getToken(QueryGrammarParser.INT, i);
		}
		public SelectorIntIdListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorIntIdList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorIntIdList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorIntIdList(this);
		}
	}

	public final SelectorIntIdListContext selectorIntIdList() throws RecognitionException {
		SelectorIntIdListContext _localctx = new SelectorIntIdListContext(_ctx, getState());
		enterRule(_localctx, 134, RULE_selectorIntIdList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(760); match(INT);
			setState(765);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(761); match(SEPARATOR);
				setState(762); match(INT);
				}
				}
				setState(767);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorDateIntervalWithNullContext extends ParserRuleContext {
		public TerminalNode NULL_VALUE(int i) {
			return getToken(QueryGrammarParser.NULL_VALUE, i);
		}
		public List<TerminalNode> DATE() { return getTokens(QueryGrammarParser.DATE); }
		public TerminalNode SEPARATOR() { return getToken(QueryGrammarParser.SEPARATOR, 0); }
		public List<TerminalNode> NULL_VALUE() { return getTokens(QueryGrammarParser.NULL_VALUE); }
		public TerminalNode DATE(int i) {
			return getToken(QueryGrammarParser.DATE, i);
		}
		public SelectorDateIntervalWithNullContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorDateIntervalWithNull; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorDateIntervalWithNull(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorDateIntervalWithNull(this);
		}
	}

	public final SelectorDateIntervalWithNullContext selectorDateIntervalWithNull() throws RecognitionException {
		SelectorDateIntervalWithNullContext _localctx = new SelectorDateIntervalWithNullContext(_ctx, getState());
		enterRule(_localctx, 136, RULE_selectorDateIntervalWithNull);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(768);
			_la = _input.LA(1);
			if ( !(_la==NULL_VALUE || _la==DATE) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(769); match(SEPARATOR);
			setState(770);
			_la = _input.LA(1);
			if ( !(_la==NULL_VALUE || _la==DATE) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorIntIntervalWithNullContext extends ParserRuleContext {
		public TerminalNode NULL_VALUE(int i) {
			return getToken(QueryGrammarParser.NULL_VALUE, i);
		}
		public List<TerminalNode> INT() { return getTokens(QueryGrammarParser.INT); }
		public TerminalNode SEPARATOR() { return getToken(QueryGrammarParser.SEPARATOR, 0); }
		public TerminalNode INT(int i) {
			return getToken(QueryGrammarParser.INT, i);
		}
		public List<TerminalNode> NULL_VALUE() { return getTokens(QueryGrammarParser.NULL_VALUE); }
		public SelectorIntIntervalWithNullContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorIntIntervalWithNull; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorIntIntervalWithNull(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorIntIntervalWithNull(this);
		}
	}

	public final SelectorIntIntervalWithNullContext selectorIntIntervalWithNull() throws RecognitionException {
		SelectorIntIntervalWithNullContext _localctx = new SelectorIntIntervalWithNullContext(_ctx, getState());
		enterRule(_localctx, 138, RULE_selectorIntIntervalWithNull);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(772);
			_la = _input.LA(1);
			if ( !(_la==NULL_VALUE || _la==INT) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(773); match(SEPARATOR);
			setState(774);
			_la = _input.LA(1);
			if ( !(_la==NULL_VALUE || _la==INT) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorDateValueContext extends ParserRuleContext {
		public TerminalNode DATE() { return getToken(QueryGrammarParser.DATE, 0); }
		public SelectorDateValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorDateValue; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorDateValue(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorDateValue(this);
		}
	}

	public final SelectorDateValueContext selectorDateValue() throws RecognitionException {
		SelectorDateValueContext _localctx = new SelectorDateValueContext(_ctx, getState());
		enterRule(_localctx, 140, RULE_selectorDateValue);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(776); match(DATE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorIntValueContext extends ParserRuleContext {
		public TerminalNode INT() { return getToken(QueryGrammarParser.INT, 0); }
		public SelectorIntValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorIntValue; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorIntValue(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorIntValue(this);
		}
	}

	public final SelectorIntValueContext selectorIntValue() throws RecognitionException {
		SelectorIntValueContext _localctx = new SelectorIntValueContext(_ctx, getState());
		enterRule(_localctx, 142, RULE_selectorIntValue);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(778); match(INT);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorNullValueContext extends ParserRuleContext {
		public TerminalNode NULL_VALUE() { return getToken(QueryGrammarParser.NULL_VALUE, 0); }
		public SelectorNullValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorNullValue; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorNullValue(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorNullValue(this);
		}
	}

	public final SelectorNullValueContext selectorNullValue() throws RecognitionException {
		SelectorNullValueContext _localctx = new SelectorNullValueContext(_ctx, getState());
		enterRule(_localctx, 144, RULE_selectorNullValue);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(780); match(NULL_VALUE);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorValueContext extends ParserRuleContext {
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public TerminalNode NULL_VALUE() { return getToken(QueryGrammarParser.NULL_VALUE, 0); }
		public SelectorValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorValue; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorValue(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorValue(this);
		}
	}

	public final SelectorValueContext selectorValue() throws RecognitionException {
		SelectorValueContext _localctx = new SelectorValueContext(_ctx, getState());
		enterRule(_localctx, 146, RULE_selectorValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(782);
			_la = _input.LA(1);
			if ( !(_la==VALUE || _la==NULL_VALUE) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorOpenIntervalContext extends ParserRuleContext {
		public TerminalNode BRACKET_SQUARE_OPENED() { return getToken(QueryGrammarParser.BRACKET_SQUARE_OPENED, 0); }
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public SelectorOpenIntervalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorOpenInterval; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorOpenInterval(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorOpenInterval(this);
		}
	}

	public final SelectorOpenIntervalContext selectorOpenInterval() throws RecognitionException {
		SelectorOpenIntervalContext _localctx = new SelectorOpenIntervalContext(_ctx, getState());
		enterRule(_localctx, 148, RULE_selectorOpenInterval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(784);
			_la = _input.LA(1);
			if ( !(_la==BRACKET_ROUND_OPENED || _la==BRACKET_SQUARE_OPENED) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorCloseIntervalContext extends ParserRuleContext {
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public TerminalNode BRACKET_SQUARE_CLOSED() { return getToken(QueryGrammarParser.BRACKET_SQUARE_CLOSED, 0); }
		public SelectorCloseIntervalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorCloseInterval; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorCloseInterval(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorCloseInterval(this);
		}
	}

	public final SelectorCloseIntervalContext selectorCloseInterval() throws RecognitionException {
		SelectorCloseIntervalContext _localctx = new SelectorCloseIntervalContext(_ctx, getState());
		enterRule(_localctx, 150, RULE_selectorCloseInterval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(786);
			_la = _input.LA(1);
			if ( !(_la==BRACKET_ROUND_CLOSED || _la==BRACKET_SQUARE_CLOSED) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorMathAggrFunctionNameContext extends ParserRuleContext {
		public TerminalNode AGGR_SUM() { return getToken(QueryGrammarParser.AGGR_SUM, 0); }
		public TerminalNode SIMPLE_ID() { return getToken(QueryGrammarParser.SIMPLE_ID, 0); }
		public TerminalNode AGGR_MEAN() { return getToken(QueryGrammarParser.AGGR_MEAN, 0); }
		public TerminalNode AGGR_AVERAGE() { return getToken(QueryGrammarParser.AGGR_AVERAGE, 0); }
		public TerminalNode AGGR_MIN() { return getToken(QueryGrammarParser.AGGR_MIN, 0); }
		public TerminalNode AGGR_MODE() { return getToken(QueryGrammarParser.AGGR_MODE, 0); }
		public TerminalNode AGGR_MAX() { return getToken(QueryGrammarParser.AGGR_MAX, 0); }
		public TerminalNode AGGR_MEDIAN() { return getToken(QueryGrammarParser.AGGR_MEDIAN, 0); }
		public TerminalNode AGGR_COUNT() { return getToken(QueryGrammarParser.AGGR_COUNT, 0); }
		public SelectorMathAggrFunctionNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorMathAggrFunctionName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorMathAggrFunctionName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorMathAggrFunctionName(this);
		}
	}

	public final SelectorMathAggrFunctionNameContext selectorMathAggrFunctionName() throws RecognitionException {
		SelectorMathAggrFunctionNameContext _localctx = new SelectorMathAggrFunctionNameContext(_ctx, getState());
		enterRule(_localctx, 152, RULE_selectorMathAggrFunctionName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(788);
			_la = _input.LA(1);
			if ( !(((((_la - 77)) & ~0x3f) == 0 && ((1L << (_la - 77)) & ((1L << (AGGR_COUNT - 77)) | (1L << (AGGR_SUM - 77)) | (1L << (AGGR_MIN - 77)) | (1L << (AGGR_MAX - 77)) | (1L << (AGGR_AVERAGE - 77)) | (1L << (AGGR_MODE - 77)) | (1L << (AGGR_MEAN - 77)) | (1L << (AGGR_MEDIAN - 77)) | (1L << (SIMPLE_ID - 77)))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorDimAggrFunctionNameContext extends ParserRuleContext {
		public TerminalNode AGGR_SUM() { return getToken(QueryGrammarParser.AGGR_SUM, 0); }
		public TerminalNode SIMPLE_ID() { return getToken(QueryGrammarParser.SIMPLE_ID, 0); }
		public TerminalNode AGGR_MEAN() { return getToken(QueryGrammarParser.AGGR_MEAN, 0); }
		public TerminalNode AGGR_AVERAGE() { return getToken(QueryGrammarParser.AGGR_AVERAGE, 0); }
		public TerminalNode AGGR_MIN() { return getToken(QueryGrammarParser.AGGR_MIN, 0); }
		public TerminalNode AGGR_MODE() { return getToken(QueryGrammarParser.AGGR_MODE, 0); }
		public TerminalNode AGGR_MAX() { return getToken(QueryGrammarParser.AGGR_MAX, 0); }
		public TerminalNode AGGR_MEDIAN() { return getToken(QueryGrammarParser.AGGR_MEDIAN, 0); }
		public TerminalNode AGGR_COUNT() { return getToken(QueryGrammarParser.AGGR_COUNT, 0); }
		public SelectorDimAggrFunctionNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorDimAggrFunctionName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorDimAggrFunctionName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorDimAggrFunctionName(this);
		}
	}

	public final SelectorDimAggrFunctionNameContext selectorDimAggrFunctionName() throws RecognitionException {
		SelectorDimAggrFunctionNameContext _localctx = new SelectorDimAggrFunctionNameContext(_ctx, getState());
		enterRule(_localctx, 154, RULE_selectorDimAggrFunctionName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(790);
			_la = _input.LA(1);
			if ( !(((((_la - 77)) & ~0x3f) == 0 && ((1L << (_la - 77)) & ((1L << (AGGR_COUNT - 77)) | (1L << (AGGR_SUM - 77)) | (1L << (AGGR_MIN - 77)) | (1L << (AGGR_MAX - 77)) | (1L << (AGGR_AVERAGE - 77)) | (1L << (AGGR_MODE - 77)) | (1L << (AGGR_MEAN - 77)) | (1L << (AGGR_MEDIAN - 77)) | (1L << (SIMPLE_ID - 77)))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorLowAggrFunctionNameContext extends ParserRuleContext {
		public TerminalNode AGGR_SUM() { return getToken(QueryGrammarParser.AGGR_SUM, 0); }
		public TerminalNode SIMPLE_ID() { return getToken(QueryGrammarParser.SIMPLE_ID, 0); }
		public TerminalNode AGGR_COUNTSTARTED() { return getToken(QueryGrammarParser.AGGR_COUNTSTARTED, 0); }
		public TerminalNode AGGR_MEAN() { return getToken(QueryGrammarParser.AGGR_MEAN, 0); }
		public TerminalNode AGGR_AVERAGE() { return getToken(QueryGrammarParser.AGGR_AVERAGE, 0); }
		public TerminalNode AGGR_MIN() { return getToken(QueryGrammarParser.AGGR_MIN, 0); }
		public TerminalNode AGGR_MODE() { return getToken(QueryGrammarParser.AGGR_MODE, 0); }
		public TerminalNode AGGR_MAX() { return getToken(QueryGrammarParser.AGGR_MAX, 0); }
		public TerminalNode AGGR_COUNTFINISHED() { return getToken(QueryGrammarParser.AGGR_COUNTFINISHED, 0); }
		public TerminalNode AGGR_MEDIAN() { return getToken(QueryGrammarParser.AGGR_MEDIAN, 0); }
		public TerminalNode AGGR_COUNT() { return getToken(QueryGrammarParser.AGGR_COUNT, 0); }
		public SelectorLowAggrFunctionNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorLowAggrFunctionName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorLowAggrFunctionName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorLowAggrFunctionName(this);
		}
	}

	public final SelectorLowAggrFunctionNameContext selectorLowAggrFunctionName() throws RecognitionException {
		SelectorLowAggrFunctionNameContext _localctx = new SelectorLowAggrFunctionNameContext(_ctx, getState());
		enterRule(_localctx, 156, RULE_selectorLowAggrFunctionName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(792);
			_la = _input.LA(1);
			if ( !(((((_la - 75)) & ~0x3f) == 0 && ((1L << (_la - 75)) & ((1L << (AGGR_COUNTSTARTED - 75)) | (1L << (AGGR_COUNTFINISHED - 75)) | (1L << (AGGR_COUNT - 75)) | (1L << (AGGR_SUM - 75)) | (1L << (AGGR_MIN - 75)) | (1L << (AGGR_MAX - 75)) | (1L << (AGGR_AVERAGE - 75)) | (1L << (AGGR_MODE - 75)) | (1L << (AGGR_MEAN - 75)) | (1L << (AGGR_MEDIAN - 75)) | (1L << (SIMPLE_ID - 75)))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorFirstMathOperatorContext extends ParserRuleContext {
		public TerminalNode MATH_MULTIPLY() { return getToken(QueryGrammarParser.MATH_MULTIPLY, 0); }
		public TerminalNode MATH_DIVISION() { return getToken(QueryGrammarParser.MATH_DIVISION, 0); }
		public SelectorFirstMathOperatorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorFirstMathOperator; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorFirstMathOperator(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorFirstMathOperator(this);
		}
	}

	public final SelectorFirstMathOperatorContext selectorFirstMathOperator() throws RecognitionException {
		SelectorFirstMathOperatorContext _localctx = new SelectorFirstMathOperatorContext(_ctx, getState());
		enterRule(_localctx, 158, RULE_selectorFirstMathOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(794);
			_la = _input.LA(1);
			if ( !(_la==MATH_MULTIPLY || _la==MATH_DIVISION) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorSecondMathOperatorContext extends ParserRuleContext {
		public TerminalNode MATH_MINUS() { return getToken(QueryGrammarParser.MATH_MINUS, 0); }
		public TerminalNode MATH_PLUS() { return getToken(QueryGrammarParser.MATH_PLUS, 0); }
		public SelectorSecondMathOperatorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorSecondMathOperator; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorSecondMathOperator(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorSecondMathOperator(this);
		}
	}

	public final SelectorSecondMathOperatorContext selectorSecondMathOperator() throws RecognitionException {
		SelectorSecondMathOperatorContext _localctx = new SelectorSecondMathOperatorContext(_ctx, getState());
		enterRule(_localctx, 160, RULE_selectorSecondMathOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(796);
			_la = _input.LA(1);
			if ( !(_la==MATH_PLUS || _la==MATH_MINUS) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorIntervalDefContext extends ParserRuleContext {
		public TerminalNode POS_END_INCL() { return getToken(QueryGrammarParser.POS_END_INCL, 0); }
		public TerminalNode POS_END_EXCL() { return getToken(QueryGrammarParser.POS_END_EXCL, 0); }
		public TerminalNode POS_START_EXCL() { return getToken(QueryGrammarParser.POS_START_EXCL, 0); }
		public TerminalNode POS_START_INCL() { return getToken(QueryGrammarParser.POS_START_INCL, 0); }
		public SelectorIntervalDefContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorIntervalDef; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorIntervalDef(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorIntervalDef(this);
		}
	}

	public final SelectorIntervalDefContext selectorIntervalDef() throws RecognitionException {
		SelectorIntervalDefContext _localctx = new SelectorIntervalDefContext(_ctx, getState());
		enterRule(_localctx, 162, RULE_selectorIntervalDef);
		int _la;
		try {
			setState(800);
			switch (_input.LA(1)) {
			case POS_START_INCL:
			case POS_START_EXCL:
				enterOuterAlt(_localctx, 1);
				{
				setState(798);
				_la = _input.LA(1);
				if ( !(_la==POS_START_INCL || _la==POS_START_EXCL) ) {
				_errHandler.recoverInline(this);
				}
				consume();
				}
				break;
			case POS_END_INCL:
			case POS_END_EXCL:
				enterOuterAlt(_localctx, 2);
				{
				setState(799);
				_la = _input.LA(1);
				if ( !(_la==POS_END_INCL || _la==POS_END_EXCL) ) {
				_errHandler.recoverInline(this);
				}
				consume();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorBooleanContext extends ParserRuleContext {
		public TerminalNode LOGICAL_FALSE() { return getToken(QueryGrammarParser.LOGICAL_FALSE, 0); }
		public TerminalNode LOGICAL_TRUE() { return getToken(QueryGrammarParser.LOGICAL_TRUE, 0); }
		public SelectorBooleanContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorBoolean; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorBoolean(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorBoolean(this);
		}
	}

	public final SelectorBooleanContext selectorBoolean() throws RecognitionException {
		SelectorBooleanContext _localctx = new SelectorBooleanContext(_ctx, getState());
		enterRule(_localctx, 164, RULE_selectorBoolean);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(802);
			_la = _input.LA(1);
			if ( !(_la==LOGICAL_TRUE || _la==LOGICAL_FALSE) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorIntervalRelationContext extends ParserRuleContext {
		public TerminalNode IR_CONTAINING() { return getToken(QueryGrammarParser.IR_CONTAINING, 0); }
		public TerminalNode IR_STARTINGWITH() { return getToken(QueryGrammarParser.IR_STARTINGWITH, 0); }
		public TerminalNode IR_BEFORE() { return getToken(QueryGrammarParser.IR_BEFORE, 0); }
		public TerminalNode IR_MEETING() { return getToken(QueryGrammarParser.IR_MEETING, 0); }
		public TerminalNode IR_OVERLAPPING() { return getToken(QueryGrammarParser.IR_OVERLAPPING, 0); }
		public TerminalNode IR_AFTER() { return getToken(QueryGrammarParser.IR_AFTER, 0); }
		public TerminalNode IR_WITHIN() { return getToken(QueryGrammarParser.IR_WITHIN, 0); }
		public TerminalNode IR_FINISHINGWITH() { return getToken(QueryGrammarParser.IR_FINISHINGWITH, 0); }
		public TerminalNode IR_DURING() { return getToken(QueryGrammarParser.IR_DURING, 0); }
		public TerminalNode IR_EQUALTO() { return getToken(QueryGrammarParser.IR_EQUALTO, 0); }
		public SelectorIntervalRelationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorIntervalRelation; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorIntervalRelation(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorIntervalRelation(this);
		}
	}

	public final SelectorIntervalRelationContext selectorIntervalRelation() throws RecognitionException {
		SelectorIntervalRelationContext _localctx = new SelectorIntervalRelationContext(_ctx, getState());
		enterRule(_localctx, 166, RULE_selectorIntervalRelation);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(804);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << IR_EQUALTO) | (1L << IR_BEFORE) | (1L << IR_AFTER) | (1L << IR_MEETING) | (1L << IR_OVERLAPPING) | (1L << IR_DURING) | (1L << IR_WITHIN) | (1L << IR_CONTAINING) | (1L << IR_STARTINGWITH) | (1L << IR_FINISHINGWITH))) != 0)) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SelectorValueListContext extends ParserRuleContext {
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public List<TerminalNode> VALUE() { return getTokens(QueryGrammarParser.VALUE); }
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public TerminalNode VALUE(int i) {
			return getToken(QueryGrammarParser.VALUE, i);
		}
		public SelectorValueListContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorValueList; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorValueList(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorValueList(this);
		}
	}

	public final SelectorValueListContext selectorValueList() throws RecognitionException {
		SelectorValueListContext _localctx = new SelectorValueListContext(_ctx, getState());
		enterRule(_localctx, 168, RULE_selectorValueList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(806); match(VALUE);
			setState(811);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(807); match(SEPARATOR);
				setState(808); match(VALUE);
				}
				}
				setState(813);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 32: return exprComp_sempred((ExprCompContext)_localctx, predIndex);

		case 49: return compLowMeasure_sempred((CompLowMeasureContext)_localctx, predIndex);

		case 50: return compLowMeasureAtom_sempred((CompLowMeasureAtomContext)_localctx, predIndex);

		case 51: return compMathMeasure_sempred((CompMathMeasureContext)_localctx, predIndex);

		case 52: return compMathMeasureAtom_sempred((CompMathMeasureAtomContext)_localctx, predIndex);

		case 53: return compDimMeasure_sempred((CompDimMeasureContext)_localctx, predIndex);

		case 54: return compDimMeasureAtom_sempred((CompDimMeasureAtomContext)_localctx, predIndex);

		case 55: return compDimMathMeasure_sempred((CompDimMathMeasureContext)_localctx, predIndex);

		case 56: return compDimMathMeasureAtom_sempred((CompDimMathMeasureAtomContext)_localctx, predIndex);

		case 57: return compDescriptorFormula_sempred((CompDescriptorFormulaContext)_localctx, predIndex);

		case 58: return compDescriptorFormulaAtom_sempred((CompDescriptorFormulaAtomContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean exprComp_sempred(ExprCompContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0: return 1 >= _localctx._p;
		}
		return true;
	}
	private boolean compDimMeasure_sempred(CompDimMeasureContext _localctx, int predIndex) {
		switch (predIndex) {
		case 5: return 2 >= _localctx._p;
		}
		return true;
	}
	private boolean compMathMeasureAtom_sempred(CompMathMeasureAtomContext _localctx, int predIndex) {
		switch (predIndex) {
		case 4: return 2 >= _localctx._p;
		}
		return true;
	}
	private boolean compDimMathMeasureAtom_sempred(CompDimMathMeasureAtomContext _localctx, int predIndex) {
		switch (predIndex) {
		case 8: return 2 >= _localctx._p;
		}
		return true;
	}
	private boolean compDimMathMeasure_sempred(CompDimMathMeasureContext _localctx, int predIndex) {
		switch (predIndex) {
		case 7: return 2 >= _localctx._p;
		}
		return true;
	}
	private boolean compDescriptorFormula_sempred(CompDescriptorFormulaContext _localctx, int predIndex) {
		switch (predIndex) {
		case 9: return 2 >= _localctx._p;
		}
		return true;
	}
	private boolean compDescriptorFormulaAtom_sempred(CompDescriptorFormulaAtomContext _localctx, int predIndex) {
		switch (predIndex) {
		case 10: return 2 >= _localctx._p;
		}
		return true;
	}
	private boolean compMathMeasure_sempred(CompMathMeasureContext _localctx, int predIndex) {
		switch (predIndex) {
		case 3: return 2 >= _localctx._p;
		}
		return true;
	}
	private boolean compDimMeasureAtom_sempred(CompDimMeasureAtomContext _localctx, int predIndex) {
		switch (predIndex) {
		case 6: return 2 >= _localctx._p;
		}
		return true;
	}
	private boolean compLowMeasureAtom_sempred(CompLowMeasureAtomContext _localctx, int predIndex) {
		switch (predIndex) {
		case 2: return 2 >= _localctx._p;
		}
		return true;
	}
	private boolean compLowMeasure_sempred(CompLowMeasureContext _localctx, int predIndex) {
		switch (predIndex) {
		case 1: return 2 >= _localctx._p;
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\3d\u0331\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t+\4"+
		",\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t\61\4\62\t\62\4\63\t\63\4\64\t"+
		"\64\4\65\t\65\4\66\t\66\4\67\t\67\48\t8\49\t9\4:\t:\4;\t;\4<\t<\4=\t="+
		"\4>\t>\4?\t?\4@\t@\4A\tA\4B\tB\4C\tC\4D\tD\4E\tE\4F\tF\4G\tG\4H\tH\4I"+
		"\tI\4J\tJ\4K\tK\4L\tL\4M\tM\4N\tN\4O\tO\4P\tP\4Q\tQ\4R\tR\4S\tS\4T\tT"+
		"\4U\tU\4V\tV\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\5"+
		"\2\u00bb\n\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\5\3\u00c5\n\3\3\3\3\3\5\3"+
		"\u00c9\n\3\5\3\u00cb\n\3\5\3\u00cd\n\3\3\3\3\3\3\3\5\3\u00d2\n\3\5\3\u00d4"+
		"\n\3\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3"+
		"\7\5\7\u00e7\n\7\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\5\b\u00f1\n\b\3\t\3\t"+
		"\3\t\3\t\3\t\3\n\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3"+
		"\f\3\f\3\f\3\f\3\f\3\f\3\f\3\r\3\r\3\r\5\r\u010e\n\r\3\r\3\r\3\r\3\r\3"+
		"\16\3\16\3\16\3\17\3\17\3\17\3\20\3\20\3\20\5\20\u011d\n\20\3\20\3\20"+
		"\3\20\3\20\3\21\3\21\3\21\3\22\3\22\3\22\3\23\3\23\3\23\3\24\3\24\3\25"+
		"\3\25\3\25\3\25\5\25\u0132\n\25\3\25\3\25\3\25\3\25\7\25\u0138\n\25\f"+
		"\25\16\25\u013b\13\25\5\25\u013d\n\25\3\26\3\26\3\26\3\26\3\26\3\26\5"+
		"\26\u0145\n\26\3\27\3\27\3\27\3\27\3\27\3\27\7\27\u014d\n\27\f\27\16\27"+
		"\u0150\13\27\5\27\u0152\n\27\3\30\3\30\3\30\3\30\3\31\3\31\3\31\3\31\3"+
		"\31\3\31\3\31\3\31\7\31\u0160\n\31\f\31\16\31\u0163\13\31\3\32\3\32\3"+
		"\32\3\32\7\32\u0169\n\32\f\32\16\32\u016c\13\32\3\32\3\32\3\33\3\33\3"+
		"\33\3\33\7\33\u0174\n\33\f\33\16\33\u0177\13\33\3\33\3\33\3\34\3\34\3"+
		"\34\3\34\3\34\3\35\3\35\5\35\u0182\n\35\3\36\3\36\3\36\3\36\3\36\3\36"+
		"\5\36\u018a\n\36\3\36\3\36\3\36\3\36\3\36\5\36\u0191\n\36\3\36\3\36\3"+
		"\36\5\36\u0196\n\36\5\36\u0198\n\36\3\36\3\36\3\36\3\36\5\36\u019e\n\36"+
		"\5\36\u01a0\n\36\3\37\3\37\3\37\3\37\3\37\3\37\5\37\u01a8\n\37\3\37\3"+
		"\37\5\37\u01ac\n\37\3\37\3\37\3\37\3\37\5\37\u01b2\n\37\3\37\3\37\5\37"+
		"\u01b6\n\37\3\37\3\37\5\37\u01ba\n\37\3 \3 \3 \7 \u01bf\n \f \16 \u01c2"+
		"\13 \3 \3 \3 \7 \u01c7\n \f \16 \u01ca\13 \3 \3 \3 \5 \u01cf\n \3!\3!"+
		"\3!\5!\u01d4\n!\3!\3!\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\5\"\u01e1\n"+
		"\"\3\"\3\"\3\"\7\"\u01e6\n\"\f\"\16\"\u01e9\13\"\3#\3#\5#\u01ed\n#\3#"+
		"\5#\u01f0\n#\3$\3$\5$\u01f4\n$\3$\3$\3$\5$\u01f9\n$\7$\u01fb\n$\f$\16"+
		"$\u01fe\13$\3%\3%\3%\5%\u0203\n%\3&\3&\3&\5&\u0208\n&\3\'\3\'\3\'\3\'"+
		"\3(\3(\3(\3(\3)\3)\3)\3)\7)\u0216\n)\f)\16)\u0219\13)\3)\3)\3*\3*\3*\3"+
		"+\3+\3+\3,\3,\3,\3,\7,\u0227\n,\f,\16,\u022a\13,\3,\3,\3-\3-\5-\u0230"+
		"\n-\3.\3.\3.\3.\5.\u0236\n.\3/\3/\3/\3/\3/\7/\u023d\n/\f/\16/\u0240\13"+
		"/\3\60\3\60\3\60\3\60\3\60\3\61\3\61\3\61\3\61\3\61\3\62\3\62\3\62\3\62"+
		"\3\62\3\63\3\63\3\63\3\63\3\63\3\63\3\63\7\63\u0258\n\63\f\63\16\63\u025b"+
		"\13\63\3\64\3\64\3\64\3\64\3\64\3\64\5\64\u0263\n\64\3\64\3\64\3\64\3"+
		"\64\7\64\u0269\n\64\f\64\16\64\u026c\13\64\3\65\3\65\3\65\3\65\3\65\3"+
		"\65\3\65\7\65\u0275\n\65\f\65\16\65\u0278\13\65\3\66\3\66\3\66\3\66\3"+
		"\66\3\66\5\66\u0280\n\66\3\66\3\66\3\66\3\66\7\66\u0286\n\66\f\66\16\66"+
		"\u0289\13\66\3\67\3\67\3\67\3\67\3\67\3\67\3\67\7\67\u0292\n\67\f\67\16"+
		"\67\u0295\13\67\38\38\38\38\38\38\58\u029d\n8\38\38\38\38\78\u02a3\n8"+
		"\f8\168\u02a6\138\39\39\39\39\39\39\39\79\u02af\n9\f9\169\u02b2\139\3"+
		":\3:\3:\3:\3:\3:\3:\5:\u02bb\n:\3:\3:\3:\3:\7:\u02c1\n:\f:\16:\u02c4\13"+
		":\3;\3;\3;\3;\3;\3;\3;\7;\u02cd\n;\f;\16;\u02d0\13;\3<\3<\3<\3<\3<\3<"+
		"\5<\u02d8\n<\3<\3<\3<\3<\7<\u02de\n<\f<\16<\u02e1\13<\3=\3=\3=\3=\3=\3"+
		"=\3>\3>\3?\3?\3@\3@\3A\3A\3A\3A\3B\3B\3C\3C\3D\3D\3D\3D\3E\3E\3E\7E\u02fe"+
		"\nE\fE\16E\u0301\13E\3F\3F\3F\3F\3G\3G\3G\3G\3H\3H\3I\3I\3J\3J\3K\3K\3"+
		"L\3L\3M\3M\3N\3N\3O\3O\3P\3P\3Q\3Q\3R\3R\3S\3S\5S\u0323\nS\3T\3T\3U\3"+
		"U\3V\3V\3V\7V\u032c\nV\fV\16V\u032f\13V\3V\2W\2\4\6\b\n\f\16\20\22\24"+
		"\26\30\32\34\36 \"$&(*,.\60\62\64\668:<>@BDFHJLNPRTVXZ\\^`bdfhjlnprtv"+
		"xz|~\u0080\u0082\u0084\u0086\u0088\u008a\u008c\u008e\u0090\u0092\u0094"+
		"\u0096\u0098\u009a\u009c\u009e\u00a0\u00a2\u00a4\u00a6\u00a8\u00aa\2\25"+
		"\3\2\'(\4\2  \"%\3\2\17\20\4\2\66\66OO\3\2BC\4\2\3\3bc\4\2\5\5^^\4\2\5"+
		"\5__\3\2\4\5\4\2XXZZ\4\2YY[[\4\2OVbb\4\2MVbb\3\2IJ\3\2KL\4\2\6\6\b\b\4"+
		"\2\7\7\t\t\3\2GH\3\28A\u0330\2\u00ba\3\2\2\2\4\u00be\3\2\2\2\6\u00d5\3"+
		"\2\2\2\b\u00d9\3\2\2\2\n\u00dd\3\2\2\2\f\u00e1\3\2\2\2\16\u00e8\3\2\2"+
		"\2\20\u00f2\3\2\2\2\22\u00f7\3\2\2\2\24\u00fc\3\2\2\2\26\u0103\3\2\2\2"+
		"\30\u010a\3\2\2\2\32\u0113\3\2\2\2\34\u0116\3\2\2\2\36\u0119\3\2\2\2 "+
		"\u0122\3\2\2\2\"\u0125\3\2\2\2$\u0128\3\2\2\2&\u012b\3\2\2\2(\u012d\3"+
		"\2\2\2*\u0144\3\2\2\2,\u0146\3\2\2\2.\u0153\3\2\2\2\60\u0157\3\2\2\2\62"+
		"\u0164\3\2\2\2\64\u016f\3\2\2\2\66\u017a\3\2\2\28\u0181\3\2\2\2:\u0183"+
		"\3\2\2\2<\u01a1\3\2\2\2>\u01ce\3\2\2\2@\u01d0\3\2\2\2B\u01e0\3\2\2\2D"+
		"\u01ea\3\2\2\2F\u01f3\3\2\2\2H\u01ff\3\2\2\2J\u0204\3\2\2\2L\u0209\3\2"+
		"\2\2N\u020d\3\2\2\2P\u0211\3\2\2\2R\u021c\3\2\2\2T\u021f\3\2\2\2V\u0222"+
		"\3\2\2\2X\u022f\3\2\2\2Z\u0235\3\2\2\2\\\u0237\3\2\2\2^\u0241\3\2\2\2"+
		"`\u0246\3\2\2\2b\u024b\3\2\2\2d\u0250\3\2\2\2f\u0262\3\2\2\2h\u026d\3"+
		"\2\2\2j\u027f\3\2\2\2l\u028a\3\2\2\2n\u029c\3\2\2\2p\u02a7\3\2\2\2r\u02ba"+
		"\3\2\2\2t\u02c5\3\2\2\2v\u02d7\3\2\2\2x\u02e2\3\2\2\2z\u02e8\3\2\2\2|"+
		"\u02ea\3\2\2\2~\u02ec\3\2\2\2\u0080\u02ee\3\2\2\2\u0082\u02f2\3\2\2\2"+
		"\u0084\u02f4\3\2\2\2\u0086\u02f6\3\2\2\2\u0088\u02fa\3\2\2\2\u008a\u0302"+
		"\3\2\2\2\u008c\u0306\3\2\2\2\u008e\u030a\3\2\2\2\u0090\u030c\3\2\2\2\u0092"+
		"\u030e\3\2\2\2\u0094\u0310\3\2\2\2\u0096\u0312\3\2\2\2\u0098\u0314\3\2"+
		"\2\2\u009a\u0316\3\2\2\2\u009c\u0318\3\2\2\2\u009e\u031a\3\2\2\2\u00a0"+
		"\u031c\3\2\2\2\u00a2\u031e\3\2\2\2\u00a4\u0322\3\2\2\2\u00a6\u0324\3\2"+
		"\2\2\u00a8\u0326\3\2\2\2\u00aa\u0328\3\2\2\2\u00ac\u00bb\5\60\31\2\u00ad"+
		"\u00bb\5\66\34\2\u00ae\u00bb\58\35\2\u00af\u00bb\5(\25\2\u00b0\u00bb\5"+
		",\27\2\u00b1\u00bb\5&\24\2\u00b2\u00bb\5$\23\2\u00b3\u00bb\5\4\3\2\u00b4"+
		"\u00bb\5\f\7\2\u00b5\u00bb\5\16\b\2\u00b6\u00bb\5\24\13\2\u00b7\u00bb"+
		"\5\26\f\2\u00b8\u00bb\5\30\r\2\u00b9\u00bb\5\36\20\2\u00ba\u00ac\3\2\2"+
		"\2\u00ba\u00ad\3\2\2\2\u00ba\u00ae\3\2\2\2\u00ba\u00af\3\2\2\2\u00ba\u00b0"+
		"\3\2\2\2\u00ba\u00b1\3\2\2\2\u00ba\u00b2\3\2\2\2\u00ba\u00b3\3\2\2\2\u00ba"+
		"\u00b4\3\2\2\2\u00ba\u00b5\3\2\2\2\u00ba\u00b6\3\2\2\2\u00ba\u00b7\3\2"+
		"\2\2\u00ba\u00b8\3\2\2\2\u00ba\u00b9\3\2\2\2\u00bb\u00bc\3\2\2\2\u00bc"+
		"\u00bd\7\2\2\3\u00bd\3\3\2\2\2\u00be\u00d3\7\23\2\2\u00bf\u00c0\7(\2\2"+
		"\u00c0\u00c1\7\4\2\2\u00c1\u00cc\5\6\4\2\u00c2\u00c4\5\n\6\2\u00c3\u00c5"+
		"\5\b\5\2\u00c4\u00c3\3\2\2\2\u00c4\u00c5\3\2\2\2\u00c5\u00cb\3\2\2\2\u00c6"+
		"\u00c8\5\b\5\2\u00c7\u00c9\5\n\6\2\u00c8\u00c7\3\2\2\2\u00c8\u00c9\3\2"+
		"\2\2\u00c9\u00cb\3\2\2\2\u00ca\u00c2\3\2\2\2\u00ca\u00c6\3\2\2\2\u00cb"+
		"\u00cd\3\2\2\2\u00cc\u00ca\3\2\2\2\u00cc\u00cd\3\2\2\2\u00cd\u00d4\3\2"+
		"\2\2\u00ce\u00cf\7\'\2\2\u00cf\u00d1\7\4\2\2\u00d0\u00d2\5\b\5\2\u00d1"+
		"\u00d0\3\2\2\2\u00d1\u00d2\3\2\2\2\u00d2\u00d4\3\2\2\2\u00d3\u00bf\3\2"+
		"\2\2\u00d3\u00ce\3\2\2\2\u00d4\5\3\2\2\2\u00d5\u00d6\7\67\2\2\u00d6\u00d7"+
		"\7\34\2\2\u00d7\u00d8\7\4\2\2\u00d8\7\3\2\2\2\u00d9\u00da\7\67\2\2\u00da"+
		"\u00db\7#\2\2\u00db\u00dc\5\u00aaV\2\u00dc\t\3\2\2\2\u00dd\u00de\7\67"+
		"\2\2\u00de\u00df\7$\2\2\u00df\u00e0\5\u00aaV\2\u00e0\13\3\2\2\2\u00e1"+
		"\u00e6\7\24\2\2\u00e2\u00e3\t\2\2\2\u00e3\u00e7\7\4\2\2\u00e4\u00e5\7"+
		"!\2\2\u00e5\u00e7\5z>\2\u00e6\u00e2\3\2\2\2\u00e6\u00e4\3\2\2\2\u00e7"+
		"\r\3\2\2\2\u00e8\u00f0\7\25\2\2\u00e9\u00ea\7(\2\2\u00ea\u00eb\7\4\2\2"+
		"\u00eb\u00f1\5\20\t\2\u00ec\u00ed\7!\2\2\u00ed\u00ee\5z>\2\u00ee\u00ef"+
		"\5\22\n\2\u00ef\u00f1\3\2\2\2\u00f0\u00e9\3\2\2\2\u00f0\u00ec\3\2\2\2"+
		"\u00f1\17\3\2\2\2\u00f2\u00f3\7/\2\2\u00f3\u00f4\7\34\2\2\u00f4\u00f5"+
		"\7W\2\2\u00f5\u00f6\7\4\2\2\u00f6\21\3\2\2\2\u00f7\u00f8\7/\2\2\u00f8"+
		"\u00f9\7\35\2\2\u00f9\u00fa\7W\2\2\u00fa\u00fb\5\u00a6T\2\u00fb\23\3\2"+
		"\2\2\u00fc\u00fd\7\26\2\2\u00fd\u00fe\7#\2\2\u00fe\u00ff\5\u00aaV\2\u00ff"+
		"\u0100\7,\2\2\u0100\u0101\t\2\2\2\u0101\u0102\7\4\2\2\u0102\25\3\2\2\2"+
		"\u0103\u0104\7\27\2\2\u0104\u0105\7#\2\2\u0105\u0106\5\u00aaV\2\u0106"+
		"\u0107\7)\2\2\u0107\u0108\t\2\2\2\u0108\u0109\7\4\2\2\u0109\27\3\2\2\2"+
		"\u010a\u010d\7\30\2\2\u010b\u010e\5\32\16\2\u010c\u010e\5\34\17\2\u010d"+
		"\u010b\3\2\2\2\u010d\u010c\3\2\2\2\u010e\u010f\3\2\2\2\u010f\u0110\7,"+
		"\2\2\u0110\u0111\7(\2\2\u0111\u0112\7\4\2\2\u0112\31\3\2\2\2\u0113\u0114"+
		"\7\'\2\2\u0114\u0115\7\4\2\2\u0115\33\3\2\2\2\u0116\u0117\7$\2\2\u0117"+
		"\u0118\5\u00aaV\2\u0118\35\3\2\2\2\u0119\u011c\7\31\2\2\u011a\u011d\5"+
		" \21\2\u011b\u011d\5\"\22\2\u011c\u011a\3\2\2\2\u011c\u011b\3\2\2\2\u011d"+
		"\u011e\3\2\2\2\u011e\u011f\7)\2\2\u011f\u0120\7(\2\2\u0120\u0121\7\4\2"+
		"\2\u0121\37\3\2\2\2\u0122\u0123\7\'\2\2\u0123\u0124\7\4\2\2\u0124!\3\2"+
		"\2\2\u0125\u0126\7$\2\2\u0126\u0127\5\u00aaV\2\u0127#\3\2\2\2\u0128\u0129"+
		"\7\13\2\2\u0129\u012a\t\3\2\2\u012a%\3\2\2\2\u012b\u012c\7\22\2\2\u012c"+
		"\'\3\2\2\2\u012d\u0131\t\4\2\2\u012e\u0132\5z>\2\u012f\u0130\7)\2\2\u0130"+
		"\u0132\7\4\2\2\u0131\u012e\3\2\2\2\u0131\u012f\3\2\2\2\u0132\u013c\3\2"+
		"\2\2\u0133\u0134\7/\2\2\u0134\u0139\5*\26\2\u0135\u0136\7`\2\2\u0136\u0138"+
		"\5*\26\2\u0137\u0135\3\2\2\2\u0138\u013b\3\2\2\2\u0139\u0137\3\2\2\2\u0139"+
		"\u013a\3\2\2\2\u013a\u013d\3\2\2\2\u013b\u0139\3\2\2\2\u013c\u0133\3\2"+
		"\2\2\u013c\u013d\3\2\2\2\u013d)\3\2\2\2\u013e\u013f\7\32\2\2\u013f\u0140"+
		"\7W\2\2\u0140\u0145\5\u00a6T\2\u0141\u0142\7\33\2\2\u0142\u0143\7W\2\2"+
		"\u0143\u0145\5\u00a6T\2\u0144\u013e\3\2\2\2\u0144\u0141\3\2\2\2\u0145"+
		"+\3\2\2\2\u0146\u0147\7\21\2\2\u0147\u0151\5z>\2\u0148\u0149\7/\2\2\u0149"+
		"\u014e\5.\30\2\u014a\u014b\7`\2\2\u014b\u014d\5.\30\2\u014c\u014a\3\2"+
		"\2\2\u014d\u0150\3\2\2\2\u014e\u014c\3\2\2\2\u014e\u014f\3\2\2\2\u014f"+
		"\u0152\3\2\2\2\u0150\u014e\3\2\2\2\u0151\u0148\3\2\2\2\u0151\u0152\3\2"+
		"\2\2\u0152-\3\2\2\2\u0153\u0154\7\32\2\2\u0154\u0155\7W\2\2\u0155\u0156"+
		"\5\u00a6T\2\u0156/\3\2\2\2\u0157\u0158\7\r\2\2\u0158\u0159\7.\2\2\u0159"+
		"\u015a\5z>\2\u015a\u015b\5\62\32\2\u015b\u015c\7\60\2\2\u015c\u0161\5"+
		"\64\33\2\u015d\u015e\7`\2\2\u015e\u0160\5\64\33\2\u015f\u015d\3\2\2\2"+
		"\u0160\u0163\3\2\2\2\u0161\u015f\3\2\2\2\u0161\u0162\3\2\2\2\u0162\61"+
		"\3\2\2\2\u0163\u0161\3\2\2\2\u0164\u0165\7X\2\2\u0165\u016a\5X-\2\u0166"+
		"\u0167\7`\2\2\u0167\u0169\5X-\2\u0168\u0166\3\2\2\2\u0169\u016c\3\2\2"+
		"\2\u016a\u0168\3\2\2\2\u016a\u016b\3\2\2\2\u016b\u016d\3\2\2\2\u016c\u016a"+
		"\3\2\2\2\u016d\u016e\7Y\2\2\u016e\63\3\2\2\2\u016f\u0170\7X\2\2\u0170"+
		"\u0175\5Z.\2\u0171\u0172\7`\2\2\u0172\u0174\5Z.\2\u0173\u0171\3\2\2\2"+
		"\u0174\u0177\3\2\2\2\u0175\u0173\3\2\2\2\u0175\u0176\3\2\2\2\u0176\u0178"+
		"\3\2\2\2\u0177\u0175\3\2\2\2\u0178\u0179\7Y\2\2\u0179\65\3\2\2\2\u017a"+
		"\u017b\7\16\2\2\u017b\u017c\5\u0088E\2\u017c\u017d\7)\2\2\u017d\u017e"+
		"\5z>\2\u017e\67\3\2\2\2\u017f\u0182\5:\36\2\u0180\u0182\5<\37\2\u0181"+
		"\u017f\3\2\2\2\u0181\u0180\3\2\2\2\u01829\3\2\2\2\u0183\u0189\7\f\2\2"+
		"\u0184\u018a\7\37\2\2\u0185\u0186\t\5\2\2\u0186\u0187\7X\2\2\u0187\u0188"+
		"\7\37\2\2\u0188\u018a\7Y\2\2\u0189\u0184\3\2\2\2\u0189\u0185\3\2\2\2\u018a"+
		"\u018b\3\2\2\2\u018b\u018c\7)\2\2\u018c\u0190\5z>\2\u018d\u018e\5\u00a8"+
		"U\2\u018e\u018f\5@!\2\u018f\u0191\3\2\2\2\u0190\u018d\3\2\2\2\u0190\u0191"+
		"\3\2\2\2\u0191\u0197\3\2\2\2\u0192\u0195\7\63\2\2\u0193\u0196\5B\"\2\u0194"+
		"\u0196\5\\/\2\u0195\u0193\3\2\2\2\u0195\u0194\3\2\2\2\u0196\u0198\3\2"+
		"\2\2\u0197\u0192\3\2\2\2\u0197\u0198\3\2\2\2\u0198\u019f\3\2\2\2\u0199"+
		"\u019a\7\64\2\2\u019a\u019d\5\u0082B\2\u019b\u019c\7`\2\2\u019c\u019e"+
		"\5\u0084C\2\u019d\u019b\3\2\2\2\u019d\u019e\3\2\2\2\u019e\u01a0\3\2\2"+
		"\2\u019f\u0199\3\2\2\2\u019f\u01a0\3\2\2\2\u01a0;\3\2\2\2\u01a1\u01a7"+
		"\7\f\2\2\u01a2\u01a8\7\36\2\2\u01a3\u01a4\7\65\2\2\u01a4\u01a5\7X\2\2"+
		"\u01a5\u01a6\7\36\2\2\u01a6\u01a8\7Y\2\2\u01a7\u01a2\3\2\2\2\u01a7\u01a3"+
		"\3\2\2\2\u01a8\u01ab\3\2\2\2\u01a9\u01aa\7*\2\2\u01aa\u01ac\5> \2\u01ab"+
		"\u01a9\3\2\2\2\u01ab\u01ac\3\2\2\2\u01ac\u01ad\3\2\2\2\u01ad\u01ae\7)"+
		"\2\2\u01ae\u01b1\5z>\2\u01af\u01b0\7-\2\2\u01b0\u01b2\5@!\2\u01b1\u01af"+
		"\3\2\2\2\u01b1\u01b2\3\2\2\2\u01b2\u01b5\3\2\2\2\u01b3\u01b4\7\63\2\2"+
		"\u01b4\u01b6\5B\"\2\u01b5\u01b3\3\2\2\2\u01b5\u01b6\3\2\2\2\u01b6\u01b9"+
		"\3\2\2\2\u01b7\u01b8\7\62\2\2\u01b8\u01ba\5D#\2\u01b9\u01b7\3\2\2\2\u01b9"+
		"\u01ba\3\2\2\2\u01ba=\3\2\2\2\u01bb\u01c0\5H%\2\u01bc\u01bd\7`\2\2\u01bd"+
		"\u01bf\5H%\2\u01be\u01bc\3\2\2\2\u01bf\u01c2\3\2\2\2\u01c0\u01be\3\2\2"+
		"\2\u01c0\u01c1\3\2\2\2\u01c1\u01cf\3\2\2\2\u01c2\u01c0\3\2\2\2\u01c3\u01c8"+
		"\5J&\2\u01c4\u01c5\7`\2\2\u01c5\u01c7\5J&\2\u01c6\u01c4\3\2\2\2\u01c7"+
		"\u01ca\3\2\2\2\u01c8\u01c6\3\2\2\2\u01c8\u01c9\3\2\2\2\u01c9\u01cb\3\2"+
		"\2\2\u01ca\u01c8\3\2\2\2\u01cb\u01cc\7+\2\2\u01cc\u01cd\5x=\2\u01cd\u01cf"+
		"\3\2\2\2\u01ce\u01bb\3\2\2\2\u01ce\u01c3\3\2\2\2\u01cf?\3\2\2\2\u01d0"+
		"\u01d3\5\u0096L\2\u01d1\u01d4\5\u0080A\2\u01d2\u01d4\5\u0086D\2\u01d3"+
		"\u01d1\3\2\2\2\u01d3\u01d2\3\2\2\2\u01d4\u01d5\3\2\2\2\u01d5\u01d6\5\u0098"+
		"M\2\u01d6A\3\2\2\2\u01d7\u01d8\b\"\1\2\u01d8\u01d9\7D\2\2\u01d9\u01e1"+
		"\5B\"\2\u01da\u01e1\5L\'\2\u01db\u01e1\5N(\2\u01dc\u01dd\7X\2\2\u01dd"+
		"\u01de\5B\"\2\u01de\u01df\7Y\2\2\u01df\u01e1\3\2\2\2\u01e0\u01d7\3\2\2"+
		"\2\u01e0\u01da\3\2\2\2\u01e0\u01db\3\2\2\2\u01e0\u01dc\3\2\2\2\u01e1\u01e7"+
		"\3\2\2\2\u01e2\u01e3\6\"\2\3\u01e3\u01e4\t\6\2\2\u01e4\u01e6\5B\"\2\u01e5"+
		"\u01e2\3\2\2\2\u01e6\u01e9\3\2\2\2\u01e7\u01e5\3\2\2\2\u01e7\u01e8\3\2"+
		"\2\2\u01e8C\3\2\2\2\u01e9\u01e7\3\2\2\2\u01ea\u01ec\5F$\2\u01eb\u01ed"+
		"\5R*\2\u01ec\u01eb\3\2\2\2\u01ec\u01ed\3\2\2\2\u01ed\u01ef\3\2\2\2\u01ee"+
		"\u01f0\5T+\2\u01ef\u01ee\3\2\2\2\u01ef\u01f0\3\2\2\2\u01f0E\3\2\2\2\u01f1"+
		"\u01f4\5x=\2\u01f2\u01f4\5|?\2\u01f3\u01f1\3\2\2\2\u01f3\u01f2\3\2\2\2"+
		"\u01f4\u01fc\3\2\2\2\u01f5\u01f8\7`\2\2\u01f6\u01f9\5x=\2\u01f7\u01f9"+
		"\5|?\2\u01f8\u01f6\3\2\2\2\u01f8\u01f7\3\2\2\2\u01f9\u01fb\3\2\2\2\u01fa"+
		"\u01f5\3\2\2\2\u01fb\u01fe\3\2\2\2\u01fc\u01fa\3\2\2\2\u01fc\u01fd\3\2"+
		"\2\2\u01fdG\3\2\2\2\u01fe\u01fc\3\2\2\2\u01ff\u0202\5d\63\2\u0200\u0201"+
		"\7\61\2\2\u0201\u0203\5~@\2\u0202\u0200\3\2\2\2\u0202\u0203\3\2\2\2\u0203"+
		"I\3\2\2\2\u0204\u0207\5p9\2\u0205\u0206\7\61\2\2\u0206\u0208\5~@\2\u0207"+
		"\u0205\3\2\2\2\u0207\u0208\3\2\2\2\u0208K\3\2\2\2\u0209\u020a\5x=\2\u020a"+
		"\u020b\7W\2\2\u020b\u020c\5\u0094K\2\u020cM\3\2\2\2\u020d\u020e\5|?\2"+
		"\u020e\u020f\7W\2\2\u020f\u0210\5\u0094K\2\u0210O\3\2\2\2\u0211\u0212"+
		"\7X\2\2\u0212\u0217\5\u0094K\2\u0213\u0214\7`\2\2\u0214\u0216\5\u0094"+
		"K\2\u0215\u0213\3\2\2\2\u0216\u0219\3\2\2\2\u0217\u0215\3\2\2\2\u0217"+
		"\u0218\3\2\2\2\u0218\u021a\3\2\2\2\u0219\u0217\3\2\2\2\u021a\u021b\7Y"+
		"\2\2\u021bQ\3\2\2\2\u021c\u021d\7E\2\2\u021d\u021e\5V,\2\u021eS\3\2\2"+
		"\2\u021f\u0220\7F\2\2\u0220\u0221\5V,\2\u0221U\3\2\2\2\u0222\u0223\7\\"+
		"\2\2\u0223\u0228\5P)\2\u0224\u0225\7`\2\2\u0225\u0227\5P)\2\u0226\u0224"+
		"\3\2\2\2\u0227\u022a\3\2\2\2\u0228\u0226\3\2\2\2\u0228\u0229\3\2\2\2\u0229"+
		"\u022b\3\2\2\2\u022a\u0228\3\2\2\2\u022b\u022c\7]\2\2\u022cW\3\2\2\2\u022d"+
		"\u0230\5\u00a4S\2\u022e\u0230\5|?\2\u022f\u022d\3\2\2\2\u022f\u022e\3"+
		"\2\2\2\u0230Y\3\2\2\2\u0231\u0236\5\u0092J\2\u0232\u0236\5\u008eH\2\u0233"+
		"\u0236\5\u0090I\2\u0234\u0236\5\u0094K\2\u0235\u0231\3\2\2\2\u0235\u0232"+
		"\3\2\2\2\u0235\u0233\3\2\2\2\u0235\u0234\3\2\2\2\u0236[\3\2\2\2\u0237"+
		"\u0238\7\n\2\2\u0238\u0239\7W\2\2\u0239\u023e\7_\2\2\u023a\u023b\7`\2"+
		"\2\u023b\u023d\7_\2\2\u023c\u023a\3\2\2\2\u023d\u0240\3\2\2\2\u023e\u023c"+
		"\3\2\2\2\u023e\u023f\3\2\2\2\u023f]\3\2\2\2\u0240\u023e\3\2\2\2\u0241"+
		"\u0242\5\u009cO\2\u0242\u0243\7X\2\2\u0243\u0244\5t;\2\u0244\u0245\7Y"+
		"\2\2\u0245_\3\2\2\2\u0246\u0247\5\u009eP\2\u0247\u0248\7X\2\2\u0248\u0249"+
		"\5t;\2\u0249\u024a\7Y\2\2\u024aa\3\2\2\2\u024b\u024c\5\u009aN\2\u024c"+
		"\u024d\7X\2\2\u024d\u024e\5d\63\2\u024e\u024f\7Y\2\2\u024fc\3\2\2\2\u0250"+
		"\u0251\b\63\1\2\u0251\u0252\5f\64\2\u0252\u0259\3\2\2\2\u0253\u0254\6"+
		"\63\3\3\u0254\u0255\5\u00a2R\2\u0255\u0256\5f\64\2\u0256\u0258\3\2\2\2"+
		"\u0257\u0253\3\2\2\2\u0258\u025b\3\2\2\2\u0259\u0257\3\2\2\2\u0259\u025a"+
		"\3\2\2\2\u025ae\3\2\2\2\u025b\u0259\3\2\2\2\u025c\u025d\b\64\1\2\u025d"+
		"\u0263\5`\61\2\u025e\u025f\7X\2\2\u025f\u0260\5d\63\2\u0260\u0261\7Y\2"+
		"\2\u0261\u0263\3\2\2\2\u0262\u025c\3\2\2\2\u0262\u025e\3\2\2\2\u0263\u026a"+
		"\3\2\2\2\u0264\u0265\6\64\4\3\u0265\u0266\5\u00a0Q\2\u0266\u0267\5f\64"+
		"\2\u0267\u0269\3\2\2\2\u0268\u0264\3\2\2\2\u0269\u026c\3\2\2\2\u026a\u0268"+
		"\3\2\2\2\u026a\u026b\3\2\2\2\u026bg\3\2\2\2\u026c\u026a\3\2\2\2\u026d"+
		"\u026e\b\65\1\2\u026e\u026f\5j\66\2\u026f\u0276\3\2\2\2\u0270\u0271\6"+
		"\65\5\3\u0271\u0272\5\u00a2R\2\u0272\u0273\5j\66\2\u0273\u0275\3\2\2\2"+
		"\u0274\u0270\3\2\2\2\u0275\u0278\3\2\2\2\u0276\u0274\3\2\2\2\u0276\u0277"+
		"\3\2\2\2\u0277i\3\2\2\2\u0278\u0276\3\2\2\2\u0279\u027a\b\66\1\2\u027a"+
		"\u0280\5b\62\2\u027b\u027c\7X\2\2\u027c\u027d\5h\65\2\u027d\u027e\7Y\2"+
		"\2\u027e\u0280\3\2\2\2\u027f\u0279\3\2\2\2\u027f\u027b\3\2\2\2\u0280\u0287"+
		"\3\2\2\2\u0281\u0282\6\66\6\3\u0282\u0283\5\u00a0Q\2\u0283\u0284\5j\66"+
		"\2\u0284\u0286\3\2\2\2\u0285\u0281\3\2\2\2\u0286\u0289\3\2\2\2\u0287\u0285"+
		"\3\2\2\2\u0287\u0288\3\2\2\2\u0288k\3\2\2\2\u0289\u0287\3\2\2\2\u028a"+
		"\u028b\b\67\1\2\u028b\u028c\5n8\2\u028c\u0293\3\2\2\2\u028d\u028e\6\67"+
		"\7\3\u028e\u028f\5\u00a2R\2\u028f\u0290\5n8\2\u0290\u0292\3\2\2\2\u0291"+
		"\u028d\3\2\2\2\u0292\u0295\3\2\2\2\u0293\u0291\3\2\2\2\u0293\u0294\3\2"+
		"\2\2\u0294m\3\2\2\2\u0295\u0293\3\2\2\2\u0296\u0297\b8\1\2\u0297\u029d"+
		"\5^\60\2\u0298\u0299\7X\2\2\u0299\u029a\5l\67\2\u029a\u029b\7Y\2\2\u029b"+
		"\u029d\3\2\2\2\u029c\u0296\3\2\2\2\u029c\u0298\3\2\2\2\u029d\u02a4\3\2"+
		"\2\2\u029e\u029f\68\b\3\u029f\u02a0\5\u00a0Q\2\u02a0\u02a1\5n8\2\u02a1"+
		"\u02a3\3\2\2\2\u02a2\u029e\3\2\2\2\u02a3\u02a6\3\2\2\2\u02a4\u02a2\3\2"+
		"\2\2\u02a4\u02a5\3\2\2\2\u02a5o\3\2\2\2\u02a6\u02a4\3\2\2\2\u02a7\u02a8"+
		"\b9\1\2\u02a8\u02a9\5r:\2\u02a9\u02b0\3\2\2\2\u02aa\u02ab\69\t\3\u02ab"+
		"\u02ac\5\u00a2R\2\u02ac\u02ad\5r:\2\u02ad\u02af\3\2\2\2\u02ae\u02aa\3"+
		"\2\2\2\u02af\u02b2\3\2\2\2\u02b0\u02ae\3\2\2\2\u02b0\u02b1\3\2\2\2\u02b1"+
		"q\3\2\2\2\u02b2\u02b0\3\2\2\2\u02b3\u02b4\b:\1\2\u02b4\u02bb\5h\65\2\u02b5"+
		"\u02bb\5l\67\2\u02b6\u02b7\7X\2\2\u02b7\u02b8\5r:\2\u02b8\u02b9\7Y\2\2"+
		"\u02b9\u02bb\3\2\2\2\u02ba\u02b3\3\2\2\2\u02ba\u02b5\3\2\2\2\u02ba\u02b6"+
		"\3\2\2\2\u02bb\u02c2\3\2\2\2\u02bc\u02bd\6:\n\3\u02bd\u02be\5\u00a0Q\2"+
		"\u02be\u02bf\5r:\2\u02bf\u02c1\3\2\2\2\u02c0\u02bc\3\2\2\2\u02c1\u02c4"+
		"\3\2\2\2\u02c2\u02c0\3\2\2\2\u02c2\u02c3\3\2\2\2\u02c3s\3\2\2\2\u02c4"+
		"\u02c2\3\2\2\2\u02c5\u02c6\b;\1\2\u02c6\u02c7\5v<\2\u02c7\u02ce\3\2\2"+
		"\2\u02c8\u02c9\6;\13\3\u02c9\u02ca\5\u00a2R\2\u02ca\u02cb\5v<\2\u02cb"+
		"\u02cd\3\2\2\2\u02cc\u02c8\3\2\2\2\u02cd\u02d0\3\2\2\2\u02ce\u02cc\3\2"+
		"\2\2\u02ce\u02cf\3\2\2\2\u02cfu\3\2\2\2\u02d0\u02ce\3\2\2\2\u02d1\u02d2"+
		"\b<\1\2\u02d2\u02d8\5|?\2\u02d3\u02d4\7X\2\2\u02d4\u02d5\5t;\2\u02d5\u02d6"+
		"\7Y\2\2\u02d6\u02d8\3\2\2\2\u02d7\u02d1\3\2\2\2\u02d7\u02d3\3\2\2\2\u02d8"+
		"\u02df\3\2\2\2\u02d9\u02da\6<\f\3\u02da\u02db\5\u00a0Q\2\u02db\u02dc\5"+
		"v<\2\u02dc\u02de\3\2\2\2\u02dd\u02d9\3\2\2\2\u02de\u02e1\3\2\2\2\u02df"+
		"\u02dd\3\2\2\2\u02df\u02e0\3\2\2\2\u02e0w\3\2\2\2\u02e1\u02df\3\2\2\2"+
		"\u02e2\u02e3\t\7\2\2\u02e3\u02e4\7a\2\2\u02e4\u02e5\t\7\2\2\u02e5\u02e6"+
		"\7a\2\2\u02e6\u02e7\t\7\2\2\u02e7y\3\2\2\2\u02e8\u02e9\t\7\2\2\u02e9{"+
		"\3\2\2\2\u02ea\u02eb\t\7\2\2\u02eb}\3\2\2\2\u02ec\u02ed\t\7\2\2\u02ed"+
		"\177\3\2\2\2\u02ee\u02ef\7^\2\2\u02ef\u02f0\7`\2\2\u02f0\u02f1\7^\2\2"+
		"\u02f1\u0081\3\2\2\2\u02f2\u02f3\7_\2\2\u02f3\u0083\3\2\2\2\u02f4\u02f5"+
		"\7_\2\2\u02f5\u0085\3\2\2\2\u02f6\u02f7\7_\2\2\u02f7\u02f8\7`\2\2\u02f8"+
		"\u02f9\7_\2\2\u02f9\u0087\3\2\2\2\u02fa\u02ff\7_\2\2\u02fb\u02fc\7`\2"+
		"\2\u02fc\u02fe\7_\2\2\u02fd\u02fb\3\2\2\2\u02fe\u0301\3\2\2\2\u02ff\u02fd"+
		"\3\2\2\2\u02ff\u0300\3\2\2\2\u0300\u0089\3\2\2\2\u0301\u02ff\3\2\2\2\u0302"+
		"\u0303\t\b\2\2\u0303\u0304\7`\2\2\u0304\u0305\t\b\2\2\u0305\u008b\3\2"+
		"\2\2\u0306\u0307\t\t\2\2\u0307\u0308\7`\2\2\u0308\u0309\t\t\2\2\u0309"+
		"\u008d\3\2\2\2\u030a\u030b\7^\2\2\u030b\u008f\3\2\2\2\u030c\u030d\7_\2"+
		"\2\u030d\u0091\3\2\2\2\u030e\u030f\7\5\2\2\u030f\u0093\3\2\2\2\u0310\u0311"+
		"\t\n\2\2\u0311\u0095\3\2\2\2\u0312\u0313\t\13\2\2\u0313\u0097\3\2\2\2"+
		"\u0314\u0315\t\f\2\2\u0315\u0099\3\2\2\2\u0316\u0317\t\r\2\2\u0317\u009b"+
		"\3\2\2\2\u0318\u0319\t\r\2\2\u0319\u009d\3\2\2\2\u031a\u031b\t\16\2\2"+
		"\u031b\u009f\3\2\2\2\u031c\u031d\t\17\2\2\u031d\u00a1\3\2\2\2\u031e\u031f"+
		"\t\20\2\2\u031f\u00a3\3\2\2\2\u0320\u0323\t\21\2\2\u0321\u0323\t\22\2"+
		"\2\u0322\u0320\3\2\2\2\u0322\u0321\3\2\2\2\u0323\u00a5\3\2\2\2\u0324\u0325"+
		"\t\23\2\2\u0325\u00a7\3\2\2\2\u0326\u0327\t\24\2\2\u0327\u00a9\3\2\2\2"+
		"\u0328\u032d\7\4\2\2\u0329\u032a\7`\2\2\u032a\u032c\7\4\2\2\u032b\u0329"+
		"\3\2\2\2\u032c\u032f\3\2\2\2\u032d\u032b\3\2\2\2\u032d\u032e\3\2\2\2\u032e"+
		"\u00ab\3\2\2\2\u032f\u032d\3\2\2\2F\u00ba\u00c4\u00c8\u00ca\u00cc\u00d1"+
		"\u00d3\u00e6\u00f0\u010d\u011c\u0131\u0139\u013c\u0144\u014e\u0151\u0161"+
		"\u016a\u0175\u0181\u0189\u0190\u0195\u0197\u019d\u019f\u01a7\u01ab\u01b1"+
		"\u01b5\u01b9\u01c0\u01c8\u01ce\u01d3\u01e0\u01e7\u01ec\u01ef\u01f3\u01f8"+
		"\u01fc\u0202\u0207\u0217\u0228\u022f\u0235\u023e\u0259\u0262\u026a\u0276"+
		"\u027f\u0287\u0293\u029c\u02a4\u02b0\u02ba\u02c2\u02ce\u02d7\u02df\u02ff"+
		"\u0322\u032d";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}