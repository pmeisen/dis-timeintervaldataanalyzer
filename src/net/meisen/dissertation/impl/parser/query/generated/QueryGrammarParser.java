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
		POS_START_EXCL=6, POS_END_EXCL=7, STMT_GET=8, STMT_SELECT=9, STMT_INSERT=10, 
		STMT_DELETE=11, STMT_OPEN=12, STMT_LOAD=13, STMT_UNLOAD=14, STMT_ALIVE=15, 
		STMT_ADD=16, STMT_DROP=17, STMT_MODIFY=18, STMT_GRANT=19, STMT_REVOKE=20, 
		STMT_ASSIGN=21, STMT_REMOVE=22, PROP_AUTOLOAD=23, PROP_FORCE=24, PROP_PASSWORD=25, 
		PROP_BULKLOAD=26, TYPE_TIMESERIES=27, TYPE_RECORDS=28, TYPE_MODELS=29, 
		TYPE_MODEL=30, TYPE_VERSION=31, TYPE_PERMISSIONS=32, TYPE_ROLES=33, TYPE_USERS=34, 
		TYPE_PERMISSION=35, TYPE_ROLE=36, TYPE_USER=37, OP_FROM=38, OP_OF=39, 
		OP_ON=40, OP_TO=41, OP_IN=42, OP_INTO=43, OP_SET=44, OP_VALUES=45, OP_ALIAS=46, 
		OP_GROUPBY=47, OP_FILTERBY=48, OP_LIMIT=49, OP_TRANSPOSE=50, OP_IDONLY=51, 
		OP_WITH=52, IR_EQUALTO=53, IR_BEFORE=54, IR_AFTER=55, IR_MEETING=56, IR_OVERLAPPING=57, 
		IR_DURING=58, IR_WITHIN=59, IR_CONTAINING=60, IR_STARTINGWITH=61, IR_FINISHINGWITH=62, 
		LOGICAL_OR=63, LOGICAL_AND=64, LOGICAL_NOT=65, LOGICAL_INCLUDE=66, LOGICAL_EXCLUDE=67, 
		LOGICAL_TRUE=68, LOGICAL_FALSE=69, MATH_MULTIPLY=70, MATH_DIVISION=71, 
		MATH_PLUS=72, MATH_MINUS=73, AGGR_COUNTSTARTED=74, AGGR_COUNTFINISHED=75, 
		AGGR_COUNT=76, AGGR_SUM=77, AGGR_MIN=78, AGGR_MAX=79, AGGR_AVERAGE=80, 
		AGGR_MODE=81, AGGR_MEAN=82, AGGR_MEDIAN=83, CMP_EQUAL=84, BRACKET_ROUND_OPENED=85, 
		BRACKET_ROUND_CLOSED=86, BRACKET_SQUARE_OPENED=87, BRACKET_SQUARE_CLOSED=88, 
		BRACKET_CURLY_OPENED=89, BRACKET_CURLY_CLOSED=90, DATE=91, INT=92, SEPARATOR=93, 
		DIMSEPARATOR=94, SIMPLE_ID=95, ENHANCED_ID=96, WHITESPACE=97;
	public static final String[] tokenNames = {
		"<INVALID>", "MARKED_ID", "VALUE", "NULL_VALUE", "POS_START_INCL", "POS_END_INCL", 
		"POS_START_EXCL", "POS_END_EXCL", "STMT_GET", "STMT_SELECT", "STMT_INSERT", 
		"STMT_DELETE", "STMT_OPEN", "STMT_LOAD", "STMT_UNLOAD", "STMT_ALIVE", 
		"STMT_ADD", "STMT_DROP", "STMT_MODIFY", "STMT_GRANT", "STMT_REVOKE", "STMT_ASSIGN", 
		"STMT_REMOVE", "PROP_AUTOLOAD", "PROP_FORCE", "PROP_PASSWORD", "PROP_BULKLOAD", 
		"TYPE_TIMESERIES", "TYPE_RECORDS", "TYPE_MODELS", "TYPE_MODEL", "TYPE_VERSION", 
		"TYPE_PERMISSIONS", "TYPE_ROLES", "TYPE_USERS", "TYPE_PERMISSION", "TYPE_ROLE", 
		"TYPE_USER", "OP_FROM", "OP_OF", "OP_ON", "OP_TO", "OP_IN", "OP_INTO", 
		"OP_SET", "OP_VALUES", "OP_ALIAS", "OP_GROUPBY", "OP_FILTERBY", "OP_LIMIT", 
		"OP_TRANSPOSE", "OP_IDONLY", "OP_WITH", "IR_EQUALTO", "IR_BEFORE", "IR_AFTER", 
		"IR_MEETING", "IR_OVERLAPPING", "IR_DURING", "IR_WITHIN", "IR_CONTAINING", 
		"IR_STARTINGWITH", "IR_FINISHINGWITH", "LOGICAL_OR", "LOGICAL_AND", "LOGICAL_NOT", 
		"LOGICAL_INCLUDE", "LOGICAL_EXCLUDE", "LOGICAL_TRUE", "LOGICAL_FALSE", 
		"'*'", "'/'", "'+'", "'-'", "AGGR_COUNTSTARTED", "AGGR_COUNTFINISHED", 
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
		RULE_compValueElement = 44, RULE_compDimAggrFunction = 45, RULE_compLowAggrFunction = 46, 
		RULE_compMathAggrFunction = 47, RULE_compLowMeasure = 48, RULE_compLowMeasureAtom = 49, 
		RULE_compMathMeasure = 50, RULE_compMathMeasureAtom = 51, RULE_compDimMeasure = 52, 
		RULE_compDimMeasureAtom = 53, RULE_compDimMathMeasure = 54, RULE_compDimMathMeasureAtom = 55, 
		RULE_compDescriptorFormula = 56, RULE_compDescriptorFormulaAtom = 57, 
		RULE_selectorMember = 58, RULE_selectorModelId = 59, RULE_selectorDescriptorId = 60, 
		RULE_selectorAlias = 61, RULE_selectorDateInterval = 62, RULE_selectorOffset = 63, 
		RULE_selectorLimit = 64, RULE_selectorIntInterval = 65, RULE_selectorIntIdList = 66, 
		RULE_selectorDateIntervalWithNull = 67, RULE_selectorIntIntervalWithNull = 68, 
		RULE_selectorDateValue = 69, RULE_selectorIntValue = 70, RULE_selectorNullValue = 71, 
		RULE_selectorValue = 72, RULE_selectorOpenInterval = 73, RULE_selectorCloseInterval = 74, 
		RULE_selectorMathAggrFunctionName = 75, RULE_selectorDimAggrFunctionName = 76, 
		RULE_selectorLowAggrFunctionName = 77, RULE_selectorFirstMathOperator = 78, 
		RULE_selectorSecondMathOperator = 79, RULE_selectorIntervalDef = 80, RULE_selectorBoolean = 81, 
		RULE_selectorIntervalRelation = 82, RULE_selectorValueList = 83;
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
		"compDimAggrFunction", "compLowAggrFunction", "compMathAggrFunction", 
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
			setState(182);
			switch (_input.LA(1)) {
			case STMT_INSERT:
				{
				setState(168); exprInsert();
				}
				break;
			case STMT_DELETE:
				{
				setState(169); exprDelete();
				}
				break;
			case STMT_SELECT:
				{
				setState(170); exprSelect();
				}
				break;
			case STMT_OPEN:
			case STMT_LOAD:
				{
				setState(171); exprLoad();
				}
				break;
			case STMT_UNLOAD:
				{
				setState(172); exprUnload();
				}
				break;
			case STMT_ALIVE:
				{
				setState(173); exprAlive();
				}
				break;
			case STMT_GET:
				{
				setState(174); exprGet();
				}
				break;
			case STMT_ADD:
				{
				setState(175); exprAdd();
				}
				break;
			case STMT_DROP:
				{
				setState(176); exprDrop();
				}
				break;
			case STMT_MODIFY:
				{
				setState(177); exprModify();
				}
				break;
			case STMT_GRANT:
				{
				setState(178); exprGrant();
				}
				break;
			case STMT_REVOKE:
				{
				setState(179); exprRevoke();
				}
				break;
			case STMT_ASSIGN:
				{
				setState(180); exprAssign();
				}
				break;
			case STMT_REMOVE:
				{
				setState(181); exprRemove();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(184); match(EOF);
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
			setState(186); match(STMT_ADD);
			setState(207);
			switch (_input.LA(1)) {
			case TYPE_USER:
				{
				setState(187); match(TYPE_USER);
				setState(188); match(VALUE);
				setState(189); exprWithPassword();
				setState(200);
				_la = _input.LA(1);
				if (_la==OP_WITH) {
					{
					setState(198);
					switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
					case 1:
						{
						setState(190); exprWithRoles();
						setState(192);
						_la = _input.LA(1);
						if (_la==OP_WITH) {
							{
							setState(191); exprWithPermissions();
							}
						}

						}
						break;

					case 2:
						{
						setState(194); exprWithPermissions();
						setState(196);
						_la = _input.LA(1);
						if (_la==OP_WITH) {
							{
							setState(195); exprWithRoles();
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
				setState(202); match(TYPE_ROLE);
				setState(203); match(VALUE);
				setState(205);
				_la = _input.LA(1);
				if (_la==OP_WITH) {
					{
					setState(204); exprWithPermissions();
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
			setState(209); match(OP_WITH);
			setState(210); match(PROP_PASSWORD);
			setState(211); match(VALUE);
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
			setState(213); match(OP_WITH);
			setState(214); match(TYPE_PERMISSIONS);
			setState(215); selectorValueList();
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
			setState(217); match(OP_WITH);
			setState(218); match(TYPE_ROLES);
			setState(219); selectorValueList();
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
			setState(221); match(STMT_DROP);
			setState(226);
			switch (_input.LA(1)) {
			case TYPE_ROLE:
			case TYPE_USER:
				{
				{
				setState(222);
				_la = _input.LA(1);
				if ( !(_la==TYPE_ROLE || _la==TYPE_USER) ) {
				_errHandler.recoverInline(this);
				}
				consume();
				setState(223); match(VALUE);
				}
				}
				break;
			case TYPE_MODEL:
				{
				setState(224); match(TYPE_MODEL);
				setState(225); selectorModelId();
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
			setState(228); match(STMT_MODIFY);
			setState(236);
			switch (_input.LA(1)) {
			case TYPE_USER:
				{
				{
				setState(229); match(TYPE_USER);
				setState(230); match(VALUE);
				setState(231); exprSetPassword();
				}
				}
				break;
			case TYPE_MODEL:
				{
				{
				setState(232); match(TYPE_MODEL);
				setState(233); selectorModelId();
				setState(234); exprSetBulkLoad();
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
			setState(238); match(OP_SET);
			setState(239); match(PROP_PASSWORD);
			setState(240); match(CMP_EQUAL);
			setState(241); match(VALUE);
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
			setState(243); match(OP_SET);
			setState(244); match(PROP_BULKLOAD);
			setState(245); match(CMP_EQUAL);
			setState(246); selectorBoolean();
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
			setState(248); match(STMT_GRANT);
			setState(249); match(TYPE_PERMISSIONS);
			setState(250); selectorValueList();
			setState(251); match(OP_TO);
			setState(252);
			_la = _input.LA(1);
			if ( !(_la==TYPE_ROLE || _la==TYPE_USER) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(253); match(VALUE);
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
			setState(255); match(STMT_REVOKE);
			setState(256); match(TYPE_PERMISSIONS);
			setState(257); selectorValueList();
			setState(258); match(OP_FROM);
			setState(259);
			_la = _input.LA(1);
			if ( !(_la==TYPE_ROLE || _la==TYPE_USER) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(260); match(VALUE);
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
			setState(262); match(STMT_ASSIGN);
			setState(265);
			switch (_input.LA(1)) {
			case TYPE_ROLE:
				{
				setState(263); exprAssignSingleRole();
				}
				break;
			case TYPE_ROLES:
				{
				setState(264); exprAssignMultipleRoles();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(267); match(OP_TO);
			setState(268); match(TYPE_USER);
			setState(269); match(VALUE);
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
			setState(271); match(TYPE_ROLE);
			setState(272); match(VALUE);
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
			setState(274); match(TYPE_ROLES);
			setState(275); selectorValueList();
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
			setState(277); match(STMT_REMOVE);
			setState(280);
			switch (_input.LA(1)) {
			case TYPE_ROLE:
				{
				setState(278); exprRemoveSingleRole();
				}
				break;
			case TYPE_ROLES:
				{
				setState(279); exprRemoveMultipleRoles();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(282); match(OP_FROM);
			setState(283); match(TYPE_USER);
			setState(284); match(VALUE);
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
			setState(286); match(TYPE_ROLE);
			setState(287); match(VALUE);
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
			setState(289); match(TYPE_ROLES);
			setState(290); selectorValueList();
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
			setState(292); match(STMT_GET);
			setState(293);
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
			setState(295); match(STMT_ALIVE);
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
			setState(297);
			_la = _input.LA(1);
			if ( !(_la==STMT_OPEN || _la==STMT_LOAD) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(301);
			switch (_input.LA(1)) {
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				{
				setState(298); selectorModelId();
				}
				break;
			case OP_FROM:
				{
				{
				setState(299); match(OP_FROM);
				setState(300); match(VALUE);
				}
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(312);
			_la = _input.LA(1);
			if (_la==OP_SET) {
				{
				setState(303); match(OP_SET);
				setState(304); exprLoadSetProperty();
				setState(309);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(305); match(SEPARATOR);
					setState(306); exprLoadSetProperty();
					}
					}
					setState(311);
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
			setState(320);
			switch (_input.LA(1)) {
			case PROP_AUTOLOAD:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(314); match(PROP_AUTOLOAD);
				setState(315); match(CMP_EQUAL);
				setState(316); selectorBoolean();
				}
				}
				break;
			case PROP_FORCE:
				enterOuterAlt(_localctx, 2);
				{
				{
				setState(317); match(PROP_FORCE);
				setState(318); match(CMP_EQUAL);
				setState(319); selectorBoolean();
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
			setState(322); match(STMT_UNLOAD);
			setState(323); selectorModelId();
			setState(333);
			_la = _input.LA(1);
			if (_la==OP_SET) {
				{
				setState(324); match(OP_SET);
				setState(325); exprUnloadSetProperty();
				setState(330);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(326); match(SEPARATOR);
					setState(327); exprUnloadSetProperty();
					}
					}
					setState(332);
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
			setState(335); match(PROP_AUTOLOAD);
			setState(336); match(CMP_EQUAL);
			setState(337); selectorBoolean();
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
			setState(339); match(STMT_INSERT);
			setState(340); match(OP_INTO);
			setState(341); selectorModelId();
			setState(342); exprStructure();
			setState(343); match(OP_VALUES);
			setState(344); exprValues();
			setState(349);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(345); match(SEPARATOR);
				setState(346); exprValues();
				}
				}
				setState(351);
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
			setState(352); match(BRACKET_ROUND_OPENED);
			setState(353); compStructureElement();
			setState(358);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(354); match(SEPARATOR);
				setState(355); compStructureElement();
				}
				}
				setState(360);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(361); match(BRACKET_ROUND_CLOSED);
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
			setState(363); match(BRACKET_ROUND_OPENED);
			setState(364); compValueElement();
			setState(369);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(365); match(SEPARATOR);
				setState(366); compValueElement();
				}
				}
				setState(371);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(372); match(BRACKET_ROUND_CLOSED);
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
			setState(374); match(STMT_DELETE);
			setState(375); selectorIntIdList();
			setState(376); match(OP_FROM);
			setState(377); selectorModelId();
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
			setState(381);
			switch ( getInterpreter().adaptivePredict(_input,20,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(379); exprSelectRecords();
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(380); exprSelectTimeSeries();
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
			setState(383); match(STMT_SELECT);
			setState(389);
			switch (_input.LA(1)) {
			case TYPE_RECORDS:
				{
				setState(384); match(TYPE_RECORDS);
				}
				break;
			case OP_IDONLY:
			case AGGR_COUNT:
				{
				setState(385);
				_la = _input.LA(1);
				if ( !(_la==OP_IDONLY || _la==AGGR_COUNT) ) {
				_errHandler.recoverInline(this);
				}
				consume();
				setState(386); match(BRACKET_ROUND_OPENED);
				setState(387); match(TYPE_RECORDS);
				setState(388); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(391); match(OP_FROM);
			setState(392); selectorModelId();
			setState(396);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << IR_EQUALTO) | (1L << IR_BEFORE) | (1L << IR_AFTER) | (1L << IR_MEETING) | (1L << IR_OVERLAPPING) | (1L << IR_DURING) | (1L << IR_WITHIN) | (1L << IR_CONTAINING) | (1L << IR_STARTINGWITH) | (1L << IR_FINISHINGWITH))) != 0)) {
				{
				setState(393); selectorIntervalRelation();
				setState(394); exprInterval();
				}
			}

			setState(400);
			_la = _input.LA(1);
			if (_la==OP_FILTERBY) {
				{
				setState(398); match(OP_FILTERBY);
				setState(399); exprComp(0);
				}
			}

			setState(408);
			_la = _input.LA(1);
			if (_la==OP_LIMIT) {
				{
				setState(402); match(OP_LIMIT);
				setState(403); selectorOffset();
				setState(406);
				_la = _input.LA(1);
				if (_la==SEPARATOR) {
					{
					setState(404); match(SEPARATOR);
					setState(405); selectorLimit();
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
			setState(410); match(STMT_SELECT);
			setState(416);
			switch (_input.LA(1)) {
			case TYPE_TIMESERIES:
				{
				setState(411); match(TYPE_TIMESERIES);
				}
				break;
			case OP_TRANSPOSE:
				{
				setState(412); match(OP_TRANSPOSE);
				setState(413); match(BRACKET_ROUND_OPENED);
				setState(414); match(TYPE_TIMESERIES);
				setState(415); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(420);
			_la = _input.LA(1);
			if (_la==OP_OF) {
				{
				setState(418); match(OP_OF);
				setState(419); exprMeasure();
				}
			}

			setState(422); match(OP_FROM);
			setState(423); selectorModelId();
			setState(426);
			_la = _input.LA(1);
			if (_la==OP_IN) {
				{
				setState(424); match(OP_IN);
				setState(425); exprInterval();
				}
			}

			setState(430);
			_la = _input.LA(1);
			if (_la==OP_FILTERBY) {
				{
				setState(428); match(OP_FILTERBY);
				setState(429); exprComp(0);
				}
			}

			setState(434);
			_la = _input.LA(1);
			if (_la==OP_GROUPBY) {
				{
				setState(432); match(OP_GROUPBY);
				setState(433); exprGroup();
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
			setState(455);
			switch ( getInterpreter().adaptivePredict(_input,33,_ctx) ) {
			case 1:
				{
				setState(436); compNamedLowMeasure();
				setState(441);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(437); match(SEPARATOR);
					setState(438); compNamedLowMeasure();
					}
					}
					setState(443);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
				break;

			case 2:
				{
				{
				setState(444); compNamedDimMathMeasure();
				setState(449);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(445); match(SEPARATOR);
					setState(446); compNamedDimMathMeasure();
					}
					}
					setState(451);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(452); match(OP_ON);
				setState(453); selectorMember();
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
			setState(457); selectorOpenInterval();
			setState(460);
			switch (_input.LA(1)) {
			case DATE:
				{
				setState(458); selectorDateInterval();
				}
				break;
			case INT:
				{
				setState(459); selectorIntInterval();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(462); selectorCloseInterval();
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
			setState(473);
			switch ( getInterpreter().adaptivePredict(_input,35,_ctx) ) {
			case 1:
				{
				setState(465); match(LOGICAL_NOT);
				setState(466); exprComp(2);
				}
				break;

			case 2:
				{
				setState(467); compMemberEqual();
				}
				break;

			case 3:
				{
				setState(468); compDescriptorEqual();
				}
				break;

			case 4:
				{
				setState(469); match(BRACKET_ROUND_OPENED);
				setState(470); exprComp(0);
				setState(471); match(BRACKET_ROUND_CLOSED);
				}
				break;
			}
			_ctx.stop = _input.LT(-1);
			setState(480);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,36,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new ExprCompContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_exprComp);
					setState(475);
					if (!(1 >= _localctx._p)) throw new FailedPredicateException(this, "1 >= $_p");
					setState(476);
					_la = _input.LA(1);
					if ( !(_la==LOGICAL_OR || _la==LOGICAL_AND) ) {
					_errHandler.recoverInline(this);
					}
					consume();
					setState(477); exprComp(2);
					}
					} 
				}
				setState(482);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,36,_ctx);
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
			setState(483); exprAggregate();
			setState(485);
			_la = _input.LA(1);
			if (_la==LOGICAL_INCLUDE) {
				{
				setState(484); compGroupInclude();
				}
			}

			setState(488);
			_la = _input.LA(1);
			if (_la==LOGICAL_EXCLUDE) {
				{
				setState(487); compGroupExclude();
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
			setState(492);
			switch ( getInterpreter().adaptivePredict(_input,39,_ctx) ) {
			case 1:
				{
				setState(490); selectorMember();
				}
				break;

			case 2:
				{
				setState(491); selectorDescriptorId();
				}
				break;
			}
			setState(501);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(494); match(SEPARATOR);
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
				}
				}
				setState(503);
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
			setState(504); compLowMeasure(0);
			setState(507);
			_la = _input.LA(1);
			if (_la==OP_ALIAS) {
				{
				setState(505); match(OP_ALIAS);
				setState(506); selectorAlias();
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
			setState(509); compDimMathMeasure(0);
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
			setState(514); selectorMember();
			setState(515); match(CMP_EQUAL);
			setState(516); selectorValue();
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
			setState(518); selectorDescriptorId();
			setState(519); match(CMP_EQUAL);
			setState(520); selectorValue();
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
			setState(522); match(BRACKET_ROUND_OPENED);
			setState(523); selectorValue();
			setState(528);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(524); match(SEPARATOR);
				setState(525); selectorValue();
				}
				}
				setState(530);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(531); match(BRACKET_ROUND_CLOSED);
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
			setState(533); match(LOGICAL_INCLUDE);
			setState(534); compGroupFilter();
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
			setState(536); match(LOGICAL_EXCLUDE);
			setState(537); compGroupFilter();
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
			setState(539); match(BRACKET_CURLY_OPENED);
			setState(540); compDescValueTupel();
			setState(545);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(541); match(SEPARATOR);
				setState(542); compDescValueTupel();
				}
				}
				setState(547);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(548); match(BRACKET_CURLY_CLOSED);
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
			setState(552);
			switch (_input.LA(1)) {
			case POS_START_INCL:
			case POS_END_INCL:
			case POS_START_EXCL:
			case POS_END_EXCL:
				enterOuterAlt(_localctx, 1);
				{
				setState(550); selectorIntervalDef();
				}
				break;
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				enterOuterAlt(_localctx, 2);
				{
				setState(551); selectorDescriptorId();
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
			setState(558);
			switch ( getInterpreter().adaptivePredict(_input,47,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(554); selectorNullValue();
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(555); selectorDateValue();
				}
				break;

			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(556); selectorIntValue();
				}
				break;

			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(557); selectorValue();
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
		enterRule(_localctx, 90, RULE_compDimAggrFunction);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(560); selectorDimAggrFunctionName();
			setState(561); match(BRACKET_ROUND_OPENED);
			setState(562); compDescriptorFormula(0);
			setState(563); match(BRACKET_ROUND_CLOSED);
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
		enterRule(_localctx, 92, RULE_compLowAggrFunction);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(565); selectorLowAggrFunctionName();
			setState(566); match(BRACKET_ROUND_OPENED);
			setState(567); compDescriptorFormula(0);
			setState(568); match(BRACKET_ROUND_CLOSED);
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
		enterRule(_localctx, 94, RULE_compMathAggrFunction);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(570); selectorMathAggrFunctionName();
			setState(571); match(BRACKET_ROUND_OPENED);
			setState(572); compLowMeasure(0);
			setState(573); match(BRACKET_ROUND_CLOSED);
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
		int _startState = 96;
		enterRecursionRule(_localctx, RULE_compLowMeasure);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(576); compLowMeasureAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(584);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,48,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompLowMeasureContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compLowMeasure);
					setState(578);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(579); selectorSecondMathOperator();
					setState(580); compLowMeasureAtom(0);
					}
					} 
				}
				setState(586);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,48,_ctx);
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
		int _startState = 98;
		enterRecursionRule(_localctx, RULE_compLowMeasureAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(593);
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
				setState(588); compLowAggrFunction();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(589); match(BRACKET_ROUND_OPENED);
				setState(590); compLowMeasure(0);
				setState(591); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(601);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,50,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompLowMeasureAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compLowMeasureAtom);
					setState(595);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(596); selectorFirstMathOperator();
					setState(597); compLowMeasureAtom(0);
					}
					} 
				}
				setState(603);
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
		int _startState = 100;
		enterRecursionRule(_localctx, RULE_compMathMeasure);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(605); compMathMeasureAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(613);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,51,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompMathMeasureContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compMathMeasure);
					setState(607);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(608); selectorSecondMathOperator();
					setState(609); compMathMeasureAtom(0);
					}
					} 
				}
				setState(615);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,51,_ctx);
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
		int _startState = 102;
		enterRecursionRule(_localctx, RULE_compMathMeasureAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(622);
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
				setState(617); compMathAggrFunction();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(618); match(BRACKET_ROUND_OPENED);
				setState(619); compMathMeasure(0);
				setState(620); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(630);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,53,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompMathMeasureAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compMathMeasureAtom);
					setState(624);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(625); selectorFirstMathOperator();
					setState(626); compMathMeasureAtom(0);
					}
					} 
				}
				setState(632);
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
		int _startState = 104;
		enterRecursionRule(_localctx, RULE_compDimMeasure);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(634); compDimMeasureAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(642);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,54,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDimMeasureContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDimMeasure);
					setState(636);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(637); selectorSecondMathOperator();
					setState(638); compDimMeasureAtom(0);
					}
					} 
				}
				setState(644);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,54,_ctx);
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
		int _startState = 106;
		enterRecursionRule(_localctx, RULE_compDimMeasureAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(651);
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
				setState(646); compDimAggrFunction();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(647); match(BRACKET_ROUND_OPENED);
				setState(648); compDimMeasure(0);
				setState(649); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(659);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,56,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDimMeasureAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDimMeasureAtom);
					setState(653);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(654); selectorFirstMathOperator();
					setState(655); compDimMeasureAtom(0);
					}
					} 
				}
				setState(661);
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
		int _startState = 108;
		enterRecursionRule(_localctx, RULE_compDimMathMeasure);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(663); compDimMathMeasureAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(671);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,57,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDimMathMeasureContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDimMathMeasure);
					setState(665);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(666); selectorSecondMathOperator();
					setState(667); compDimMathMeasureAtom(0);
					}
					} 
				}
				setState(673);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,57,_ctx);
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
		int _startState = 110;
		enterRecursionRule(_localctx, RULE_compDimMathMeasureAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(681);
			switch ( getInterpreter().adaptivePredict(_input,58,_ctx) ) {
			case 1:
				{
				setState(675); compMathMeasure(0);
				}
				break;

			case 2:
				{
				setState(676); compDimMeasure(0);
				}
				break;

			case 3:
				{
				setState(677); match(BRACKET_ROUND_OPENED);
				setState(678); compDimMathMeasureAtom(0);
				setState(679); match(BRACKET_ROUND_CLOSED);
				}
				break;
			}
			_ctx.stop = _input.LT(-1);
			setState(689);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,59,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDimMathMeasureAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDimMathMeasureAtom);
					setState(683);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(684); selectorFirstMathOperator();
					setState(685); compDimMathMeasureAtom(0);
					}
					} 
				}
				setState(691);
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
		int _startState = 112;
		enterRecursionRule(_localctx, RULE_compDescriptorFormula);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(693); compDescriptorFormulaAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(701);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,60,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDescriptorFormulaContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDescriptorFormula);
					setState(695);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(696); selectorSecondMathOperator();
					setState(697); compDescriptorFormulaAtom(0);
					}
					} 
				}
				setState(703);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,60,_ctx);
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
		int _startState = 114;
		enterRecursionRule(_localctx, RULE_compDescriptorFormulaAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(710);
			switch (_input.LA(1)) {
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				{
				setState(705); selectorDescriptorId();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(706); match(BRACKET_ROUND_OPENED);
				setState(707); compDescriptorFormula(0);
				setState(708); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(718);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,62,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDescriptorFormulaAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDescriptorFormulaAtom);
					setState(712);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(713); selectorFirstMathOperator();
					setState(714); compDescriptorFormulaAtom(0);
					}
					} 
				}
				setState(720);
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
		enterRule(_localctx, 116, RULE_selectorMember);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(721);
			_la = _input.LA(1);
			if ( !(_la==MARKED_ID || _la==SIMPLE_ID || _la==ENHANCED_ID) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(722); match(DIMSEPARATOR);
			setState(723);
			_la = _input.LA(1);
			if ( !(_la==MARKED_ID || _la==SIMPLE_ID || _la==ENHANCED_ID) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(724); match(DIMSEPARATOR);
			setState(725);
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
		enterRule(_localctx, 118, RULE_selectorModelId);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(727);
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
		enterRule(_localctx, 120, RULE_selectorDescriptorId);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(729);
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
		enterRule(_localctx, 122, RULE_selectorAlias);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(731);
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
		enterRule(_localctx, 124, RULE_selectorDateInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(733); match(DATE);
			setState(734); match(SEPARATOR);
			setState(735); match(DATE);
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
		enterRule(_localctx, 126, RULE_selectorOffset);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(737); match(INT);
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
		enterRule(_localctx, 128, RULE_selectorLimit);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(739); match(INT);
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
		enterRule(_localctx, 130, RULE_selectorIntInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(741); match(INT);
			setState(742); match(SEPARATOR);
			setState(743); match(INT);
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
		enterRule(_localctx, 132, RULE_selectorIntIdList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(745); match(INT);
			setState(750);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(746); match(SEPARATOR);
				setState(747); match(INT);
				}
				}
				setState(752);
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
		enterRule(_localctx, 134, RULE_selectorDateIntervalWithNull);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(753);
			_la = _input.LA(1);
			if ( !(_la==NULL_VALUE || _la==DATE) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(754); match(SEPARATOR);
			setState(755);
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
		enterRule(_localctx, 136, RULE_selectorIntIntervalWithNull);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(757);
			_la = _input.LA(1);
			if ( !(_la==NULL_VALUE || _la==INT) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(758); match(SEPARATOR);
			setState(759);
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
		enterRule(_localctx, 138, RULE_selectorDateValue);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(761); match(DATE);
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
		enterRule(_localctx, 140, RULE_selectorIntValue);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(763); match(INT);
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
		enterRule(_localctx, 142, RULE_selectorNullValue);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(765); match(NULL_VALUE);
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
		enterRule(_localctx, 144, RULE_selectorValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(767);
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
		enterRule(_localctx, 146, RULE_selectorOpenInterval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(769);
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
		enterRule(_localctx, 148, RULE_selectorCloseInterval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(771);
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
		enterRule(_localctx, 150, RULE_selectorMathAggrFunctionName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(773);
			_la = _input.LA(1);
			if ( !(((((_la - 76)) & ~0x3f) == 0 && ((1L << (_la - 76)) & ((1L << (AGGR_COUNT - 76)) | (1L << (AGGR_SUM - 76)) | (1L << (AGGR_MIN - 76)) | (1L << (AGGR_MAX - 76)) | (1L << (AGGR_AVERAGE - 76)) | (1L << (AGGR_MODE - 76)) | (1L << (AGGR_MEAN - 76)) | (1L << (AGGR_MEDIAN - 76)) | (1L << (SIMPLE_ID - 76)))) != 0)) ) {
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
		enterRule(_localctx, 152, RULE_selectorDimAggrFunctionName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(775);
			_la = _input.LA(1);
			if ( !(((((_la - 76)) & ~0x3f) == 0 && ((1L << (_la - 76)) & ((1L << (AGGR_COUNT - 76)) | (1L << (AGGR_SUM - 76)) | (1L << (AGGR_MIN - 76)) | (1L << (AGGR_MAX - 76)) | (1L << (AGGR_AVERAGE - 76)) | (1L << (AGGR_MODE - 76)) | (1L << (AGGR_MEAN - 76)) | (1L << (AGGR_MEDIAN - 76)) | (1L << (SIMPLE_ID - 76)))) != 0)) ) {
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
		enterRule(_localctx, 154, RULE_selectorLowAggrFunctionName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(777);
			_la = _input.LA(1);
			if ( !(((((_la - 74)) & ~0x3f) == 0 && ((1L << (_la - 74)) & ((1L << (AGGR_COUNTSTARTED - 74)) | (1L << (AGGR_COUNTFINISHED - 74)) | (1L << (AGGR_COUNT - 74)) | (1L << (AGGR_SUM - 74)) | (1L << (AGGR_MIN - 74)) | (1L << (AGGR_MAX - 74)) | (1L << (AGGR_AVERAGE - 74)) | (1L << (AGGR_MODE - 74)) | (1L << (AGGR_MEAN - 74)) | (1L << (AGGR_MEDIAN - 74)) | (1L << (SIMPLE_ID - 74)))) != 0)) ) {
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
		enterRule(_localctx, 156, RULE_selectorFirstMathOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(779);
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
		enterRule(_localctx, 158, RULE_selectorSecondMathOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(781);
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
		enterRule(_localctx, 160, RULE_selectorIntervalDef);
		int _la;
		try {
			setState(785);
			switch (_input.LA(1)) {
			case POS_START_INCL:
			case POS_START_EXCL:
				enterOuterAlt(_localctx, 1);
				{
				setState(783);
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
				setState(784);
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
		enterRule(_localctx, 162, RULE_selectorBoolean);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(787);
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
		enterRule(_localctx, 164, RULE_selectorIntervalRelation);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(789);
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
		enterRule(_localctx, 166, RULE_selectorValueList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(791); match(VALUE);
			setState(796);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(792); match(SEPARATOR);
				setState(793); match(VALUE);
				}
				}
				setState(798);
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

		case 48: return compLowMeasure_sempred((CompLowMeasureContext)_localctx, predIndex);

		case 49: return compLowMeasureAtom_sempred((CompLowMeasureAtomContext)_localctx, predIndex);

		case 50: return compMathMeasure_sempred((CompMathMeasureContext)_localctx, predIndex);

		case 51: return compMathMeasureAtom_sempred((CompMathMeasureAtomContext)_localctx, predIndex);

		case 52: return compDimMeasure_sempred((CompDimMeasureContext)_localctx, predIndex);

		case 53: return compDimMeasureAtom_sempred((CompDimMeasureAtomContext)_localctx, predIndex);

		case 54: return compDimMathMeasure_sempred((CompDimMathMeasureContext)_localctx, predIndex);

		case 55: return compDimMathMeasureAtom_sempred((CompDimMathMeasureAtomContext)_localctx, predIndex);

		case 56: return compDescriptorFormula_sempred((CompDescriptorFormulaContext)_localctx, predIndex);

		case 57: return compDescriptorFormulaAtom_sempred((CompDescriptorFormulaAtomContext)_localctx, predIndex);
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
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\3c\u0322\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t+\4"+
		",\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t\61\4\62\t\62\4\63\t\63\4\64\t"+
		"\64\4\65\t\65\4\66\t\66\4\67\t\67\48\t8\49\t9\4:\t:\4;\t;\4<\t<\4=\t="+
		"\4>\t>\4?\t?\4@\t@\4A\tA\4B\tB\4C\tC\4D\tD\4E\tE\4F\tF\4G\tG\4H\tH\4I"+
		"\tI\4J\tJ\4K\tK\4L\tL\4M\tM\4N\tN\4O\tO\4P\tP\4Q\tQ\4R\tR\4S\tS\4T\tT"+
		"\4U\tU\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\5\2\u00b9"+
		"\n\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\5\3\u00c3\n\3\3\3\3\3\5\3\u00c7\n"+
		"\3\5\3\u00c9\n\3\5\3\u00cb\n\3\3\3\3\3\3\3\5\3\u00d0\n\3\5\3\u00d2\n\3"+
		"\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\5"+
		"\7\u00e5\n\7\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\5\b\u00ef\n\b\3\t\3\t\3\t"+
		"\3\t\3\t\3\n\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\f\3"+
		"\f\3\f\3\f\3\f\3\f\3\f\3\r\3\r\3\r\5\r\u010c\n\r\3\r\3\r\3\r\3\r\3\16"+
		"\3\16\3\16\3\17\3\17\3\17\3\20\3\20\3\20\5\20\u011b\n\20\3\20\3\20\3\20"+
		"\3\20\3\21\3\21\3\21\3\22\3\22\3\22\3\23\3\23\3\23\3\24\3\24\3\25\3\25"+
		"\3\25\3\25\5\25\u0130\n\25\3\25\3\25\3\25\3\25\7\25\u0136\n\25\f\25\16"+
		"\25\u0139\13\25\5\25\u013b\n\25\3\26\3\26\3\26\3\26\3\26\3\26\5\26\u0143"+
		"\n\26\3\27\3\27\3\27\3\27\3\27\3\27\7\27\u014b\n\27\f\27\16\27\u014e\13"+
		"\27\5\27\u0150\n\27\3\30\3\30\3\30\3\30\3\31\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\31\7\31\u015e\n\31\f\31\16\31\u0161\13\31\3\32\3\32\3\32\3\32"+
		"\7\32\u0167\n\32\f\32\16\32\u016a\13\32\3\32\3\32\3\33\3\33\3\33\3\33"+
		"\7\33\u0172\n\33\f\33\16\33\u0175\13\33\3\33\3\33\3\34\3\34\3\34\3\34"+
		"\3\34\3\35\3\35\5\35\u0180\n\35\3\36\3\36\3\36\3\36\3\36\3\36\5\36\u0188"+
		"\n\36\3\36\3\36\3\36\3\36\3\36\5\36\u018f\n\36\3\36\3\36\5\36\u0193\n"+
		"\36\3\36\3\36\3\36\3\36\5\36\u0199\n\36\5\36\u019b\n\36\3\37\3\37\3\37"+
		"\3\37\3\37\3\37\5\37\u01a3\n\37\3\37\3\37\5\37\u01a7\n\37\3\37\3\37\3"+
		"\37\3\37\5\37\u01ad\n\37\3\37\3\37\5\37\u01b1\n\37\3\37\3\37\5\37\u01b5"+
		"\n\37\3 \3 \3 \7 \u01ba\n \f \16 \u01bd\13 \3 \3 \3 \7 \u01c2\n \f \16"+
		" \u01c5\13 \3 \3 \3 \5 \u01ca\n \3!\3!\3!\5!\u01cf\n!\3!\3!\3\"\3\"\3"+
		"\"\3\"\3\"\3\"\3\"\3\"\3\"\5\"\u01dc\n\"\3\"\3\"\3\"\7\"\u01e1\n\"\f\""+
		"\16\"\u01e4\13\"\3#\3#\5#\u01e8\n#\3#\5#\u01eb\n#\3$\3$\5$\u01ef\n$\3"+
		"$\3$\3$\5$\u01f4\n$\7$\u01f6\n$\f$\16$\u01f9\13$\3%\3%\3%\5%\u01fe\n%"+
		"\3&\3&\3&\5&\u0203\n&\3\'\3\'\3\'\3\'\3(\3(\3(\3(\3)\3)\3)\3)\7)\u0211"+
		"\n)\f)\16)\u0214\13)\3)\3)\3*\3*\3*\3+\3+\3+\3,\3,\3,\3,\7,\u0222\n,\f"+
		",\16,\u0225\13,\3,\3,\3-\3-\5-\u022b\n-\3.\3.\3.\3.\5.\u0231\n.\3/\3/"+
		"\3/\3/\3/\3\60\3\60\3\60\3\60\3\60\3\61\3\61\3\61\3\61\3\61\3\62\3\62"+
		"\3\62\3\62\3\62\3\62\3\62\7\62\u0249\n\62\f\62\16\62\u024c\13\62\3\63"+
		"\3\63\3\63\3\63\3\63\3\63\5\63\u0254\n\63\3\63\3\63\3\63\3\63\7\63\u025a"+
		"\n\63\f\63\16\63\u025d\13\63\3\64\3\64\3\64\3\64\3\64\3\64\3\64\7\64\u0266"+
		"\n\64\f\64\16\64\u0269\13\64\3\65\3\65\3\65\3\65\3\65\3\65\5\65\u0271"+
		"\n\65\3\65\3\65\3\65\3\65\7\65\u0277\n\65\f\65\16\65\u027a\13\65\3\66"+
		"\3\66\3\66\3\66\3\66\3\66\3\66\7\66\u0283\n\66\f\66\16\66\u0286\13\66"+
		"\3\67\3\67\3\67\3\67\3\67\3\67\5\67\u028e\n\67\3\67\3\67\3\67\3\67\7\67"+
		"\u0294\n\67\f\67\16\67\u0297\13\67\38\38\38\38\38\38\38\78\u02a0\n8\f"+
		"8\168\u02a3\138\39\39\39\39\39\39\39\59\u02ac\n9\39\39\39\39\79\u02b2"+
		"\n9\f9\169\u02b5\139\3:\3:\3:\3:\3:\3:\3:\7:\u02be\n:\f:\16:\u02c1\13"+
		":\3;\3;\3;\3;\3;\3;\5;\u02c9\n;\3;\3;\3;\3;\7;\u02cf\n;\f;\16;\u02d2\13"+
		";\3<\3<\3<\3<\3<\3<\3=\3=\3>\3>\3?\3?\3@\3@\3@\3@\3A\3A\3B\3B\3C\3C\3"+
		"C\3C\3D\3D\3D\7D\u02ef\nD\fD\16D\u02f2\13D\3E\3E\3E\3E\3F\3F\3F\3F\3G"+
		"\3G\3H\3H\3I\3I\3J\3J\3K\3K\3L\3L\3M\3M\3N\3N\3O\3O\3P\3P\3Q\3Q\3R\3R"+
		"\5R\u0314\nR\3S\3S\3T\3T\3U\3U\3U\7U\u031d\nU\fU\16U\u0320\13U\3U\2V\2"+
		"\4\6\b\n\f\16\20\22\24\26\30\32\34\36 \"$&(*,.\60\62\64\668:<>@BDFHJL"+
		"NPRTVXZ\\^`bdfhjlnprtvxz|~\u0080\u0082\u0084\u0086\u0088\u008a\u008c\u008e"+
		"\u0090\u0092\u0094\u0096\u0098\u009a\u009c\u009e\u00a0\u00a2\u00a4\u00a6"+
		"\u00a8\2\25\3\2&\'\4\2\37\37!$\3\2\16\17\4\2\65\65NN\3\2AB\4\2\3\3ab\4"+
		"\2\5\5]]\4\2\5\5^^\3\2\4\5\4\2WWYY\4\2XXZZ\4\2NUaa\4\2LUaa\3\2HI\3\2J"+
		"K\4\2\6\6\b\b\4\2\7\7\t\t\3\2FG\3\2\67@\u0320\2\u00b8\3\2\2\2\4\u00bc"+
		"\3\2\2\2\6\u00d3\3\2\2\2\b\u00d7\3\2\2\2\n\u00db\3\2\2\2\f\u00df\3\2\2"+
		"\2\16\u00e6\3\2\2\2\20\u00f0\3\2\2\2\22\u00f5\3\2\2\2\24\u00fa\3\2\2\2"+
		"\26\u0101\3\2\2\2\30\u0108\3\2\2\2\32\u0111\3\2\2\2\34\u0114\3\2\2\2\36"+
		"\u0117\3\2\2\2 \u0120\3\2\2\2\"\u0123\3\2\2\2$\u0126\3\2\2\2&\u0129\3"+
		"\2\2\2(\u012b\3\2\2\2*\u0142\3\2\2\2,\u0144\3\2\2\2.\u0151\3\2\2\2\60"+
		"\u0155\3\2\2\2\62\u0162\3\2\2\2\64\u016d\3\2\2\2\66\u0178\3\2\2\28\u017f"+
		"\3\2\2\2:\u0181\3\2\2\2<\u019c\3\2\2\2>\u01c9\3\2\2\2@\u01cb\3\2\2\2B"+
		"\u01db\3\2\2\2D\u01e5\3\2\2\2F\u01ee\3\2\2\2H\u01fa\3\2\2\2J\u01ff\3\2"+
		"\2\2L\u0204\3\2\2\2N\u0208\3\2\2\2P\u020c\3\2\2\2R\u0217\3\2\2\2T\u021a"+
		"\3\2\2\2V\u021d\3\2\2\2X\u022a\3\2\2\2Z\u0230\3\2\2\2\\\u0232\3\2\2\2"+
		"^\u0237\3\2\2\2`\u023c\3\2\2\2b\u0241\3\2\2\2d\u0253\3\2\2\2f\u025e\3"+
		"\2\2\2h\u0270\3\2\2\2j\u027b\3\2\2\2l\u028d\3\2\2\2n\u0298\3\2\2\2p\u02ab"+
		"\3\2\2\2r\u02b6\3\2\2\2t\u02c8\3\2\2\2v\u02d3\3\2\2\2x\u02d9\3\2\2\2z"+
		"\u02db\3\2\2\2|\u02dd\3\2\2\2~\u02df\3\2\2\2\u0080\u02e3\3\2\2\2\u0082"+
		"\u02e5\3\2\2\2\u0084\u02e7\3\2\2\2\u0086\u02eb\3\2\2\2\u0088\u02f3\3\2"+
		"\2\2\u008a\u02f7\3\2\2\2\u008c\u02fb\3\2\2\2\u008e\u02fd\3\2\2\2\u0090"+
		"\u02ff\3\2\2\2\u0092\u0301\3\2\2\2\u0094\u0303\3\2\2\2\u0096\u0305\3\2"+
		"\2\2\u0098\u0307\3\2\2\2\u009a\u0309\3\2\2\2\u009c\u030b\3\2\2\2\u009e"+
		"\u030d\3\2\2\2\u00a0\u030f\3\2\2\2\u00a2\u0313\3\2\2\2\u00a4\u0315\3\2"+
		"\2\2\u00a6\u0317\3\2\2\2\u00a8\u0319\3\2\2\2\u00aa\u00b9\5\60\31\2\u00ab"+
		"\u00b9\5\66\34\2\u00ac\u00b9\58\35\2\u00ad\u00b9\5(\25\2\u00ae\u00b9\5"+
		",\27\2\u00af\u00b9\5&\24\2\u00b0\u00b9\5$\23\2\u00b1\u00b9\5\4\3\2\u00b2"+
		"\u00b9\5\f\7\2\u00b3\u00b9\5\16\b\2\u00b4\u00b9\5\24\13\2\u00b5\u00b9"+
		"\5\26\f\2\u00b6\u00b9\5\30\r\2\u00b7\u00b9\5\36\20\2\u00b8\u00aa\3\2\2"+
		"\2\u00b8\u00ab\3\2\2\2\u00b8\u00ac\3\2\2\2\u00b8\u00ad\3\2\2\2\u00b8\u00ae"+
		"\3\2\2\2\u00b8\u00af\3\2\2\2\u00b8\u00b0\3\2\2\2\u00b8\u00b1\3\2\2\2\u00b8"+
		"\u00b2\3\2\2\2\u00b8\u00b3\3\2\2\2\u00b8\u00b4\3\2\2\2\u00b8\u00b5\3\2"+
		"\2\2\u00b8\u00b6\3\2\2\2\u00b8\u00b7\3\2\2\2\u00b9\u00ba\3\2\2\2\u00ba"+
		"\u00bb\7\2\2\3\u00bb\3\3\2\2\2\u00bc\u00d1\7\22\2\2\u00bd\u00be\7\'\2"+
		"\2\u00be\u00bf\7\4\2\2\u00bf\u00ca\5\6\4\2\u00c0\u00c2\5\n\6\2\u00c1\u00c3"+
		"\5\b\5\2\u00c2\u00c1\3\2\2\2\u00c2\u00c3\3\2\2\2\u00c3\u00c9\3\2\2\2\u00c4"+
		"\u00c6\5\b\5\2\u00c5\u00c7\5\n\6\2\u00c6\u00c5\3\2\2\2\u00c6\u00c7\3\2"+
		"\2\2\u00c7\u00c9\3\2\2\2\u00c8\u00c0\3\2\2\2\u00c8\u00c4\3\2\2\2\u00c9"+
		"\u00cb\3\2\2\2\u00ca\u00c8\3\2\2\2\u00ca\u00cb\3\2\2\2\u00cb\u00d2\3\2"+
		"\2\2\u00cc\u00cd\7&\2\2\u00cd\u00cf\7\4\2\2\u00ce\u00d0\5\b\5\2\u00cf"+
		"\u00ce\3\2\2\2\u00cf\u00d0\3\2\2\2\u00d0\u00d2\3\2\2\2\u00d1\u00bd\3\2"+
		"\2\2\u00d1\u00cc\3\2\2\2\u00d2\5\3\2\2\2\u00d3\u00d4\7\66\2\2\u00d4\u00d5"+
		"\7\33\2\2\u00d5\u00d6\7\4\2\2\u00d6\7\3\2\2\2\u00d7\u00d8\7\66\2\2\u00d8"+
		"\u00d9\7\"\2\2\u00d9\u00da\5\u00a8U\2\u00da\t\3\2\2\2\u00db\u00dc\7\66"+
		"\2\2\u00dc\u00dd\7#\2\2\u00dd\u00de\5\u00a8U\2\u00de\13\3\2\2\2\u00df"+
		"\u00e4\7\23\2\2\u00e0\u00e1\t\2\2\2\u00e1\u00e5\7\4\2\2\u00e2\u00e3\7"+
		" \2\2\u00e3\u00e5\5x=\2\u00e4\u00e0\3\2\2\2\u00e4\u00e2\3\2\2\2\u00e5"+
		"\r\3\2\2\2\u00e6\u00ee\7\24\2\2\u00e7\u00e8\7\'\2\2\u00e8\u00e9\7\4\2"+
		"\2\u00e9\u00ef\5\20\t\2\u00ea\u00eb\7 \2\2\u00eb\u00ec\5x=\2\u00ec\u00ed"+
		"\5\22\n\2\u00ed\u00ef\3\2\2\2\u00ee\u00e7\3\2\2\2\u00ee\u00ea\3\2\2\2"+
		"\u00ef\17\3\2\2\2\u00f0\u00f1\7.\2\2\u00f1\u00f2\7\33\2\2\u00f2\u00f3"+
		"\7V\2\2\u00f3\u00f4\7\4\2\2\u00f4\21\3\2\2\2\u00f5\u00f6\7.\2\2\u00f6"+
		"\u00f7\7\34\2\2\u00f7\u00f8\7V\2\2\u00f8\u00f9\5\u00a4S\2\u00f9\23\3\2"+
		"\2\2\u00fa\u00fb\7\25\2\2\u00fb\u00fc\7\"\2\2\u00fc\u00fd\5\u00a8U\2\u00fd"+
		"\u00fe\7+\2\2\u00fe\u00ff\t\2\2\2\u00ff\u0100\7\4\2\2\u0100\25\3\2\2\2"+
		"\u0101\u0102\7\26\2\2\u0102\u0103\7\"\2\2\u0103\u0104\5\u00a8U\2\u0104"+
		"\u0105\7(\2\2\u0105\u0106\t\2\2\2\u0106\u0107\7\4\2\2\u0107\27\3\2\2\2"+
		"\u0108\u010b\7\27\2\2\u0109\u010c\5\32\16\2\u010a\u010c\5\34\17\2\u010b"+
		"\u0109\3\2\2\2\u010b\u010a\3\2\2\2\u010c\u010d\3\2\2\2\u010d\u010e\7+"+
		"\2\2\u010e\u010f\7\'\2\2\u010f\u0110\7\4\2\2\u0110\31\3\2\2\2\u0111\u0112"+
		"\7&\2\2\u0112\u0113\7\4\2\2\u0113\33\3\2\2\2\u0114\u0115\7#\2\2\u0115"+
		"\u0116\5\u00a8U\2\u0116\35\3\2\2\2\u0117\u011a\7\30\2\2\u0118\u011b\5"+
		" \21\2\u0119\u011b\5\"\22\2\u011a\u0118\3\2\2\2\u011a\u0119\3\2\2\2\u011b"+
		"\u011c\3\2\2\2\u011c\u011d\7(\2\2\u011d\u011e\7\'\2\2\u011e\u011f\7\4"+
		"\2\2\u011f\37\3\2\2\2\u0120\u0121\7&\2\2\u0121\u0122\7\4\2\2\u0122!\3"+
		"\2\2\2\u0123\u0124\7#\2\2\u0124\u0125\5\u00a8U\2\u0125#\3\2\2\2\u0126"+
		"\u0127\7\n\2\2\u0127\u0128\t\3\2\2\u0128%\3\2\2\2\u0129\u012a\7\21\2\2"+
		"\u012a\'\3\2\2\2\u012b\u012f\t\4\2\2\u012c\u0130\5x=\2\u012d\u012e\7("+
		"\2\2\u012e\u0130\7\4\2\2\u012f\u012c\3\2\2\2\u012f\u012d\3\2\2\2\u0130"+
		"\u013a\3\2\2\2\u0131\u0132\7.\2\2\u0132\u0137\5*\26\2\u0133\u0134\7_\2"+
		"\2\u0134\u0136\5*\26\2\u0135\u0133\3\2\2\2\u0136\u0139\3\2\2\2\u0137\u0135"+
		"\3\2\2\2\u0137\u0138\3\2\2\2\u0138\u013b\3\2\2\2\u0139\u0137\3\2\2\2\u013a"+
		"\u0131\3\2\2\2\u013a\u013b\3\2\2\2\u013b)\3\2\2\2\u013c\u013d\7\31\2\2"+
		"\u013d\u013e\7V\2\2\u013e\u0143\5\u00a4S\2\u013f\u0140\7\32\2\2\u0140"+
		"\u0141\7V\2\2\u0141\u0143\5\u00a4S\2\u0142\u013c\3\2\2\2\u0142\u013f\3"+
		"\2\2\2\u0143+\3\2\2\2\u0144\u0145\7\20\2\2\u0145\u014f\5x=\2\u0146\u0147"+
		"\7.\2\2\u0147\u014c\5.\30\2\u0148\u0149\7_\2\2\u0149\u014b\5.\30\2\u014a"+
		"\u0148\3\2\2\2\u014b\u014e\3\2\2\2\u014c\u014a\3\2\2\2\u014c\u014d\3\2"+
		"\2\2\u014d\u0150\3\2\2\2\u014e\u014c\3\2\2\2\u014f\u0146\3\2\2\2\u014f"+
		"\u0150\3\2\2\2\u0150-\3\2\2\2\u0151\u0152\7\31\2\2\u0152\u0153\7V\2\2"+
		"\u0153\u0154\5\u00a4S\2\u0154/\3\2\2\2\u0155\u0156\7\f\2\2\u0156\u0157"+
		"\7-\2\2\u0157\u0158\5x=\2\u0158\u0159\5\62\32\2\u0159\u015a\7/\2\2\u015a"+
		"\u015f\5\64\33\2\u015b\u015c\7_\2\2\u015c\u015e\5\64\33\2\u015d\u015b"+
		"\3\2\2\2\u015e\u0161\3\2\2\2\u015f\u015d\3\2\2\2\u015f\u0160\3\2\2\2\u0160"+
		"\61\3\2\2\2\u0161\u015f\3\2\2\2\u0162\u0163\7W\2\2\u0163\u0168\5X-\2\u0164"+
		"\u0165\7_\2\2\u0165\u0167\5X-\2\u0166\u0164\3\2\2\2\u0167\u016a\3\2\2"+
		"\2\u0168\u0166\3\2\2\2\u0168\u0169\3\2\2\2\u0169\u016b\3\2\2\2\u016a\u0168"+
		"\3\2\2\2\u016b\u016c\7X\2\2\u016c\63\3\2\2\2\u016d\u016e\7W\2\2\u016e"+
		"\u0173\5Z.\2\u016f\u0170\7_\2\2\u0170\u0172\5Z.\2\u0171\u016f\3\2\2\2"+
		"\u0172\u0175\3\2\2\2\u0173\u0171\3\2\2\2\u0173\u0174\3\2\2\2\u0174\u0176"+
		"\3\2\2\2\u0175\u0173\3\2\2\2\u0176\u0177\7X\2\2\u0177\65\3\2\2\2\u0178"+
		"\u0179\7\r\2\2\u0179\u017a\5\u0086D\2\u017a\u017b\7(\2\2\u017b\u017c\5"+
		"x=\2\u017c\67\3\2\2\2\u017d\u0180\5:\36\2\u017e\u0180\5<\37\2\u017f\u017d"+
		"\3\2\2\2\u017f\u017e\3\2\2\2\u01809\3\2\2\2\u0181\u0187\7\13\2\2\u0182"+
		"\u0188\7\36\2\2\u0183\u0184\t\5\2\2\u0184\u0185\7W\2\2\u0185\u0186\7\36"+
		"\2\2\u0186\u0188\7X\2\2\u0187\u0182\3\2\2\2\u0187\u0183\3\2\2\2\u0188"+
		"\u0189\3\2\2\2\u0189\u018a\7(\2\2\u018a\u018e\5x=\2\u018b\u018c\5\u00a6"+
		"T\2\u018c\u018d\5@!\2\u018d\u018f\3\2\2\2\u018e\u018b\3\2\2\2\u018e\u018f"+
		"\3\2\2\2\u018f\u0192\3\2\2\2\u0190\u0191\7\62\2\2\u0191\u0193\5B\"\2\u0192"+
		"\u0190\3\2\2\2\u0192\u0193\3\2\2\2\u0193\u019a\3\2\2\2\u0194\u0195\7\63"+
		"\2\2\u0195\u0198\5\u0080A\2\u0196\u0197\7_\2\2\u0197\u0199\5\u0082B\2"+
		"\u0198\u0196\3\2\2\2\u0198\u0199\3\2\2\2\u0199\u019b\3\2\2\2\u019a\u0194"+
		"\3\2\2\2\u019a\u019b\3\2\2\2\u019b;\3\2\2\2\u019c\u01a2\7\13\2\2\u019d"+
		"\u01a3\7\35\2\2\u019e\u019f\7\64\2\2\u019f\u01a0\7W\2\2\u01a0\u01a1\7"+
		"\35\2\2\u01a1\u01a3\7X\2\2\u01a2\u019d\3\2\2\2\u01a2\u019e\3\2\2\2\u01a3"+
		"\u01a6\3\2\2\2\u01a4\u01a5\7)\2\2\u01a5\u01a7\5> \2\u01a6\u01a4\3\2\2"+
		"\2\u01a6\u01a7\3\2\2\2\u01a7\u01a8\3\2\2\2\u01a8\u01a9\7(\2\2\u01a9\u01ac"+
		"\5x=\2\u01aa\u01ab\7,\2\2\u01ab\u01ad\5@!\2\u01ac\u01aa\3\2\2\2\u01ac"+
		"\u01ad\3\2\2\2\u01ad\u01b0\3\2\2\2\u01ae\u01af\7\62\2\2\u01af\u01b1\5"+
		"B\"\2\u01b0\u01ae\3\2\2\2\u01b0\u01b1\3\2\2\2\u01b1\u01b4\3\2\2\2\u01b2"+
		"\u01b3\7\61\2\2\u01b3\u01b5\5D#\2\u01b4\u01b2\3\2\2\2\u01b4\u01b5\3\2"+
		"\2\2\u01b5=\3\2\2\2\u01b6\u01bb\5H%\2\u01b7\u01b8\7_\2\2\u01b8\u01ba\5"+
		"H%\2\u01b9\u01b7\3\2\2\2\u01ba\u01bd\3\2\2\2\u01bb\u01b9\3\2\2\2\u01bb"+
		"\u01bc\3\2\2\2\u01bc\u01ca\3\2\2\2\u01bd\u01bb\3\2\2\2\u01be\u01c3\5J"+
		"&\2\u01bf\u01c0\7_\2\2\u01c0\u01c2\5J&\2\u01c1\u01bf\3\2\2\2\u01c2\u01c5"+
		"\3\2\2\2\u01c3\u01c1\3\2\2\2\u01c3\u01c4\3\2\2\2\u01c4\u01c6\3\2\2\2\u01c5"+
		"\u01c3\3\2\2\2\u01c6\u01c7\7*\2\2\u01c7\u01c8\5v<\2\u01c8\u01ca\3\2\2"+
		"\2\u01c9\u01b6\3\2\2\2\u01c9\u01be\3\2\2\2\u01ca?\3\2\2\2\u01cb\u01ce"+
		"\5\u0094K\2\u01cc\u01cf\5~@\2\u01cd\u01cf\5\u0084C\2\u01ce\u01cc\3\2\2"+
		"\2\u01ce\u01cd\3\2\2\2\u01cf\u01d0\3\2\2\2\u01d0\u01d1\5\u0096L\2\u01d1"+
		"A\3\2\2\2\u01d2\u01d3\b\"\1\2\u01d3\u01d4\7C\2\2\u01d4\u01dc\5B\"\2\u01d5"+
		"\u01dc\5L\'\2\u01d6\u01dc\5N(\2\u01d7\u01d8\7W\2\2\u01d8\u01d9\5B\"\2"+
		"\u01d9\u01da\7X\2\2\u01da\u01dc\3\2\2\2\u01db\u01d2\3\2\2\2\u01db\u01d5"+
		"\3\2\2\2\u01db\u01d6\3\2\2\2\u01db\u01d7\3\2\2\2\u01dc\u01e2\3\2\2\2\u01dd"+
		"\u01de\6\"\2\3\u01de\u01df\t\6\2\2\u01df\u01e1\5B\"\2\u01e0\u01dd\3\2"+
		"\2\2\u01e1\u01e4\3\2\2\2\u01e2\u01e0\3\2\2\2\u01e2\u01e3\3\2\2\2\u01e3"+
		"C\3\2\2\2\u01e4\u01e2\3\2\2\2\u01e5\u01e7\5F$\2\u01e6\u01e8\5R*\2\u01e7"+
		"\u01e6\3\2\2\2\u01e7\u01e8\3\2\2\2\u01e8\u01ea\3\2\2\2\u01e9\u01eb\5T"+
		"+\2\u01ea\u01e9\3\2\2\2\u01ea\u01eb\3\2\2\2\u01ebE\3\2\2\2\u01ec\u01ef"+
		"\5v<\2\u01ed\u01ef\5z>\2\u01ee\u01ec\3\2\2\2\u01ee\u01ed\3\2\2\2\u01ef"+
		"\u01f7\3\2\2\2\u01f0\u01f3\7_\2\2\u01f1\u01f4\5v<\2\u01f2\u01f4\5z>\2"+
		"\u01f3\u01f1\3\2\2\2\u01f3\u01f2\3\2\2\2\u01f4\u01f6\3\2\2\2\u01f5\u01f0"+
		"\3\2\2\2\u01f6\u01f9\3\2\2\2\u01f7\u01f5\3\2\2\2\u01f7\u01f8\3\2\2\2\u01f8"+
		"G\3\2\2\2\u01f9\u01f7\3\2\2\2\u01fa\u01fd\5b\62\2\u01fb\u01fc\7\60\2\2"+
		"\u01fc\u01fe\5|?\2\u01fd\u01fb\3\2\2\2\u01fd\u01fe\3\2\2\2\u01feI\3\2"+
		"\2\2\u01ff\u0202\5n8\2\u0200\u0201\7\60\2\2\u0201\u0203\5|?\2\u0202\u0200"+
		"\3\2\2\2\u0202\u0203\3\2\2\2\u0203K\3\2\2\2\u0204\u0205\5v<\2\u0205\u0206"+
		"\7V\2\2\u0206\u0207\5\u0092J\2\u0207M\3\2\2\2\u0208\u0209\5z>\2\u0209"+
		"\u020a\7V\2\2\u020a\u020b\5\u0092J\2\u020bO\3\2\2\2\u020c\u020d\7W\2\2"+
		"\u020d\u0212\5\u0092J\2\u020e\u020f\7_\2\2\u020f\u0211\5\u0092J\2\u0210"+
		"\u020e\3\2\2\2\u0211\u0214\3\2\2\2\u0212\u0210\3\2\2\2\u0212\u0213\3\2"+
		"\2\2\u0213\u0215\3\2\2\2\u0214\u0212\3\2\2\2\u0215\u0216\7X\2\2\u0216"+
		"Q\3\2\2\2\u0217\u0218\7D\2\2\u0218\u0219\5V,\2\u0219S\3\2\2\2\u021a\u021b"+
		"\7E\2\2\u021b\u021c\5V,\2\u021cU\3\2\2\2\u021d\u021e\7[\2\2\u021e\u0223"+
		"\5P)\2\u021f\u0220\7_\2\2\u0220\u0222\5P)\2\u0221\u021f\3\2\2\2\u0222"+
		"\u0225\3\2\2\2\u0223\u0221\3\2\2\2\u0223\u0224\3\2\2\2\u0224\u0226\3\2"+
		"\2\2\u0225\u0223\3\2\2\2\u0226\u0227\7\\\2\2\u0227W\3\2\2\2\u0228\u022b"+
		"\5\u00a2R\2\u0229\u022b\5z>\2\u022a\u0228\3\2\2\2\u022a\u0229\3\2\2\2"+
		"\u022bY\3\2\2\2\u022c\u0231\5\u0090I\2\u022d\u0231\5\u008cG\2\u022e\u0231"+
		"\5\u008eH\2\u022f\u0231\5\u0092J\2\u0230\u022c\3\2\2\2\u0230\u022d\3\2"+
		"\2\2\u0230\u022e\3\2\2\2\u0230\u022f\3\2\2\2\u0231[\3\2\2\2\u0232\u0233"+
		"\5\u009aN\2\u0233\u0234\7W\2\2\u0234\u0235\5r:\2\u0235\u0236\7X\2\2\u0236"+
		"]\3\2\2\2\u0237\u0238\5\u009cO\2\u0238\u0239\7W\2\2\u0239\u023a\5r:\2"+
		"\u023a\u023b\7X\2\2\u023b_\3\2\2\2\u023c\u023d\5\u0098M\2\u023d\u023e"+
		"\7W\2\2\u023e\u023f\5b\62\2\u023f\u0240\7X\2\2\u0240a\3\2\2\2\u0241\u0242"+
		"\b\62\1\2\u0242\u0243\5d\63\2\u0243\u024a\3\2\2\2\u0244\u0245\6\62\3\3"+
		"\u0245\u0246\5\u00a0Q\2\u0246\u0247\5d\63\2\u0247\u0249\3\2\2\2\u0248"+
		"\u0244\3\2\2\2\u0249\u024c\3\2\2\2\u024a\u0248\3\2\2\2\u024a\u024b\3\2"+
		"\2\2\u024bc\3\2\2\2\u024c\u024a\3\2\2\2\u024d\u024e\b\63\1\2\u024e\u0254"+
		"\5^\60\2\u024f\u0250\7W\2\2\u0250\u0251\5b\62\2\u0251\u0252\7X\2\2\u0252"+
		"\u0254\3\2\2\2\u0253\u024d\3\2\2\2\u0253\u024f\3\2\2\2\u0254\u025b\3\2"+
		"\2\2\u0255\u0256\6\63\4\3\u0256\u0257\5\u009eP\2\u0257\u0258\5d\63\2\u0258"+
		"\u025a\3\2\2\2\u0259\u0255\3\2\2\2\u025a\u025d\3\2\2\2\u025b\u0259\3\2"+
		"\2\2\u025b\u025c\3\2\2\2\u025ce\3\2\2\2\u025d\u025b\3\2\2\2\u025e\u025f"+
		"\b\64\1\2\u025f\u0260\5h\65\2\u0260\u0267\3\2\2\2\u0261\u0262\6\64\5\3"+
		"\u0262\u0263\5\u00a0Q\2\u0263\u0264\5h\65\2\u0264\u0266\3\2\2\2\u0265"+
		"\u0261\3\2\2\2\u0266\u0269\3\2\2\2\u0267\u0265\3\2\2\2\u0267\u0268\3\2"+
		"\2\2\u0268g\3\2\2\2\u0269\u0267\3\2\2\2\u026a\u026b\b\65\1\2\u026b\u0271"+
		"\5`\61\2\u026c\u026d\7W\2\2\u026d\u026e\5f\64\2\u026e\u026f\7X\2\2\u026f"+
		"\u0271\3\2\2\2\u0270\u026a\3\2\2\2\u0270\u026c\3\2\2\2\u0271\u0278\3\2"+
		"\2\2\u0272\u0273\6\65\6\3\u0273\u0274\5\u009eP\2\u0274\u0275\5h\65\2\u0275"+
		"\u0277\3\2\2\2\u0276\u0272\3\2\2\2\u0277\u027a\3\2\2\2\u0278\u0276\3\2"+
		"\2\2\u0278\u0279\3\2\2\2\u0279i\3\2\2\2\u027a\u0278\3\2\2\2\u027b\u027c"+
		"\b\66\1\2\u027c\u027d\5l\67\2\u027d\u0284\3\2\2\2\u027e\u027f\6\66\7\3"+
		"\u027f\u0280\5\u00a0Q\2\u0280\u0281\5l\67\2\u0281\u0283\3\2\2\2\u0282"+
		"\u027e\3\2\2\2\u0283\u0286\3\2\2\2\u0284\u0282\3\2\2\2\u0284\u0285\3\2"+
		"\2\2\u0285k\3\2\2\2\u0286\u0284\3\2\2\2\u0287\u0288\b\67\1\2\u0288\u028e"+
		"\5\\/\2\u0289\u028a\7W\2\2\u028a\u028b\5j\66\2\u028b\u028c\7X\2\2\u028c"+
		"\u028e\3\2\2\2\u028d\u0287\3\2\2\2\u028d\u0289\3\2\2\2\u028e\u0295\3\2"+
		"\2\2\u028f\u0290\6\67\b\3\u0290\u0291\5\u009eP\2\u0291\u0292\5l\67\2\u0292"+
		"\u0294\3\2\2\2\u0293\u028f\3\2\2\2\u0294\u0297\3\2\2\2\u0295\u0293\3\2"+
		"\2\2\u0295\u0296\3\2\2\2\u0296m\3\2\2\2\u0297\u0295\3\2\2\2\u0298\u0299"+
		"\b8\1\2\u0299\u029a\5p9\2\u029a\u02a1\3\2\2\2\u029b\u029c\68\t\3\u029c"+
		"\u029d\5\u00a0Q\2\u029d\u029e\5p9\2\u029e\u02a0\3\2\2\2\u029f\u029b\3"+
		"\2\2\2\u02a0\u02a3\3\2\2\2\u02a1\u029f\3\2\2\2\u02a1\u02a2\3\2\2\2\u02a2"+
		"o\3\2\2\2\u02a3\u02a1\3\2\2\2\u02a4\u02a5\b9\1\2\u02a5\u02ac\5f\64\2\u02a6"+
		"\u02ac\5j\66\2\u02a7\u02a8\7W\2\2\u02a8\u02a9\5p9\2\u02a9\u02aa\7X\2\2"+
		"\u02aa\u02ac\3\2\2\2\u02ab\u02a4\3\2\2\2\u02ab\u02a6\3\2\2\2\u02ab\u02a7"+
		"\3\2\2\2\u02ac\u02b3\3\2\2\2\u02ad\u02ae\69\n\3\u02ae\u02af\5\u009eP\2"+
		"\u02af\u02b0\5p9\2\u02b0\u02b2\3\2\2\2\u02b1\u02ad\3\2\2\2\u02b2\u02b5"+
		"\3\2\2\2\u02b3\u02b1\3\2\2\2\u02b3\u02b4\3\2\2\2\u02b4q\3\2\2\2\u02b5"+
		"\u02b3\3\2\2\2\u02b6\u02b7\b:\1\2\u02b7\u02b8\5t;\2\u02b8\u02bf\3\2\2"+
		"\2\u02b9\u02ba\6:\13\3\u02ba\u02bb\5\u00a0Q\2\u02bb\u02bc\5t;\2\u02bc"+
		"\u02be\3\2\2\2\u02bd\u02b9\3\2\2\2\u02be\u02c1\3\2\2\2\u02bf\u02bd\3\2"+
		"\2\2\u02bf\u02c0\3\2\2\2\u02c0s\3\2\2\2\u02c1\u02bf\3\2\2\2\u02c2\u02c3"+
		"\b;\1\2\u02c3\u02c9\5z>\2\u02c4\u02c5\7W\2\2\u02c5\u02c6\5r:\2\u02c6\u02c7"+
		"\7X\2\2\u02c7\u02c9\3\2\2\2\u02c8\u02c2\3\2\2\2\u02c8\u02c4\3\2\2\2\u02c9"+
		"\u02d0\3\2\2\2\u02ca\u02cb\6;\f\3\u02cb\u02cc\5\u009eP\2\u02cc\u02cd\5"+
		"t;\2\u02cd\u02cf\3\2\2\2\u02ce\u02ca\3\2\2\2\u02cf\u02d2\3\2\2\2\u02d0"+
		"\u02ce\3\2\2\2\u02d0\u02d1\3\2\2\2\u02d1u\3\2\2\2\u02d2\u02d0\3\2\2\2"+
		"\u02d3\u02d4\t\7\2\2\u02d4\u02d5\7`\2\2\u02d5\u02d6\t\7\2\2\u02d6\u02d7"+
		"\7`\2\2\u02d7\u02d8\t\7\2\2\u02d8w\3\2\2\2\u02d9\u02da\t\7\2\2\u02day"+
		"\3\2\2\2\u02db\u02dc\t\7\2\2\u02dc{\3\2\2\2\u02dd\u02de\t\7\2\2\u02de"+
		"}\3\2\2\2\u02df\u02e0\7]\2\2\u02e0\u02e1\7_\2\2\u02e1\u02e2\7]\2\2\u02e2"+
		"\177\3\2\2\2\u02e3\u02e4\7^\2\2\u02e4\u0081\3\2\2\2\u02e5\u02e6\7^\2\2"+
		"\u02e6\u0083\3\2\2\2\u02e7\u02e8\7^\2\2\u02e8\u02e9\7_\2\2\u02e9\u02ea"+
		"\7^\2\2\u02ea\u0085\3\2\2\2\u02eb\u02f0\7^\2\2\u02ec\u02ed\7_\2\2\u02ed"+
		"\u02ef\7^\2\2\u02ee\u02ec\3\2\2\2\u02ef\u02f2\3\2\2\2\u02f0\u02ee\3\2"+
		"\2\2\u02f0\u02f1\3\2\2\2\u02f1\u0087\3\2\2\2\u02f2\u02f0\3\2\2\2\u02f3"+
		"\u02f4\t\b\2\2\u02f4\u02f5\7_\2\2\u02f5\u02f6\t\b\2\2\u02f6\u0089\3\2"+
		"\2\2\u02f7\u02f8\t\t\2\2\u02f8\u02f9\7_\2\2\u02f9\u02fa\t\t\2\2\u02fa"+
		"\u008b\3\2\2\2\u02fb\u02fc\7]\2\2\u02fc\u008d\3\2\2\2\u02fd\u02fe\7^\2"+
		"\2\u02fe\u008f\3\2\2\2\u02ff\u0300\7\5\2\2\u0300\u0091\3\2\2\2\u0301\u0302"+
		"\t\n\2\2\u0302\u0093\3\2\2\2\u0303\u0304\t\13\2\2\u0304\u0095\3\2\2\2"+
		"\u0305\u0306\t\f\2\2\u0306\u0097\3\2\2\2\u0307\u0308\t\r\2\2\u0308\u0099"+
		"\3\2\2\2\u0309\u030a\t\r\2\2\u030a\u009b\3\2\2\2\u030b\u030c\t\16\2\2"+
		"\u030c\u009d\3\2\2\2\u030d\u030e\t\17\2\2\u030e\u009f\3\2\2\2\u030f\u0310"+
		"\t\20\2\2\u0310\u00a1\3\2\2\2\u0311\u0314\t\21\2\2\u0312\u0314\t\22\2"+
		"\2\u0313\u0311\3\2\2\2\u0313\u0312\3\2\2\2\u0314\u00a3\3\2\2\2\u0315\u0316"+
		"\t\23\2\2\u0316\u00a5\3\2\2\2\u0317\u0318\t\24\2\2\u0318\u00a7\3\2\2\2"+
		"\u0319\u031e\7\4\2\2\u031a\u031b\7_\2\2\u031b\u031d\7\4\2\2\u031c\u031a"+
		"\3\2\2\2\u031d\u0320\3\2\2\2\u031e\u031c\3\2\2\2\u031e\u031f\3\2\2\2\u031f"+
		"\u00a9\3\2\2\2\u0320\u031e\3\2\2\2D\u00b8\u00c2\u00c6\u00c8\u00ca\u00cf"+
		"\u00d1\u00e4\u00ee\u010b\u011a\u012f\u0137\u013a\u0142\u014c\u014f\u015f"+
		"\u0168\u0173\u017f\u0187\u018e\u0192\u0198\u019a\u01a2\u01a6\u01ac\u01b0"+
		"\u01b4\u01bb\u01c3\u01c9\u01ce\u01db\u01e2\u01e7\u01ea\u01ee\u01f3\u01f7"+
		"\u01fd\u0202\u0212\u0223\u022a\u0230\u024a\u0253\u025b\u0267\u0270\u0278"+
		"\u0284\u028d\u0295\u02a1\u02ab\u02b3\u02bf\u02c8\u02d0\u02f0\u0313\u031e";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}