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
		TYPE_TIMESERIES=26, TYPE_RECORDS=27, TYPE_MODELS=28, TYPE_MODEL=29, TYPE_VERSION=30, 
		TYPE_PERMISSIONS=31, TYPE_ROLES=32, TYPE_USERS=33, TYPE_PERMISSION=34, 
		TYPE_ROLE=35, TYPE_USER=36, OP_FROM=37, OP_OF=38, OP_ON=39, OP_TO=40, 
		OP_IN=41, OP_INTO=42, OP_SET=43, OP_VALUES=44, OP_ALIAS=45, OP_GROUPBY=46, 
		OP_FILTERBY=47, OP_TRANSPOSE=48, OP_IDONLY=49, OP_WITH=50, IR_EQUALTO=51, 
		IR_BEFORE=52, IR_AFTER=53, IR_MEETING=54, IR_OVERLAPPING=55, IR_DURING=56, 
		IR_WITHIN=57, IR_CONTAINING=58, IR_STARTINGWITH=59, IR_FINISHINGWITH=60, 
		LOGICAL_OR=61, LOGICAL_AND=62, LOGICAL_NOT=63, LOGICAL_IGNORE=64, LOGICAL_TRUE=65, 
		LOGICAL_FALSE=66, MATH_MULTIPLY=67, MATH_DIVISION=68, MATH_PLUS=69, MATH_MINUS=70, 
		AGGR_COUNTSTARTED=71, AGGR_COUNTFINISHED=72, AGGR_COUNT=73, AGGR_SUM=74, 
		AGGR_MIN=75, AGGR_MAX=76, AGGR_AVERAGE=77, AGGR_MODE=78, AGGR_MEAN=79, 
		AGGR_MEDIAN=80, CMP_EQUAL=81, BRACKET_ROUND_OPENED=82, BRACKET_ROUND_CLOSED=83, 
		BRACKET_SQUARE_OPENED=84, BRACKET_SQUARE_CLOSED=85, BRACKET_CURLY_OPENED=86, 
		BRACKET_CURLY_CLOSED=87, DATE=88, INT=89, SEPARATOR=90, DIMSEPARATOR=91, 
		SIMPLE_ID=92, ENHANCED_ID=93, WHITESPACE=94;
	public static final String[] tokenNames = {
		"<INVALID>", "MARKED_ID", "VALUE", "NULL_VALUE", "POS_START_INCL", "POS_END_INCL", 
		"POS_START_EXCL", "POS_END_EXCL", "STMT_GET", "STMT_SELECT", "STMT_INSERT", 
		"STMT_DELETE", "STMT_OPEN", "STMT_LOAD", "STMT_UNLOAD", "STMT_ALIVE", 
		"STMT_ADD", "STMT_DROP", "STMT_MODIFY", "STMT_GRANT", "STMT_REVOKE", "STMT_ASSIGN", 
		"STMT_REMOVE", "PROP_AUTOLOAD", "PROP_FORCE", "PROP_PASSWORD", "TYPE_TIMESERIES", 
		"TYPE_RECORDS", "TYPE_MODELS", "TYPE_MODEL", "TYPE_VERSION", "TYPE_PERMISSIONS", 
		"TYPE_ROLES", "TYPE_USERS", "TYPE_PERMISSION", "TYPE_ROLE", "TYPE_USER", 
		"OP_FROM", "OP_OF", "OP_ON", "OP_TO", "OP_IN", "OP_INTO", "OP_SET", "OP_VALUES", 
		"OP_ALIAS", "OP_GROUPBY", "OP_FILTERBY", "OP_TRANSPOSE", "OP_IDONLY", 
		"OP_WITH", "IR_EQUALTO", "IR_BEFORE", "IR_AFTER", "IR_MEETING", "IR_OVERLAPPING", 
		"IR_DURING", "IR_WITHIN", "IR_CONTAINING", "IR_STARTINGWITH", "IR_FINISHINGWITH", 
		"LOGICAL_OR", "LOGICAL_AND", "LOGICAL_NOT", "LOGICAL_IGNORE", "LOGICAL_TRUE", 
		"LOGICAL_FALSE", "'*'", "'/'", "'+'", "'-'", "AGGR_COUNTSTARTED", "AGGR_COUNTFINISHED", 
		"AGGR_COUNT", "AGGR_SUM", "AGGR_MIN", "AGGR_MAX", "AGGR_AVERAGE", "AGGR_MODE", 
		"AGGR_MEAN", "AGGR_MEDIAN", "'='", "'('", "')'", "'['", "']'", "'{'", 
		"'}'", "DATE", "INT", "','", "'.'", "SIMPLE_ID", "ENHANCED_ID", "WHITESPACE"
	};
	public static final int
		RULE_root = 0, RULE_exprAdd = 1, RULE_exprWithPassword = 2, RULE_exprWithPermissions = 3, 
		RULE_exprWithRoles = 4, RULE_exprDrop = 5, RULE_exprModify = 6, RULE_exprSetPassword = 7, 
		RULE_exprGrant = 8, RULE_exprRevoke = 9, RULE_exprAssign = 10, RULE_exprAssignSingleRole = 11, 
		RULE_exprAssignMultipleRoles = 12, RULE_exprRemove = 13, RULE_exprRemoveSingleRole = 14, 
		RULE_exprRemoveMultipleRoles = 15, RULE_exprGet = 16, RULE_exprAlive = 17, 
		RULE_exprLoad = 18, RULE_exprLoadSetProperty = 19, RULE_exprUnload = 20, 
		RULE_exprUnloadSetProperty = 21, RULE_exprInsert = 22, RULE_exprStructure = 23, 
		RULE_exprValues = 24, RULE_exprDelete = 25, RULE_exprSelect = 26, RULE_exprSelectRecords = 27, 
		RULE_exprSelectTimeSeries = 28, RULE_exprMeasure = 29, RULE_exprInterval = 30, 
		RULE_exprComp = 31, RULE_exprGroup = 32, RULE_exprAggregate = 33, RULE_compNamedLowMeasure = 34, 
		RULE_compNamedDimMathMeasure = 35, RULE_compMemberEqual = 36, RULE_compDescriptorEqual = 37, 
		RULE_compDescValueTupel = 38, RULE_compGroupIgnore = 39, RULE_compStructureElement = 40, 
		RULE_compValueElement = 41, RULE_compDimAggrFunction = 42, RULE_compLowAggrFunction = 43, 
		RULE_compMathAggrFunction = 44, RULE_compLowMeasure = 45, RULE_compLowMeasureAtom = 46, 
		RULE_compMathMeasure = 47, RULE_compMathMeasureAtom = 48, RULE_compDimMeasure = 49, 
		RULE_compDimMeasureAtom = 50, RULE_compDimMathMeasure = 51, RULE_compDimMathMeasureAtom = 52, 
		RULE_compDescriptorFormula = 53, RULE_compDescriptorFormulaAtom = 54, 
		RULE_selectorMember = 55, RULE_selectorModelId = 56, RULE_selectorDescriptorId = 57, 
		RULE_selectorAlias = 58, RULE_selectorDateInterval = 59, RULE_selectorIntInterval = 60, 
		RULE_selectorIntIdList = 61, RULE_selectorDateIntervalWithNull = 62, RULE_selectorIntIntervalWithNull = 63, 
		RULE_selectorDateValueOrNull = 64, RULE_selectorIntValueOrNull = 65, RULE_selectorOpenInterval = 66, 
		RULE_selectorCloseInterval = 67, RULE_selectorValue = 68, RULE_selectorMathAggrFunctionName = 69, 
		RULE_selectorDimAggrFunctionName = 70, RULE_selectorLowAggrFunctionName = 71, 
		RULE_selectorFirstMathOperator = 72, RULE_selectorSecondMathOperator = 73, 
		RULE_selectorIntervalDef = 74, RULE_selectorBoolean = 75, RULE_selectorIntervalRelation = 76, 
		RULE_selectorValueList = 77;
	public static final String[] ruleNames = {
		"root", "exprAdd", "exprWithPassword", "exprWithPermissions", "exprWithRoles", 
		"exprDrop", "exprModify", "exprSetPassword", "exprGrant", "exprRevoke", 
		"exprAssign", "exprAssignSingleRole", "exprAssignMultipleRoles", "exprRemove", 
		"exprRemoveSingleRole", "exprRemoveMultipleRoles", "exprGet", "exprAlive", 
		"exprLoad", "exprLoadSetProperty", "exprUnload", "exprUnloadSetProperty", 
		"exprInsert", "exprStructure", "exprValues", "exprDelete", "exprSelect", 
		"exprSelectRecords", "exprSelectTimeSeries", "exprMeasure", "exprInterval", 
		"exprComp", "exprGroup", "exprAggregate", "compNamedLowMeasure", "compNamedDimMathMeasure", 
		"compMemberEqual", "compDescriptorEqual", "compDescValueTupel", "compGroupIgnore", 
		"compStructureElement", "compValueElement", "compDimAggrFunction", "compLowAggrFunction", 
		"compMathAggrFunction", "compLowMeasure", "compLowMeasureAtom", "compMathMeasure", 
		"compMathMeasureAtom", "compDimMeasure", "compDimMeasureAtom", "compDimMathMeasure", 
		"compDimMathMeasureAtom", "compDescriptorFormula", "compDescriptorFormulaAtom", 
		"selectorMember", "selectorModelId", "selectorDescriptorId", "selectorAlias", 
		"selectorDateInterval", "selectorIntInterval", "selectorIntIdList", "selectorDateIntervalWithNull", 
		"selectorIntIntervalWithNull", "selectorDateValueOrNull", "selectorIntValueOrNull", 
		"selectorOpenInterval", "selectorCloseInterval", "selectorValue", "selectorMathAggrFunctionName", 
		"selectorDimAggrFunctionName", "selectorLowAggrFunctionName", "selectorFirstMathOperator", 
		"selectorSecondMathOperator", "selectorIntervalDef", "selectorBoolean", 
		"selectorIntervalRelation", "selectorValueList"
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
			setState(170);
			switch (_input.LA(1)) {
			case STMT_INSERT:
				{
				setState(156); exprInsert();
				}
				break;
			case STMT_DELETE:
				{
				setState(157); exprDelete();
				}
				break;
			case STMT_SELECT:
				{
				setState(158); exprSelect();
				}
				break;
			case STMT_OPEN:
			case STMT_LOAD:
				{
				setState(159); exprLoad();
				}
				break;
			case STMT_UNLOAD:
				{
				setState(160); exprUnload();
				}
				break;
			case STMT_ALIVE:
				{
				setState(161); exprAlive();
				}
				break;
			case STMT_GET:
				{
				setState(162); exprGet();
				}
				break;
			case STMT_ADD:
				{
				setState(163); exprAdd();
				}
				break;
			case STMT_DROP:
				{
				setState(164); exprDrop();
				}
				break;
			case STMT_MODIFY:
				{
				setState(165); exprModify();
				}
				break;
			case STMT_GRANT:
				{
				setState(166); exprGrant();
				}
				break;
			case STMT_REVOKE:
				{
				setState(167); exprRevoke();
				}
				break;
			case STMT_ASSIGN:
				{
				setState(168); exprAssign();
				}
				break;
			case STMT_REMOVE:
				{
				setState(169); exprRemove();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(172); match(EOF);
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
			setState(174); match(STMT_ADD);
			setState(195);
			switch (_input.LA(1)) {
			case TYPE_USER:
				{
				setState(175); match(TYPE_USER);
				setState(176); match(VALUE);
				setState(177); exprWithPassword();
				setState(188);
				_la = _input.LA(1);
				if (_la==OP_WITH) {
					{
					setState(186);
					switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
					case 1:
						{
						setState(178); exprWithRoles();
						setState(180);
						_la = _input.LA(1);
						if (_la==OP_WITH) {
							{
							setState(179); exprWithPermissions();
							}
						}

						}
						break;

					case 2:
						{
						setState(182); exprWithPermissions();
						setState(184);
						_la = _input.LA(1);
						if (_la==OP_WITH) {
							{
							setState(183); exprWithRoles();
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
				setState(190); match(TYPE_ROLE);
				setState(191); match(VALUE);
				setState(193);
				_la = _input.LA(1);
				if (_la==OP_WITH) {
					{
					setState(192); exprWithPermissions();
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
			setState(197); match(OP_WITH);
			setState(198); match(PROP_PASSWORD);
			setState(199); match(VALUE);
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
			setState(201); match(OP_WITH);
			setState(202); match(TYPE_PERMISSIONS);
			setState(203); selectorValueList();
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
			setState(205); match(OP_WITH);
			setState(206); match(TYPE_ROLES);
			setState(207); selectorValueList();
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
			setState(209); match(STMT_DROP);
			setState(214);
			switch (_input.LA(1)) {
			case TYPE_ROLE:
			case TYPE_USER:
				{
				{
				setState(210);
				_la = _input.LA(1);
				if ( !(_la==TYPE_ROLE || _la==TYPE_USER) ) {
				_errHandler.recoverInline(this);
				}
				consume();
				setState(211); match(VALUE);
				}
				}
				break;
			case TYPE_MODEL:
				{
				setState(212); match(TYPE_MODEL);
				setState(213); selectorModelId();
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
			setState(216); match(STMT_MODIFY);
			setState(217); match(TYPE_USER);
			setState(218); match(VALUE);
			setState(219); exprSetPassword();
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
		public TerminalNode OP_TO() { return getToken(QueryGrammarParser.OP_TO, 0); }
		public TerminalNode OP_SET() { return getToken(QueryGrammarParser.OP_SET, 0); }
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public TerminalNode PROP_PASSWORD() { return getToken(QueryGrammarParser.PROP_PASSWORD, 0); }
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
			setState(221); match(OP_SET);
			setState(222); match(PROP_PASSWORD);
			setState(223); match(OP_TO);
			setState(224); match(VALUE);
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
		enterRule(_localctx, 16, RULE_exprGrant);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(226); match(STMT_GRANT);
			setState(227); match(TYPE_PERMISSIONS);
			setState(228); selectorValueList();
			setState(229); match(OP_TO);
			setState(230);
			_la = _input.LA(1);
			if ( !(_la==TYPE_ROLE || _la==TYPE_USER) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(231); match(VALUE);
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
		enterRule(_localctx, 18, RULE_exprRevoke);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(233); match(STMT_REVOKE);
			setState(234); match(TYPE_PERMISSIONS);
			setState(235); selectorValueList();
			setState(236); match(OP_FROM);
			setState(237);
			_la = _input.LA(1);
			if ( !(_la==TYPE_ROLE || _la==TYPE_USER) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(238); match(VALUE);
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
		enterRule(_localctx, 20, RULE_exprAssign);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(240); match(STMT_ASSIGN);
			setState(243);
			switch (_input.LA(1)) {
			case TYPE_ROLE:
				{
				setState(241); exprAssignSingleRole();
				}
				break;
			case TYPE_ROLES:
				{
				setState(242); exprAssignMultipleRoles();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(245); match(OP_TO);
			setState(246); match(TYPE_USER);
			setState(247); match(VALUE);
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
		enterRule(_localctx, 22, RULE_exprAssignSingleRole);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(249); match(TYPE_ROLE);
			setState(250); match(VALUE);
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
		enterRule(_localctx, 24, RULE_exprAssignMultipleRoles);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(252); match(TYPE_ROLES);
			setState(253); selectorValueList();
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
		enterRule(_localctx, 26, RULE_exprRemove);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(255); match(STMT_REMOVE);
			setState(258);
			switch (_input.LA(1)) {
			case TYPE_ROLE:
				{
				setState(256); exprRemoveSingleRole();
				}
				break;
			case TYPE_ROLES:
				{
				setState(257); exprRemoveMultipleRoles();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(260); match(OP_FROM);
			setState(261); match(TYPE_USER);
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
		enterRule(_localctx, 28, RULE_exprRemoveSingleRole);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(264); match(TYPE_ROLE);
			setState(265); match(VALUE);
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
		enterRule(_localctx, 30, RULE_exprRemoveMultipleRoles);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(267); match(TYPE_ROLES);
			setState(268); selectorValueList();
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
		enterRule(_localctx, 32, RULE_exprGet);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(270); match(STMT_GET);
			setState(271);
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
		enterRule(_localctx, 34, RULE_exprAlive);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(273); match(STMT_ALIVE);
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
		enterRule(_localctx, 36, RULE_exprLoad);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(275);
			_la = _input.LA(1);
			if ( !(_la==STMT_OPEN || _la==STMT_LOAD) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(279);
			switch (_input.LA(1)) {
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				{
				setState(276); selectorModelId();
				}
				break;
			case OP_FROM:
				{
				{
				setState(277); match(OP_FROM);
				setState(278); match(VALUE);
				}
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(290);
			_la = _input.LA(1);
			if (_la==OP_SET) {
				{
				setState(281); match(OP_SET);
				setState(282); exprLoadSetProperty();
				setState(287);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(283); match(SEPARATOR);
					setState(284); exprLoadSetProperty();
					}
					}
					setState(289);
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
		enterRule(_localctx, 38, RULE_exprLoadSetProperty);
		try {
			setState(298);
			switch (_input.LA(1)) {
			case PROP_AUTOLOAD:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(292); match(PROP_AUTOLOAD);
				setState(293); match(CMP_EQUAL);
				setState(294); selectorBoolean();
				}
				}
				break;
			case PROP_FORCE:
				enterOuterAlt(_localctx, 2);
				{
				{
				setState(295); match(PROP_FORCE);
				setState(296); match(CMP_EQUAL);
				setState(297); selectorBoolean();
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
		enterRule(_localctx, 40, RULE_exprUnload);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(300); match(STMT_UNLOAD);
			setState(301); selectorModelId();
			setState(311);
			_la = _input.LA(1);
			if (_la==OP_SET) {
				{
				setState(302); match(OP_SET);
				setState(303); exprUnloadSetProperty();
				setState(308);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(304); match(SEPARATOR);
					setState(305); exprUnloadSetProperty();
					}
					}
					setState(310);
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
		enterRule(_localctx, 42, RULE_exprUnloadSetProperty);
		try {
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(313); match(PROP_AUTOLOAD);
			setState(314); match(CMP_EQUAL);
			setState(315); selectorBoolean();
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
		enterRule(_localctx, 44, RULE_exprInsert);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(317); match(STMT_INSERT);
			setState(318); match(OP_INTO);
			setState(319); selectorModelId();
			setState(320); exprStructure();
			setState(321); match(OP_VALUES);
			setState(322); exprValues();
			setState(327);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(323); match(SEPARATOR);
				setState(324); exprValues();
				}
				}
				setState(329);
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
		enterRule(_localctx, 46, RULE_exprStructure);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(330); match(BRACKET_ROUND_OPENED);
			setState(331); compStructureElement();
			setState(336);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(332); match(SEPARATOR);
				setState(333); compStructureElement();
				}
				}
				setState(338);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(339); match(BRACKET_ROUND_CLOSED);
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
		enterRule(_localctx, 48, RULE_exprValues);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(341); match(BRACKET_ROUND_OPENED);
			setState(342); compValueElement();
			setState(347);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(343); match(SEPARATOR);
				setState(344); compValueElement();
				}
				}
				setState(349);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(350); match(BRACKET_ROUND_CLOSED);
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
		enterRule(_localctx, 50, RULE_exprDelete);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(352); match(STMT_DELETE);
			setState(353); selectorIntIdList();
			setState(354); match(OP_FROM);
			setState(355); selectorModelId();
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
		enterRule(_localctx, 52, RULE_exprSelect);
		try {
			setState(359);
			switch ( getInterpreter().adaptivePredict(_input,19,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(357); exprSelectRecords();
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(358); exprSelectTimeSeries();
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
		public TerminalNode OP_IDONLY() { return getToken(QueryGrammarParser.OP_IDONLY, 0); }
		public ExprCompContext exprComp() {
			return getRuleContext(ExprCompContext.class,0);
		}
		public SelectorIntervalRelationContext selectorIntervalRelation() {
			return getRuleContext(SelectorIntervalRelationContext.class,0);
		}
		public TerminalNode OP_FROM() { return getToken(QueryGrammarParser.OP_FROM, 0); }
		public ExprIntervalContext exprInterval() {
			return getRuleContext(ExprIntervalContext.class,0);
		}
		public TerminalNode STMT_SELECT() { return getToken(QueryGrammarParser.STMT_SELECT, 0); }
		public TerminalNode OP_FILTERBY() { return getToken(QueryGrammarParser.OP_FILTERBY, 0); }
		public TerminalNode TYPE_RECORDS() { return getToken(QueryGrammarParser.TYPE_RECORDS, 0); }
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public TerminalNode AGGR_COUNT() { return getToken(QueryGrammarParser.AGGR_COUNT, 0); }
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
		enterRule(_localctx, 54, RULE_exprSelectRecords);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(361); match(STMT_SELECT);
			setState(367);
			switch (_input.LA(1)) {
			case TYPE_RECORDS:
				{
				setState(362); match(TYPE_RECORDS);
				}
				break;
			case OP_IDONLY:
			case AGGR_COUNT:
				{
				setState(363);
				_la = _input.LA(1);
				if ( !(_la==OP_IDONLY || _la==AGGR_COUNT) ) {
				_errHandler.recoverInline(this);
				}
				consume();
				setState(364); match(BRACKET_ROUND_OPENED);
				setState(365); match(TYPE_RECORDS);
				setState(366); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(369); match(OP_FROM);
			setState(370); selectorModelId();
			setState(374);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << IR_EQUALTO) | (1L << IR_BEFORE) | (1L << IR_AFTER) | (1L << IR_MEETING) | (1L << IR_OVERLAPPING) | (1L << IR_DURING) | (1L << IR_WITHIN) | (1L << IR_CONTAINING) | (1L << IR_STARTINGWITH) | (1L << IR_FINISHINGWITH))) != 0)) {
				{
				setState(371); selectorIntervalRelation();
				setState(372); exprInterval();
				}
			}

			setState(378);
			_la = _input.LA(1);
			if (_la==OP_FILTERBY) {
				{
				setState(376); match(OP_FILTERBY);
				setState(377); exprComp(0);
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
		enterRule(_localctx, 56, RULE_exprSelectTimeSeries);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(380); match(STMT_SELECT);
			setState(386);
			switch (_input.LA(1)) {
			case TYPE_TIMESERIES:
				{
				setState(381); match(TYPE_TIMESERIES);
				}
				break;
			case OP_TRANSPOSE:
				{
				setState(382); match(OP_TRANSPOSE);
				setState(383); match(BRACKET_ROUND_OPENED);
				setState(384); match(TYPE_TIMESERIES);
				setState(385); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(390);
			_la = _input.LA(1);
			if (_la==OP_OF) {
				{
				setState(388); match(OP_OF);
				setState(389); exprMeasure();
				}
			}

			setState(392); match(OP_FROM);
			setState(393); selectorModelId();
			setState(396);
			_la = _input.LA(1);
			if (_la==OP_IN) {
				{
				setState(394); match(OP_IN);
				setState(395); exprInterval();
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

			setState(404);
			_la = _input.LA(1);
			if (_la==OP_GROUPBY) {
				{
				setState(402); match(OP_GROUPBY);
				setState(403); exprGroup();
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
		enterRule(_localctx, 58, RULE_exprMeasure);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(425);
			switch ( getInterpreter().adaptivePredict(_input,30,_ctx) ) {
			case 1:
				{
				setState(406); compNamedLowMeasure();
				setState(411);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(407); match(SEPARATOR);
					setState(408); compNamedLowMeasure();
					}
					}
					setState(413);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
				break;

			case 2:
				{
				{
				setState(414); compNamedDimMathMeasure();
				setState(419);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(415); match(SEPARATOR);
					setState(416); compNamedDimMathMeasure();
					}
					}
					setState(421);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(422); match(OP_ON);
				setState(423); selectorMember();
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
		enterRule(_localctx, 60, RULE_exprInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(427); selectorOpenInterval();
			setState(430);
			switch (_input.LA(1)) {
			case DATE:
				{
				setState(428); selectorDateInterval();
				}
				break;
			case INT:
				{
				setState(429); selectorIntInterval();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(432); selectorCloseInterval();
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
		int _startState = 62;
		enterRecursionRule(_localctx, RULE_exprComp);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(443);
			switch ( getInterpreter().adaptivePredict(_input,32,_ctx) ) {
			case 1:
				{
				setState(435); match(LOGICAL_NOT);
				setState(436); exprComp(2);
				}
				break;

			case 2:
				{
				setState(437); compMemberEqual();
				}
				break;

			case 3:
				{
				setState(438); compDescriptorEqual();
				}
				break;

			case 4:
				{
				setState(439); match(BRACKET_ROUND_OPENED);
				setState(440); exprComp(0);
				setState(441); match(BRACKET_ROUND_CLOSED);
				}
				break;
			}
			_ctx.stop = _input.LT(-1);
			setState(450);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,33,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new ExprCompContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_exprComp);
					setState(445);
					if (!(1 >= _localctx._p)) throw new FailedPredicateException(this, "1 >= $_p");
					setState(446);
					_la = _input.LA(1);
					if ( !(_la==LOGICAL_OR || _la==LOGICAL_AND) ) {
					_errHandler.recoverInline(this);
					}
					consume();
					setState(447); exprComp(2);
					}
					} 
				}
				setState(452);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,33,_ctx);
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
		public CompGroupIgnoreContext compGroupIgnore() {
			return getRuleContext(CompGroupIgnoreContext.class,0);
		}
		public ExprAggregateContext exprAggregate() {
			return getRuleContext(ExprAggregateContext.class,0);
		}
		public TerminalNode LOGICAL_IGNORE() { return getToken(QueryGrammarParser.LOGICAL_IGNORE, 0); }
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
		enterRule(_localctx, 64, RULE_exprGroup);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(453); exprAggregate();
			setState(456);
			_la = _input.LA(1);
			if (_la==LOGICAL_IGNORE) {
				{
				setState(454); match(LOGICAL_IGNORE);
				setState(455); compGroupIgnore();
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
		enterRule(_localctx, 66, RULE_exprAggregate);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(460);
			switch ( getInterpreter().adaptivePredict(_input,35,_ctx) ) {
			case 1:
				{
				setState(458); selectorMember();
				}
				break;

			case 2:
				{
				setState(459); selectorDescriptorId();
				}
				break;
			}
			setState(469);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(462); match(SEPARATOR);
				setState(465);
				switch ( getInterpreter().adaptivePredict(_input,36,_ctx) ) {
				case 1:
					{
					setState(463); selectorMember();
					}
					break;

				case 2:
					{
					setState(464); selectorDescriptorId();
					}
					break;
				}
				}
				}
				setState(471);
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
		enterRule(_localctx, 68, RULE_compNamedLowMeasure);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(472); compLowMeasure(0);
			setState(475);
			_la = _input.LA(1);
			if (_la==OP_ALIAS) {
				{
				setState(473); match(OP_ALIAS);
				setState(474); selectorAlias();
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
		enterRule(_localctx, 70, RULE_compNamedDimMathMeasure);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(477); compDimMathMeasure(0);
			setState(480);
			_la = _input.LA(1);
			if (_la==OP_ALIAS) {
				{
				setState(478); match(OP_ALIAS);
				setState(479); selectorAlias();
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
		enterRule(_localctx, 72, RULE_compMemberEqual);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(482); selectorMember();
			setState(483); match(CMP_EQUAL);
			setState(484); selectorValue();
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
		enterRule(_localctx, 74, RULE_compDescriptorEqual);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(486); selectorDescriptorId();
			setState(487); match(CMP_EQUAL);
			setState(488); selectorValue();
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
		enterRule(_localctx, 76, RULE_compDescValueTupel);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(490); match(BRACKET_ROUND_OPENED);
			setState(491); selectorValue();
			setState(496);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(492); match(SEPARATOR);
				setState(493); selectorValue();
				}
				}
				setState(498);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(499); match(BRACKET_ROUND_CLOSED);
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

	public static class CompGroupIgnoreContext extends ParserRuleContext {
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
		public CompGroupIgnoreContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compGroupIgnore; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompGroupIgnore(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompGroupIgnore(this);
		}
	}

	public final CompGroupIgnoreContext compGroupIgnore() throws RecognitionException {
		CompGroupIgnoreContext _localctx = new CompGroupIgnoreContext(_ctx, getState());
		enterRule(_localctx, 78, RULE_compGroupIgnore);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(501); match(BRACKET_CURLY_OPENED);
			setState(502); compDescValueTupel();
			setState(507);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(503); match(SEPARATOR);
				setState(504); compDescValueTupel();
				}
				}
				setState(509);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(510); match(BRACKET_CURLY_CLOSED);
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
		enterRule(_localctx, 80, RULE_compStructureElement);
		try {
			setState(514);
			switch (_input.LA(1)) {
			case POS_START_INCL:
			case POS_END_INCL:
			case POS_START_EXCL:
			case POS_END_EXCL:
				enterOuterAlt(_localctx, 1);
				{
				setState(512); selectorIntervalDef();
				}
				break;
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				enterOuterAlt(_localctx, 2);
				{
				setState(513); selectorDescriptorId();
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
		public SelectorIntValueOrNullContext selectorIntValueOrNull() {
			return getRuleContext(SelectorIntValueOrNullContext.class,0);
		}
		public SelectorDateValueOrNullContext selectorDateValueOrNull() {
			return getRuleContext(SelectorDateValueOrNullContext.class,0);
		}
		public SelectorValueContext selectorValue() {
			return getRuleContext(SelectorValueContext.class,0);
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
		enterRule(_localctx, 82, RULE_compValueElement);
		try {
			setState(519);
			switch ( getInterpreter().adaptivePredict(_input,43,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(516); selectorDateValueOrNull();
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(517); selectorIntValueOrNull();
				}
				break;

			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(518); selectorValue();
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
		enterRule(_localctx, 84, RULE_compDimAggrFunction);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(521); selectorDimAggrFunctionName();
			setState(522); match(BRACKET_ROUND_OPENED);
			setState(523); compDescriptorFormula(0);
			setState(524); match(BRACKET_ROUND_CLOSED);
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
		enterRule(_localctx, 86, RULE_compLowAggrFunction);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(526); selectorLowAggrFunctionName();
			setState(527); match(BRACKET_ROUND_OPENED);
			setState(528); compDescriptorFormula(0);
			setState(529); match(BRACKET_ROUND_CLOSED);
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
		enterRule(_localctx, 88, RULE_compMathAggrFunction);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(531); selectorMathAggrFunctionName();
			setState(532); match(BRACKET_ROUND_OPENED);
			setState(533); compLowMeasure(0);
			setState(534); match(BRACKET_ROUND_CLOSED);
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
		int _startState = 90;
		enterRecursionRule(_localctx, RULE_compLowMeasure);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(537); compLowMeasureAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(545);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,44,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompLowMeasureContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compLowMeasure);
					setState(539);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(540); selectorSecondMathOperator();
					setState(541); compLowMeasureAtom(0);
					}
					} 
				}
				setState(547);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,44,_ctx);
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
		int _startState = 92;
		enterRecursionRule(_localctx, RULE_compLowMeasureAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(554);
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
				setState(549); compLowAggrFunction();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(550); match(BRACKET_ROUND_OPENED);
				setState(551); compLowMeasure(0);
				setState(552); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(562);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,46,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompLowMeasureAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compLowMeasureAtom);
					setState(556);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(557); selectorFirstMathOperator();
					setState(558); compLowMeasureAtom(0);
					}
					} 
				}
				setState(564);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,46,_ctx);
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
		int _startState = 94;
		enterRecursionRule(_localctx, RULE_compMathMeasure);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(566); compMathMeasureAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(574);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,47,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompMathMeasureContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compMathMeasure);
					setState(568);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(569); selectorSecondMathOperator();
					setState(570); compMathMeasureAtom(0);
					}
					} 
				}
				setState(576);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,47,_ctx);
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
		int _startState = 96;
		enterRecursionRule(_localctx, RULE_compMathMeasureAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(583);
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
				setState(578); compMathAggrFunction();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(579); match(BRACKET_ROUND_OPENED);
				setState(580); compMathMeasure(0);
				setState(581); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(591);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,49,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompMathMeasureAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compMathMeasureAtom);
					setState(585);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(586); selectorFirstMathOperator();
					setState(587); compMathMeasureAtom(0);
					}
					} 
				}
				setState(593);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,49,_ctx);
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
		int _startState = 98;
		enterRecursionRule(_localctx, RULE_compDimMeasure);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(595); compDimMeasureAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(603);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,50,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDimMeasureContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDimMeasure);
					setState(597);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(598); selectorSecondMathOperator();
					setState(599); compDimMeasureAtom(0);
					}
					} 
				}
				setState(605);
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
		int _startState = 100;
		enterRecursionRule(_localctx, RULE_compDimMeasureAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(612);
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
				setState(607); compDimAggrFunction();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(608); match(BRACKET_ROUND_OPENED);
				setState(609); compDimMeasure(0);
				setState(610); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(620);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,52,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDimMeasureAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDimMeasureAtom);
					setState(614);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(615); selectorFirstMathOperator();
					setState(616); compDimMeasureAtom(0);
					}
					} 
				}
				setState(622);
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
		int _startState = 102;
		enterRecursionRule(_localctx, RULE_compDimMathMeasure);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(624); compDimMathMeasureAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(632);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,53,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDimMathMeasureContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDimMathMeasure);
					setState(626);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(627); selectorSecondMathOperator();
					setState(628); compDimMathMeasureAtom(0);
					}
					} 
				}
				setState(634);
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
		int _startState = 104;
		enterRecursionRule(_localctx, RULE_compDimMathMeasureAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(642);
			switch ( getInterpreter().adaptivePredict(_input,54,_ctx) ) {
			case 1:
				{
				setState(636); compMathMeasure(0);
				}
				break;

			case 2:
				{
				setState(637); compDimMeasure(0);
				}
				break;

			case 3:
				{
				setState(638); match(BRACKET_ROUND_OPENED);
				setState(639); compDimMathMeasureAtom(0);
				setState(640); match(BRACKET_ROUND_CLOSED);
				}
				break;
			}
			_ctx.stop = _input.LT(-1);
			setState(650);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,55,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDimMathMeasureAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDimMathMeasureAtom);
					setState(644);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(645); selectorFirstMathOperator();
					setState(646); compDimMathMeasureAtom(0);
					}
					} 
				}
				setState(652);
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
		int _startState = 106;
		enterRecursionRule(_localctx, RULE_compDescriptorFormula);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(654); compDescriptorFormulaAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(662);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,56,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDescriptorFormulaContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDescriptorFormula);
					setState(656);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(657); selectorSecondMathOperator();
					setState(658); compDescriptorFormulaAtom(0);
					}
					} 
				}
				setState(664);
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
		int _startState = 108;
		enterRecursionRule(_localctx, RULE_compDescriptorFormulaAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(671);
			switch (_input.LA(1)) {
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				{
				setState(666); selectorDescriptorId();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(667); match(BRACKET_ROUND_OPENED);
				setState(668); compDescriptorFormula(0);
				setState(669); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(679);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,58,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDescriptorFormulaAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDescriptorFormulaAtom);
					setState(673);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(674); selectorFirstMathOperator();
					setState(675); compDescriptorFormulaAtom(0);
					}
					} 
				}
				setState(681);
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
		enterRule(_localctx, 110, RULE_selectorMember);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(682);
			_la = _input.LA(1);
			if ( !(_la==MARKED_ID || _la==SIMPLE_ID || _la==ENHANCED_ID) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(683); match(DIMSEPARATOR);
			setState(684);
			_la = _input.LA(1);
			if ( !(_la==MARKED_ID || _la==SIMPLE_ID || _la==ENHANCED_ID) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(685); match(DIMSEPARATOR);
			setState(686);
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
		enterRule(_localctx, 112, RULE_selectorModelId);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(688);
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
		enterRule(_localctx, 114, RULE_selectorDescriptorId);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(690);
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
		enterRule(_localctx, 116, RULE_selectorAlias);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(692);
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
		enterRule(_localctx, 118, RULE_selectorDateInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(694); match(DATE);
			setState(695); match(SEPARATOR);
			setState(696); match(DATE);
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
		enterRule(_localctx, 120, RULE_selectorIntInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(698); match(INT);
			setState(699); match(SEPARATOR);
			setState(700); match(INT);
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
		enterRule(_localctx, 122, RULE_selectorIntIdList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(702); match(INT);
			setState(707);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(703); match(SEPARATOR);
				setState(704); match(INT);
				}
				}
				setState(709);
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
		enterRule(_localctx, 124, RULE_selectorDateIntervalWithNull);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(710);
			_la = _input.LA(1);
			if ( !(_la==NULL_VALUE || _la==DATE) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(711); match(SEPARATOR);
			setState(712);
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
		enterRule(_localctx, 126, RULE_selectorIntIntervalWithNull);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(714);
			_la = _input.LA(1);
			if ( !(_la==NULL_VALUE || _la==INT) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(715); match(SEPARATOR);
			setState(716);
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

	public static class SelectorDateValueOrNullContext extends ParserRuleContext {
		public TerminalNode DATE() { return getToken(QueryGrammarParser.DATE, 0); }
		public TerminalNode NULL_VALUE() { return getToken(QueryGrammarParser.NULL_VALUE, 0); }
		public SelectorDateValueOrNullContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorDateValueOrNull; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorDateValueOrNull(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorDateValueOrNull(this);
		}
	}

	public final SelectorDateValueOrNullContext selectorDateValueOrNull() throws RecognitionException {
		SelectorDateValueOrNullContext _localctx = new SelectorDateValueOrNullContext(_ctx, getState());
		enterRule(_localctx, 128, RULE_selectorDateValueOrNull);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(718);
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

	public static class SelectorIntValueOrNullContext extends ParserRuleContext {
		public TerminalNode INT() { return getToken(QueryGrammarParser.INT, 0); }
		public TerminalNode NULL_VALUE() { return getToken(QueryGrammarParser.NULL_VALUE, 0); }
		public SelectorIntValueOrNullContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorIntValueOrNull; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorIntValueOrNull(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorIntValueOrNull(this);
		}
	}

	public final SelectorIntValueOrNullContext selectorIntValueOrNull() throws RecognitionException {
		SelectorIntValueOrNullContext _localctx = new SelectorIntValueOrNullContext(_ctx, getState());
		enterRule(_localctx, 130, RULE_selectorIntValueOrNull);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(720);
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
		enterRule(_localctx, 132, RULE_selectorOpenInterval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(722);
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
		enterRule(_localctx, 134, RULE_selectorCloseInterval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(724);
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
		enterRule(_localctx, 136, RULE_selectorValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(726);
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
		enterRule(_localctx, 138, RULE_selectorMathAggrFunctionName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(728);
			_la = _input.LA(1);
			if ( !(((((_la - 73)) & ~0x3f) == 0 && ((1L << (_la - 73)) & ((1L << (AGGR_COUNT - 73)) | (1L << (AGGR_SUM - 73)) | (1L << (AGGR_MIN - 73)) | (1L << (AGGR_MAX - 73)) | (1L << (AGGR_AVERAGE - 73)) | (1L << (AGGR_MODE - 73)) | (1L << (AGGR_MEAN - 73)) | (1L << (AGGR_MEDIAN - 73)) | (1L << (SIMPLE_ID - 73)))) != 0)) ) {
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
		enterRule(_localctx, 140, RULE_selectorDimAggrFunctionName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(730);
			_la = _input.LA(1);
			if ( !(((((_la - 73)) & ~0x3f) == 0 && ((1L << (_la - 73)) & ((1L << (AGGR_COUNT - 73)) | (1L << (AGGR_SUM - 73)) | (1L << (AGGR_MIN - 73)) | (1L << (AGGR_MAX - 73)) | (1L << (AGGR_AVERAGE - 73)) | (1L << (AGGR_MODE - 73)) | (1L << (AGGR_MEAN - 73)) | (1L << (AGGR_MEDIAN - 73)) | (1L << (SIMPLE_ID - 73)))) != 0)) ) {
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
		enterRule(_localctx, 142, RULE_selectorLowAggrFunctionName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(732);
			_la = _input.LA(1);
			if ( !(((((_la - 71)) & ~0x3f) == 0 && ((1L << (_la - 71)) & ((1L << (AGGR_COUNTSTARTED - 71)) | (1L << (AGGR_COUNTFINISHED - 71)) | (1L << (AGGR_COUNT - 71)) | (1L << (AGGR_SUM - 71)) | (1L << (AGGR_MIN - 71)) | (1L << (AGGR_MAX - 71)) | (1L << (AGGR_AVERAGE - 71)) | (1L << (AGGR_MODE - 71)) | (1L << (AGGR_MEAN - 71)) | (1L << (AGGR_MEDIAN - 71)) | (1L << (SIMPLE_ID - 71)))) != 0)) ) {
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
		enterRule(_localctx, 144, RULE_selectorFirstMathOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(734);
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
		enterRule(_localctx, 146, RULE_selectorSecondMathOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(736);
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
		enterRule(_localctx, 148, RULE_selectorIntervalDef);
		int _la;
		try {
			setState(740);
			switch (_input.LA(1)) {
			case POS_START_INCL:
			case POS_START_EXCL:
				enterOuterAlt(_localctx, 1);
				{
				setState(738);
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
				setState(739);
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
		enterRule(_localctx, 150, RULE_selectorBoolean);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(742);
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
		enterRule(_localctx, 152, RULE_selectorIntervalRelation);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(744);
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
		enterRule(_localctx, 154, RULE_selectorValueList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(746); match(VALUE);
			setState(751);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(747); match(SEPARATOR);
				setState(748); match(VALUE);
				}
				}
				setState(753);
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
		case 31: return exprComp_sempred((ExprCompContext)_localctx, predIndex);

		case 45: return compLowMeasure_sempred((CompLowMeasureContext)_localctx, predIndex);

		case 46: return compLowMeasureAtom_sempred((CompLowMeasureAtomContext)_localctx, predIndex);

		case 47: return compMathMeasure_sempred((CompMathMeasureContext)_localctx, predIndex);

		case 48: return compMathMeasureAtom_sempred((CompMathMeasureAtomContext)_localctx, predIndex);

		case 49: return compDimMeasure_sempred((CompDimMeasureContext)_localctx, predIndex);

		case 50: return compDimMeasureAtom_sempred((CompDimMeasureAtomContext)_localctx, predIndex);

		case 51: return compDimMathMeasure_sempred((CompDimMathMeasureContext)_localctx, predIndex);

		case 52: return compDimMathMeasureAtom_sempred((CompDimMathMeasureAtomContext)_localctx, predIndex);

		case 53: return compDescriptorFormula_sempred((CompDescriptorFormulaContext)_localctx, predIndex);

		case 54: return compDescriptorFormulaAtom_sempred((CompDescriptorFormulaAtomContext)_localctx, predIndex);
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
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\3`\u02f5\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t+\4"+
		",\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t\61\4\62\t\62\4\63\t\63\4\64\t"+
		"\64\4\65\t\65\4\66\t\66\4\67\t\67\48\t8\49\t9\4:\t:\4;\t;\4<\t<\4=\t="+
		"\4>\t>\4?\t?\4@\t@\4A\tA\4B\tB\4C\tC\4D\tD\4E\tE\4F\tF\4G\tG\4H\tH\4I"+
		"\tI\4J\tJ\4K\tK\4L\tL\4M\tM\4N\tN\4O\tO\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3"+
		"\2\3\2\3\2\3\2\3\2\3\2\3\2\5\2\u00ad\n\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3"+
		"\3\5\3\u00b7\n\3\3\3\3\3\5\3\u00bb\n\3\5\3\u00bd\n\3\5\3\u00bf\n\3\3\3"+
		"\3\3\3\3\5\3\u00c4\n\3\5\3\u00c6\n\3\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3"+
		"\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\5\7\u00d9\n\7\3\b\3\b\3\b\3\b\3\b\3"+
		"\t\3\t\3\t\3\t\3\t\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13"+
		"\3\13\3\13\3\f\3\f\3\f\5\f\u00f6\n\f\3\f\3\f\3\f\3\f\3\r\3\r\3\r\3\16"+
		"\3\16\3\16\3\17\3\17\3\17\5\17\u0105\n\17\3\17\3\17\3\17\3\17\3\20\3\20"+
		"\3\20\3\21\3\21\3\21\3\22\3\22\3\22\3\23\3\23\3\24\3\24\3\24\3\24\5\24"+
		"\u011a\n\24\3\24\3\24\3\24\3\24\7\24\u0120\n\24\f\24\16\24\u0123\13\24"+
		"\5\24\u0125\n\24\3\25\3\25\3\25\3\25\3\25\3\25\5\25\u012d\n\25\3\26\3"+
		"\26\3\26\3\26\3\26\3\26\7\26\u0135\n\26\f\26\16\26\u0138\13\26\5\26\u013a"+
		"\n\26\3\27\3\27\3\27\3\27\3\30\3\30\3\30\3\30\3\30\3\30\3\30\3\30\7\30"+
		"\u0148\n\30\f\30\16\30\u014b\13\30\3\31\3\31\3\31\3\31\7\31\u0151\n\31"+
		"\f\31\16\31\u0154\13\31\3\31\3\31\3\32\3\32\3\32\3\32\7\32\u015c\n\32"+
		"\f\32\16\32\u015f\13\32\3\32\3\32\3\33\3\33\3\33\3\33\3\33\3\34\3\34\5"+
		"\34\u016a\n\34\3\35\3\35\3\35\3\35\3\35\3\35\5\35\u0172\n\35\3\35\3\35"+
		"\3\35\3\35\3\35\5\35\u0179\n\35\3\35\3\35\5\35\u017d\n\35\3\36\3\36\3"+
		"\36\3\36\3\36\3\36\5\36\u0185\n\36\3\36\3\36\5\36\u0189\n\36\3\36\3\36"+
		"\3\36\3\36\5\36\u018f\n\36\3\36\3\36\5\36\u0193\n\36\3\36\3\36\5\36\u0197"+
		"\n\36\3\37\3\37\3\37\7\37\u019c\n\37\f\37\16\37\u019f\13\37\3\37\3\37"+
		"\3\37\7\37\u01a4\n\37\f\37\16\37\u01a7\13\37\3\37\3\37\3\37\5\37\u01ac"+
		"\n\37\3 \3 \3 \5 \u01b1\n \3 \3 \3!\3!\3!\3!\3!\3!\3!\3!\3!\5!\u01be\n"+
		"!\3!\3!\3!\7!\u01c3\n!\f!\16!\u01c6\13!\3\"\3\"\3\"\5\"\u01cb\n\"\3#\3"+
		"#\5#\u01cf\n#\3#\3#\3#\5#\u01d4\n#\7#\u01d6\n#\f#\16#\u01d9\13#\3$\3$"+
		"\3$\5$\u01de\n$\3%\3%\3%\5%\u01e3\n%\3&\3&\3&\3&\3\'\3\'\3\'\3\'\3(\3"+
		"(\3(\3(\7(\u01f1\n(\f(\16(\u01f4\13(\3(\3(\3)\3)\3)\3)\7)\u01fc\n)\f)"+
		"\16)\u01ff\13)\3)\3)\3*\3*\5*\u0205\n*\3+\3+\3+\5+\u020a\n+\3,\3,\3,\3"+
		",\3,\3-\3-\3-\3-\3-\3.\3.\3.\3.\3.\3/\3/\3/\3/\3/\3/\3/\7/\u0222\n/\f"+
		"/\16/\u0225\13/\3\60\3\60\3\60\3\60\3\60\3\60\5\60\u022d\n\60\3\60\3\60"+
		"\3\60\3\60\7\60\u0233\n\60\f\60\16\60\u0236\13\60\3\61\3\61\3\61\3\61"+
		"\3\61\3\61\3\61\7\61\u023f\n\61\f\61\16\61\u0242\13\61\3\62\3\62\3\62"+
		"\3\62\3\62\3\62\5\62\u024a\n\62\3\62\3\62\3\62\3\62\7\62\u0250\n\62\f"+
		"\62\16\62\u0253\13\62\3\63\3\63\3\63\3\63\3\63\3\63\3\63\7\63\u025c\n"+
		"\63\f\63\16\63\u025f\13\63\3\64\3\64\3\64\3\64\3\64\3\64\5\64\u0267\n"+
		"\64\3\64\3\64\3\64\3\64\7\64\u026d\n\64\f\64\16\64\u0270\13\64\3\65\3"+
		"\65\3\65\3\65\3\65\3\65\3\65\7\65\u0279\n\65\f\65\16\65\u027c\13\65\3"+
		"\66\3\66\3\66\3\66\3\66\3\66\3\66\5\66\u0285\n\66\3\66\3\66\3\66\3\66"+
		"\7\66\u028b\n\66\f\66\16\66\u028e\13\66\3\67\3\67\3\67\3\67\3\67\3\67"+
		"\3\67\7\67\u0297\n\67\f\67\16\67\u029a\13\67\38\38\38\38\38\38\58\u02a2"+
		"\n8\38\38\38\38\78\u02a8\n8\f8\168\u02ab\138\39\39\39\39\39\39\3:\3:\3"+
		";\3;\3<\3<\3=\3=\3=\3=\3>\3>\3>\3>\3?\3?\3?\7?\u02c4\n?\f?\16?\u02c7\13"+
		"?\3@\3@\3@\3@\3A\3A\3A\3A\3B\3B\3C\3C\3D\3D\3E\3E\3F\3F\3G\3G\3H\3H\3"+
		"I\3I\3J\3J\3K\3K\3L\3L\5L\u02e7\nL\3M\3M\3N\3N\3O\3O\3O\7O\u02f0\nO\f"+
		"O\16O\u02f3\13O\3O\2P\2\4\6\b\n\f\16\20\22\24\26\30\32\34\36 \"$&(*,."+
		"\60\62\64\668:<>@BDFHJLNPRTVXZ\\^`bdfhjlnprtvxz|~\u0080\u0082\u0084\u0086"+
		"\u0088\u008a\u008c\u008e\u0090\u0092\u0094\u0096\u0098\u009a\u009c\2\25"+
		"\3\2%&\4\2\36\36 #\3\2\16\17\4\2\63\63KK\3\2?@\4\2\3\3^_\4\2\5\5ZZ\4\2"+
		"\5\5[[\4\2TTVV\4\2UUWW\3\2\4\5\4\2KR^^\4\2IR^^\3\2EF\3\2GH\4\2\6\6\b\b"+
		"\4\2\7\7\t\t\3\2CD\3\2\65>\u02f4\2\u00ac\3\2\2\2\4\u00b0\3\2\2\2\6\u00c7"+
		"\3\2\2\2\b\u00cb\3\2\2\2\n\u00cf\3\2\2\2\f\u00d3\3\2\2\2\16\u00da\3\2"+
		"\2\2\20\u00df\3\2\2\2\22\u00e4\3\2\2\2\24\u00eb\3\2\2\2\26\u00f2\3\2\2"+
		"\2\30\u00fb\3\2\2\2\32\u00fe\3\2\2\2\34\u0101\3\2\2\2\36\u010a\3\2\2\2"+
		" \u010d\3\2\2\2\"\u0110\3\2\2\2$\u0113\3\2\2\2&\u0115\3\2\2\2(\u012c\3"+
		"\2\2\2*\u012e\3\2\2\2,\u013b\3\2\2\2.\u013f\3\2\2\2\60\u014c\3\2\2\2\62"+
		"\u0157\3\2\2\2\64\u0162\3\2\2\2\66\u0169\3\2\2\28\u016b\3\2\2\2:\u017e"+
		"\3\2\2\2<\u01ab\3\2\2\2>\u01ad\3\2\2\2@\u01bd\3\2\2\2B\u01c7\3\2\2\2D"+
		"\u01ce\3\2\2\2F\u01da\3\2\2\2H\u01df\3\2\2\2J\u01e4\3\2\2\2L\u01e8\3\2"+
		"\2\2N\u01ec\3\2\2\2P\u01f7\3\2\2\2R\u0204\3\2\2\2T\u0209\3\2\2\2V\u020b"+
		"\3\2\2\2X\u0210\3\2\2\2Z\u0215\3\2\2\2\\\u021a\3\2\2\2^\u022c\3\2\2\2"+
		"`\u0237\3\2\2\2b\u0249\3\2\2\2d\u0254\3\2\2\2f\u0266\3\2\2\2h\u0271\3"+
		"\2\2\2j\u0284\3\2\2\2l\u028f\3\2\2\2n\u02a1\3\2\2\2p\u02ac\3\2\2\2r\u02b2"+
		"\3\2\2\2t\u02b4\3\2\2\2v\u02b6\3\2\2\2x\u02b8\3\2\2\2z\u02bc\3\2\2\2|"+
		"\u02c0\3\2\2\2~\u02c8\3\2\2\2\u0080\u02cc\3\2\2\2\u0082\u02d0\3\2\2\2"+
		"\u0084\u02d2\3\2\2\2\u0086\u02d4\3\2\2\2\u0088\u02d6\3\2\2\2\u008a\u02d8"+
		"\3\2\2\2\u008c\u02da\3\2\2\2\u008e\u02dc\3\2\2\2\u0090\u02de\3\2\2\2\u0092"+
		"\u02e0\3\2\2\2\u0094\u02e2\3\2\2\2\u0096\u02e6\3\2\2\2\u0098\u02e8\3\2"+
		"\2\2\u009a\u02ea\3\2\2\2\u009c\u02ec\3\2\2\2\u009e\u00ad\5.\30\2\u009f"+
		"\u00ad\5\64\33\2\u00a0\u00ad\5\66\34\2\u00a1\u00ad\5&\24\2\u00a2\u00ad"+
		"\5*\26\2\u00a3\u00ad\5$\23\2\u00a4\u00ad\5\"\22\2\u00a5\u00ad\5\4\3\2"+
		"\u00a6\u00ad\5\f\7\2\u00a7\u00ad\5\16\b\2\u00a8\u00ad\5\22\n\2\u00a9\u00ad"+
		"\5\24\13\2\u00aa\u00ad\5\26\f\2\u00ab\u00ad\5\34\17\2\u00ac\u009e\3\2"+
		"\2\2\u00ac\u009f\3\2\2\2\u00ac\u00a0\3\2\2\2\u00ac\u00a1\3\2\2\2\u00ac"+
		"\u00a2\3\2\2\2\u00ac\u00a3\3\2\2\2\u00ac\u00a4\3\2\2\2\u00ac\u00a5\3\2"+
		"\2\2\u00ac\u00a6\3\2\2\2\u00ac\u00a7\3\2\2\2\u00ac\u00a8\3\2\2\2\u00ac"+
		"\u00a9\3\2\2\2\u00ac\u00aa\3\2\2\2\u00ac\u00ab\3\2\2\2\u00ad\u00ae\3\2"+
		"\2\2\u00ae\u00af\7\2\2\3\u00af\3\3\2\2\2\u00b0\u00c5\7\22\2\2\u00b1\u00b2"+
		"\7&\2\2\u00b2\u00b3\7\4\2\2\u00b3\u00be\5\6\4\2\u00b4\u00b6\5\n\6\2\u00b5"+
		"\u00b7\5\b\5\2\u00b6\u00b5\3\2\2\2\u00b6\u00b7\3\2\2\2\u00b7\u00bd\3\2"+
		"\2\2\u00b8\u00ba\5\b\5\2\u00b9\u00bb\5\n\6\2\u00ba\u00b9\3\2\2\2\u00ba"+
		"\u00bb\3\2\2\2\u00bb\u00bd\3\2\2\2\u00bc\u00b4\3\2\2\2\u00bc\u00b8\3\2"+
		"\2\2\u00bd\u00bf\3\2\2\2\u00be\u00bc\3\2\2\2\u00be\u00bf\3\2\2\2\u00bf"+
		"\u00c6\3\2\2\2\u00c0\u00c1\7%\2\2\u00c1\u00c3\7\4\2\2\u00c2\u00c4\5\b"+
		"\5\2\u00c3\u00c2\3\2\2\2\u00c3\u00c4\3\2\2\2\u00c4\u00c6\3\2\2\2\u00c5"+
		"\u00b1\3\2\2\2\u00c5\u00c0\3\2\2\2\u00c6\5\3\2\2\2\u00c7\u00c8\7\64\2"+
		"\2\u00c8\u00c9\7\33\2\2\u00c9\u00ca\7\4\2\2\u00ca\7\3\2\2\2\u00cb\u00cc"+
		"\7\64\2\2\u00cc\u00cd\7!\2\2\u00cd\u00ce\5\u009cO\2\u00ce\t\3\2\2\2\u00cf"+
		"\u00d0\7\64\2\2\u00d0\u00d1\7\"\2\2\u00d1\u00d2\5\u009cO\2\u00d2\13\3"+
		"\2\2\2\u00d3\u00d8\7\23\2\2\u00d4\u00d5\t\2\2\2\u00d5\u00d9\7\4\2\2\u00d6"+
		"\u00d7\7\37\2\2\u00d7\u00d9\5r:\2\u00d8\u00d4\3\2\2\2\u00d8\u00d6\3\2"+
		"\2\2\u00d9\r\3\2\2\2\u00da\u00db\7\24\2\2\u00db\u00dc\7&\2\2\u00dc\u00dd"+
		"\7\4\2\2\u00dd\u00de\5\20\t\2\u00de\17\3\2\2\2\u00df\u00e0\7-\2\2\u00e0"+
		"\u00e1\7\33\2\2\u00e1\u00e2\7*\2\2\u00e2\u00e3\7\4\2\2\u00e3\21\3\2\2"+
		"\2\u00e4\u00e5\7\25\2\2\u00e5\u00e6\7!\2\2\u00e6\u00e7\5\u009cO\2\u00e7"+
		"\u00e8\7*\2\2\u00e8\u00e9\t\2\2\2\u00e9\u00ea\7\4\2\2\u00ea\23\3\2\2\2"+
		"\u00eb\u00ec\7\26\2\2\u00ec\u00ed\7!\2\2\u00ed\u00ee\5\u009cO\2\u00ee"+
		"\u00ef\7\'\2\2\u00ef\u00f0\t\2\2\2\u00f0\u00f1\7\4\2\2\u00f1\25\3\2\2"+
		"\2\u00f2\u00f5\7\27\2\2\u00f3\u00f6\5\30\r\2\u00f4\u00f6\5\32\16\2\u00f5"+
		"\u00f3\3\2\2\2\u00f5\u00f4\3\2\2\2\u00f6\u00f7\3\2\2\2\u00f7\u00f8\7*"+
		"\2\2\u00f8\u00f9\7&\2\2\u00f9\u00fa\7\4\2\2\u00fa\27\3\2\2\2\u00fb\u00fc"+
		"\7%\2\2\u00fc\u00fd\7\4\2\2\u00fd\31\3\2\2\2\u00fe\u00ff\7\"\2\2\u00ff"+
		"\u0100\5\u009cO\2\u0100\33\3\2\2\2\u0101\u0104\7\30\2\2\u0102\u0105\5"+
		"\36\20\2\u0103\u0105\5 \21\2\u0104\u0102\3\2\2\2\u0104\u0103\3\2\2\2\u0105"+
		"\u0106\3\2\2\2\u0106\u0107\7\'\2\2\u0107\u0108\7&\2\2\u0108\u0109\7\4"+
		"\2\2\u0109\35\3\2\2\2\u010a\u010b\7%\2\2\u010b\u010c\7\4\2\2\u010c\37"+
		"\3\2\2\2\u010d\u010e\7\"\2\2\u010e\u010f\5\u009cO\2\u010f!\3\2\2\2\u0110"+
		"\u0111\7\n\2\2\u0111\u0112\t\3\2\2\u0112#\3\2\2\2\u0113\u0114\7\21\2\2"+
		"\u0114%\3\2\2\2\u0115\u0119\t\4\2\2\u0116\u011a\5r:\2\u0117\u0118\7\'"+
		"\2\2\u0118\u011a\7\4\2\2\u0119\u0116\3\2\2\2\u0119\u0117\3\2\2\2\u011a"+
		"\u0124\3\2\2\2\u011b\u011c\7-\2\2\u011c\u0121\5(\25\2\u011d\u011e\7\\"+
		"\2\2\u011e\u0120\5(\25\2\u011f\u011d\3\2\2\2\u0120\u0123\3\2\2\2\u0121"+
		"\u011f\3\2\2\2\u0121\u0122\3\2\2\2\u0122\u0125\3\2\2\2\u0123\u0121\3\2"+
		"\2\2\u0124\u011b\3\2\2\2\u0124\u0125\3\2\2\2\u0125\'\3\2\2\2\u0126\u0127"+
		"\7\31\2\2\u0127\u0128\7S\2\2\u0128\u012d\5\u0098M\2\u0129\u012a\7\32\2"+
		"\2\u012a\u012b\7S\2\2\u012b\u012d\5\u0098M\2\u012c\u0126\3\2\2\2\u012c"+
		"\u0129\3\2\2\2\u012d)\3\2\2\2\u012e\u012f\7\20\2\2\u012f\u0139\5r:\2\u0130"+
		"\u0131\7-\2\2\u0131\u0136\5,\27\2\u0132\u0133\7\\\2\2\u0133\u0135\5,\27"+
		"\2\u0134\u0132\3\2\2\2\u0135\u0138\3\2\2\2\u0136\u0134\3\2\2\2\u0136\u0137"+
		"\3\2\2\2\u0137\u013a\3\2\2\2\u0138\u0136\3\2\2\2\u0139\u0130\3\2\2\2\u0139"+
		"\u013a\3\2\2\2\u013a+\3\2\2\2\u013b\u013c\7\31\2\2\u013c\u013d\7S\2\2"+
		"\u013d\u013e\5\u0098M\2\u013e-\3\2\2\2\u013f\u0140\7\f\2\2\u0140\u0141"+
		"\7,\2\2\u0141\u0142\5r:\2\u0142\u0143\5\60\31\2\u0143\u0144\7.\2\2\u0144"+
		"\u0149\5\62\32\2\u0145\u0146\7\\\2\2\u0146\u0148\5\62\32\2\u0147\u0145"+
		"\3\2\2\2\u0148\u014b\3\2\2\2\u0149\u0147\3\2\2\2\u0149\u014a\3\2\2\2\u014a"+
		"/\3\2\2\2\u014b\u0149\3\2\2\2\u014c\u014d\7T\2\2\u014d\u0152\5R*\2\u014e"+
		"\u014f\7\\\2\2\u014f\u0151\5R*\2\u0150\u014e\3\2\2\2\u0151\u0154\3\2\2"+
		"\2\u0152\u0150\3\2\2\2\u0152\u0153\3\2\2\2\u0153\u0155\3\2\2\2\u0154\u0152"+
		"\3\2\2\2\u0155\u0156\7U\2\2\u0156\61\3\2\2\2\u0157\u0158\7T\2\2\u0158"+
		"\u015d\5T+\2\u0159\u015a\7\\\2\2\u015a\u015c\5T+\2\u015b\u0159\3\2\2\2"+
		"\u015c\u015f\3\2\2\2\u015d\u015b\3\2\2\2\u015d\u015e\3\2\2\2\u015e\u0160"+
		"\3\2\2\2\u015f\u015d\3\2\2\2\u0160\u0161\7U\2\2\u0161\63\3\2\2\2\u0162"+
		"\u0163\7\r\2\2\u0163\u0164\5|?\2\u0164\u0165\7\'\2\2\u0165\u0166\5r:\2"+
		"\u0166\65\3\2\2\2\u0167\u016a\58\35\2\u0168\u016a\5:\36\2\u0169\u0167"+
		"\3\2\2\2\u0169\u0168\3\2\2\2\u016a\67\3\2\2\2\u016b\u0171\7\13\2\2\u016c"+
		"\u0172\7\35\2\2\u016d\u016e\t\5\2\2\u016e\u016f\7T\2\2\u016f\u0170\7\35"+
		"\2\2\u0170\u0172\7U\2\2\u0171\u016c\3\2\2\2\u0171\u016d\3\2\2\2\u0172"+
		"\u0173\3\2\2\2\u0173\u0174\7\'\2\2\u0174\u0178\5r:\2\u0175\u0176\5\u009a"+
		"N\2\u0176\u0177\5> \2\u0177\u0179\3\2\2\2\u0178\u0175\3\2\2\2\u0178\u0179"+
		"\3\2\2\2\u0179\u017c\3\2\2\2\u017a\u017b\7\61\2\2\u017b\u017d\5@!\2\u017c"+
		"\u017a\3\2\2\2\u017c\u017d\3\2\2\2\u017d9\3\2\2\2\u017e\u0184\7\13\2\2"+
		"\u017f\u0185\7\34\2\2\u0180\u0181\7\62\2\2\u0181\u0182\7T\2\2\u0182\u0183"+
		"\7\34\2\2\u0183\u0185\7U\2\2\u0184\u017f\3\2\2\2\u0184\u0180\3\2\2\2\u0185"+
		"\u0188\3\2\2\2\u0186\u0187\7(\2\2\u0187\u0189\5<\37\2\u0188\u0186\3\2"+
		"\2\2\u0188\u0189\3\2\2\2\u0189\u018a\3\2\2\2\u018a\u018b\7\'\2\2\u018b"+
		"\u018e\5r:\2\u018c\u018d\7+\2\2\u018d\u018f\5> \2\u018e\u018c\3\2\2\2"+
		"\u018e\u018f\3\2\2\2\u018f\u0192\3\2\2\2\u0190\u0191\7\61\2\2\u0191\u0193"+
		"\5@!\2\u0192\u0190\3\2\2\2\u0192\u0193\3\2\2\2\u0193\u0196\3\2\2\2\u0194"+
		"\u0195\7\60\2\2\u0195\u0197\5B\"\2\u0196\u0194\3\2\2\2\u0196\u0197\3\2"+
		"\2\2\u0197;\3\2\2\2\u0198\u019d\5F$\2\u0199\u019a\7\\\2\2\u019a\u019c"+
		"\5F$\2\u019b\u0199\3\2\2\2\u019c\u019f\3\2\2\2\u019d\u019b\3\2\2\2\u019d"+
		"\u019e\3\2\2\2\u019e\u01ac\3\2\2\2\u019f\u019d\3\2\2\2\u01a0\u01a5\5H"+
		"%\2\u01a1\u01a2\7\\\2\2\u01a2\u01a4\5H%\2\u01a3\u01a1\3\2\2\2\u01a4\u01a7"+
		"\3\2\2\2\u01a5\u01a3\3\2\2\2\u01a5\u01a6\3\2\2\2\u01a6\u01a8\3\2\2\2\u01a7"+
		"\u01a5\3\2\2\2\u01a8\u01a9\7)\2\2\u01a9\u01aa\5p9\2\u01aa\u01ac\3\2\2"+
		"\2\u01ab\u0198\3\2\2\2\u01ab\u01a0\3\2\2\2\u01ac=\3\2\2\2\u01ad\u01b0"+
		"\5\u0086D\2\u01ae\u01b1\5x=\2\u01af\u01b1\5z>\2\u01b0\u01ae\3\2\2\2\u01b0"+
		"\u01af\3\2\2\2\u01b1\u01b2\3\2\2\2\u01b2\u01b3\5\u0088E\2\u01b3?\3\2\2"+
		"\2\u01b4\u01b5\b!\1\2\u01b5\u01b6\7A\2\2\u01b6\u01be\5@!\2\u01b7\u01be"+
		"\5J&\2\u01b8\u01be\5L\'\2\u01b9\u01ba\7T\2\2\u01ba\u01bb\5@!\2\u01bb\u01bc"+
		"\7U\2\2\u01bc\u01be\3\2\2\2\u01bd\u01b4\3\2\2\2\u01bd\u01b7\3\2\2\2\u01bd"+
		"\u01b8\3\2\2\2\u01bd\u01b9\3\2\2\2\u01be\u01c4\3\2\2\2\u01bf\u01c0\6!"+
		"\2\3\u01c0\u01c1\t\6\2\2\u01c1\u01c3\5@!\2\u01c2\u01bf\3\2\2\2\u01c3\u01c6"+
		"\3\2\2\2\u01c4\u01c2\3\2\2\2\u01c4\u01c5\3\2\2\2\u01c5A\3\2\2\2\u01c6"+
		"\u01c4\3\2\2\2\u01c7\u01ca\5D#\2\u01c8\u01c9\7B\2\2\u01c9\u01cb\5P)\2"+
		"\u01ca\u01c8\3\2\2\2\u01ca\u01cb\3\2\2\2\u01cbC\3\2\2\2\u01cc\u01cf\5"+
		"p9\2\u01cd\u01cf\5t;\2\u01ce\u01cc\3\2\2\2\u01ce\u01cd\3\2\2\2\u01cf\u01d7"+
		"\3\2\2\2\u01d0\u01d3\7\\\2\2\u01d1\u01d4\5p9\2\u01d2\u01d4\5t;\2\u01d3"+
		"\u01d1\3\2\2\2\u01d3\u01d2\3\2\2\2\u01d4\u01d6\3\2\2\2\u01d5\u01d0\3\2"+
		"\2\2\u01d6\u01d9\3\2\2\2\u01d7\u01d5\3\2\2\2\u01d7\u01d8\3\2\2\2\u01d8"+
		"E\3\2\2\2\u01d9\u01d7\3\2\2\2\u01da\u01dd\5\\/\2\u01db\u01dc\7/\2\2\u01dc"+
		"\u01de\5v<\2\u01dd\u01db\3\2\2\2\u01dd\u01de\3\2\2\2\u01deG\3\2\2\2\u01df"+
		"\u01e2\5h\65\2\u01e0\u01e1\7/\2\2\u01e1\u01e3\5v<\2\u01e2\u01e0\3\2\2"+
		"\2\u01e2\u01e3\3\2\2\2\u01e3I\3\2\2\2\u01e4\u01e5\5p9\2\u01e5\u01e6\7"+
		"S\2\2\u01e6\u01e7\5\u008aF\2\u01e7K\3\2\2\2\u01e8\u01e9\5t;\2\u01e9\u01ea"+
		"\7S\2\2\u01ea\u01eb\5\u008aF\2\u01ebM\3\2\2\2\u01ec\u01ed\7T\2\2\u01ed"+
		"\u01f2\5\u008aF\2\u01ee\u01ef\7\\\2\2\u01ef\u01f1\5\u008aF\2\u01f0\u01ee"+
		"\3\2\2\2\u01f1\u01f4\3\2\2\2\u01f2\u01f0\3\2\2\2\u01f2\u01f3\3\2\2\2\u01f3"+
		"\u01f5\3\2\2\2\u01f4\u01f2\3\2\2\2\u01f5\u01f6\7U\2\2\u01f6O\3\2\2\2\u01f7"+
		"\u01f8\7X\2\2\u01f8\u01fd\5N(\2\u01f9\u01fa\7\\\2\2\u01fa\u01fc\5N(\2"+
		"\u01fb\u01f9\3\2\2\2\u01fc\u01ff\3\2\2\2\u01fd\u01fb\3\2\2\2\u01fd\u01fe"+
		"\3\2\2\2\u01fe\u0200\3\2\2\2\u01ff\u01fd\3\2\2\2\u0200\u0201\7Y\2\2\u0201"+
		"Q\3\2\2\2\u0202\u0205\5\u0096L\2\u0203\u0205\5t;\2\u0204\u0202\3\2\2\2"+
		"\u0204\u0203\3\2\2\2\u0205S\3\2\2\2\u0206\u020a\5\u0082B\2\u0207\u020a"+
		"\5\u0084C\2\u0208\u020a\5\u008aF\2\u0209\u0206\3\2\2\2\u0209\u0207\3\2"+
		"\2\2\u0209\u0208\3\2\2\2\u020aU\3\2\2\2\u020b\u020c\5\u008eH\2\u020c\u020d"+
		"\7T\2\2\u020d\u020e\5l\67\2\u020e\u020f\7U\2\2\u020fW\3\2\2\2\u0210\u0211"+
		"\5\u0090I\2\u0211\u0212\7T\2\2\u0212\u0213\5l\67\2\u0213\u0214\7U\2\2"+
		"\u0214Y\3\2\2\2\u0215\u0216\5\u008cG\2\u0216\u0217\7T\2\2\u0217\u0218"+
		"\5\\/\2\u0218\u0219\7U\2\2\u0219[\3\2\2\2\u021a\u021b\b/\1\2\u021b\u021c"+
		"\5^\60\2\u021c\u0223\3\2\2\2\u021d\u021e\6/\3\3\u021e\u021f\5\u0094K\2"+
		"\u021f\u0220\5^\60\2\u0220\u0222\3\2\2\2\u0221\u021d\3\2\2\2\u0222\u0225"+
		"\3\2\2\2\u0223\u0221\3\2\2\2\u0223\u0224\3\2\2\2\u0224]\3\2\2\2\u0225"+
		"\u0223\3\2\2\2\u0226\u0227\b\60\1\2\u0227\u022d\5X-\2\u0228\u0229\7T\2"+
		"\2\u0229\u022a\5\\/\2\u022a\u022b\7U\2\2\u022b\u022d\3\2\2\2\u022c\u0226"+
		"\3\2\2\2\u022c\u0228\3\2\2\2\u022d\u0234\3\2\2\2\u022e\u022f\6\60\4\3"+
		"\u022f\u0230\5\u0092J\2\u0230\u0231\5^\60\2\u0231\u0233\3\2\2\2\u0232"+
		"\u022e\3\2\2\2\u0233\u0236\3\2\2\2\u0234\u0232\3\2\2\2\u0234\u0235\3\2"+
		"\2\2\u0235_\3\2\2\2\u0236\u0234\3\2\2\2\u0237\u0238\b\61\1\2\u0238\u0239"+
		"\5b\62\2\u0239\u0240\3\2\2\2\u023a\u023b\6\61\5\3\u023b\u023c\5\u0094"+
		"K\2\u023c\u023d\5b\62\2\u023d\u023f\3\2\2\2\u023e\u023a\3\2\2\2\u023f"+
		"\u0242\3\2\2\2\u0240\u023e\3\2\2\2\u0240\u0241\3\2\2\2\u0241a\3\2\2\2"+
		"\u0242\u0240\3\2\2\2\u0243\u0244\b\62\1\2\u0244\u024a\5Z.\2\u0245\u0246"+
		"\7T\2\2\u0246\u0247\5`\61\2\u0247\u0248\7U\2\2\u0248\u024a\3\2\2\2\u0249"+
		"\u0243\3\2\2\2\u0249\u0245\3\2\2\2\u024a\u0251\3\2\2\2\u024b\u024c\6\62"+
		"\6\3\u024c\u024d\5\u0092J\2\u024d\u024e\5b\62\2\u024e\u0250\3\2\2\2\u024f"+
		"\u024b\3\2\2\2\u0250\u0253\3\2\2\2\u0251\u024f\3\2\2\2\u0251\u0252\3\2"+
		"\2\2\u0252c\3\2\2\2\u0253\u0251\3\2\2\2\u0254\u0255\b\63\1\2\u0255\u0256"+
		"\5f\64\2\u0256\u025d\3\2\2\2\u0257\u0258\6\63\7\3\u0258\u0259\5\u0094"+
		"K\2\u0259\u025a\5f\64\2\u025a\u025c\3\2\2\2\u025b\u0257\3\2\2\2\u025c"+
		"\u025f\3\2\2\2\u025d\u025b\3\2\2\2\u025d\u025e\3\2\2\2\u025ee\3\2\2\2"+
		"\u025f\u025d\3\2\2\2\u0260\u0261\b\64\1\2\u0261\u0267\5V,\2\u0262\u0263"+
		"\7T\2\2\u0263\u0264\5d\63\2\u0264\u0265\7U\2\2\u0265\u0267\3\2\2\2\u0266"+
		"\u0260\3\2\2\2\u0266\u0262\3\2\2\2\u0267\u026e\3\2\2\2\u0268\u0269\6\64"+
		"\b\3\u0269\u026a\5\u0092J\2\u026a\u026b\5f\64\2\u026b\u026d\3\2\2\2\u026c"+
		"\u0268\3\2\2\2\u026d\u0270\3\2\2\2\u026e\u026c\3\2\2\2\u026e\u026f\3\2"+
		"\2\2\u026fg\3\2\2\2\u0270\u026e\3\2\2\2\u0271\u0272\b\65\1\2\u0272\u0273"+
		"\5j\66\2\u0273\u027a\3\2\2\2\u0274\u0275\6\65\t\3\u0275\u0276\5\u0094"+
		"K\2\u0276\u0277\5j\66\2\u0277\u0279\3\2\2\2\u0278\u0274\3\2\2\2\u0279"+
		"\u027c\3\2\2\2\u027a\u0278\3\2\2\2\u027a\u027b\3\2\2\2\u027bi\3\2\2\2"+
		"\u027c\u027a\3\2\2\2\u027d\u027e\b\66\1\2\u027e\u0285\5`\61\2\u027f\u0285"+
		"\5d\63\2\u0280\u0281\7T\2\2\u0281\u0282\5j\66\2\u0282\u0283\7U\2\2\u0283"+
		"\u0285\3\2\2\2\u0284\u027d\3\2\2\2\u0284\u027f\3\2\2\2\u0284\u0280\3\2"+
		"\2\2\u0285\u028c\3\2\2\2\u0286\u0287\6\66\n\3\u0287\u0288\5\u0092J\2\u0288"+
		"\u0289\5j\66\2\u0289\u028b\3\2\2\2\u028a\u0286\3\2\2\2\u028b\u028e\3\2"+
		"\2\2\u028c\u028a\3\2\2\2\u028c\u028d\3\2\2\2\u028dk\3\2\2\2\u028e\u028c"+
		"\3\2\2\2\u028f\u0290\b\67\1\2\u0290\u0291\5n8\2\u0291\u0298\3\2\2\2\u0292"+
		"\u0293\6\67\13\3\u0293\u0294\5\u0094K\2\u0294\u0295\5n8\2\u0295\u0297"+
		"\3\2\2\2\u0296\u0292\3\2\2\2\u0297\u029a\3\2\2\2\u0298\u0296\3\2\2\2\u0298"+
		"\u0299\3\2\2\2\u0299m\3\2\2\2\u029a\u0298\3\2\2\2\u029b\u029c\b8\1\2\u029c"+
		"\u02a2\5t;\2\u029d\u029e\7T\2\2\u029e\u029f\5l\67\2\u029f\u02a0\7U\2\2"+
		"\u02a0\u02a2\3\2\2\2\u02a1\u029b\3\2\2\2\u02a1\u029d\3\2\2\2\u02a2\u02a9"+
		"\3\2\2\2\u02a3\u02a4\68\f\3\u02a4\u02a5\5\u0092J\2\u02a5\u02a6\5n8\2\u02a6"+
		"\u02a8\3\2\2\2\u02a7\u02a3\3\2\2\2\u02a8\u02ab\3\2\2\2\u02a9\u02a7\3\2"+
		"\2\2\u02a9\u02aa\3\2\2\2\u02aao\3\2\2\2\u02ab\u02a9\3\2\2\2\u02ac\u02ad"+
		"\t\7\2\2\u02ad\u02ae\7]\2\2\u02ae\u02af\t\7\2\2\u02af\u02b0\7]\2\2\u02b0"+
		"\u02b1\t\7\2\2\u02b1q\3\2\2\2\u02b2\u02b3\t\7\2\2\u02b3s\3\2\2\2\u02b4"+
		"\u02b5\t\7\2\2\u02b5u\3\2\2\2\u02b6\u02b7\t\7\2\2\u02b7w\3\2\2\2\u02b8"+
		"\u02b9\7Z\2\2\u02b9\u02ba\7\\\2\2\u02ba\u02bb\7Z\2\2\u02bby\3\2\2\2\u02bc"+
		"\u02bd\7[\2\2\u02bd\u02be\7\\\2\2\u02be\u02bf\7[\2\2\u02bf{\3\2\2\2\u02c0"+
		"\u02c5\7[\2\2\u02c1\u02c2\7\\\2\2\u02c2\u02c4\7[\2\2\u02c3\u02c1\3\2\2"+
		"\2\u02c4\u02c7\3\2\2\2\u02c5\u02c3\3\2\2\2\u02c5\u02c6\3\2\2\2\u02c6}"+
		"\3\2\2\2\u02c7\u02c5\3\2\2\2\u02c8\u02c9\t\b\2\2\u02c9\u02ca\7\\\2\2\u02ca"+
		"\u02cb\t\b\2\2\u02cb\177\3\2\2\2\u02cc\u02cd\t\t\2\2\u02cd\u02ce\7\\\2"+
		"\2\u02ce\u02cf\t\t\2\2\u02cf\u0081\3\2\2\2\u02d0\u02d1\t\b\2\2\u02d1\u0083"+
		"\3\2\2\2\u02d2\u02d3\t\t\2\2\u02d3\u0085\3\2\2\2\u02d4\u02d5\t\n\2\2\u02d5"+
		"\u0087\3\2\2\2\u02d6\u02d7\t\13\2\2\u02d7\u0089\3\2\2\2\u02d8\u02d9\t"+
		"\f\2\2\u02d9\u008b\3\2\2\2\u02da\u02db\t\r\2\2\u02db\u008d\3\2\2\2\u02dc"+
		"\u02dd\t\r\2\2\u02dd\u008f\3\2\2\2\u02de\u02df\t\16\2\2\u02df\u0091\3"+
		"\2\2\2\u02e0\u02e1\t\17\2\2\u02e1\u0093\3\2\2\2\u02e2\u02e3\t\20\2\2\u02e3"+
		"\u0095\3\2\2\2\u02e4\u02e7\t\21\2\2\u02e5\u02e7\t\22\2\2\u02e6\u02e4\3"+
		"\2\2\2\u02e6\u02e5\3\2\2\2\u02e7\u0097\3\2\2\2\u02e8\u02e9\t\23\2\2\u02e9"+
		"\u0099\3\2\2\2\u02ea\u02eb\t\24\2\2\u02eb\u009b\3\2\2\2\u02ec\u02f1\7"+
		"\4\2\2\u02ed\u02ee\7\\\2\2\u02ee\u02f0\7\4\2\2\u02ef\u02ed\3\2\2\2\u02f0"+
		"\u02f3\3\2\2\2\u02f1\u02ef\3\2\2\2\u02f1\u02f2\3\2\2\2\u02f2\u009d\3\2"+
		"\2\2\u02f3\u02f1\3\2\2\2@\u00ac\u00b6\u00ba\u00bc\u00be\u00c3\u00c5\u00d8"+
		"\u00f5\u0104\u0119\u0121\u0124\u012c\u0136\u0139\u0149\u0152\u015d\u0169"+
		"\u0171\u0178\u017c\u0184\u0188\u018e\u0192\u0196\u019d\u01a5\u01ab\u01b0"+
		"\u01bd\u01c4\u01ca\u01ce\u01d3\u01d7\u01dd\u01e2\u01f2\u01fd\u0204\u0209"+
		"\u0223\u022c\u0234\u0240\u0249\u0251\u025d\u0266\u026e\u027a\u0284\u028c"+
		"\u0298\u02a1\u02a9\u02c5\u02e6\u02f1";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}