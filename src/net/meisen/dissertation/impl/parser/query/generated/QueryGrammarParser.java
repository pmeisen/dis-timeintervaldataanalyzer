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
		STMT_OPEN=11, STMT_LOAD=12, STMT_UNLOAD=13, STMT_ALIVE=14, STMT_ADD=15, 
		STMT_DROP=16, STMT_MODIFY=17, STMT_GRANT=18, STMT_REVOKE=19, STMT_ASSIGN=20, 
		STMT_REMOVE=21, PROP_AUTOLOAD=22, PROP_FORCE=23, PROP_PASSWORD=24, TYPE_TIMESERIES=25, 
		TYPE_RECORDS=26, TYPE_MODELS=27, TYPE_VERSION=28, TYPE_PERMISSIONS=29, 
		TYPE_ROLES=30, TYPE_USERS=31, TYPE_PERMISSION=32, TYPE_ROLE=33, TYPE_USER=34, 
		OP_FROM=35, OP_OF=36, OP_TO=37, OP_IN=38, OP_INTO=39, OP_SET=40, OP_VALUES=41, 
		OP_ALIAS=42, OP_GROUPBY=43, OP_FILTERBY=44, OP_TRANSPOSE=45, OP_IDONLY=46, 
		OP_WITH=47, IR_EQUALTO=48, IR_BEFORE=49, IR_AFTER=50, IR_MEETING=51, IR_OVERLAPPING=52, 
		IR_DURING=53, IR_WITHIN=54, IR_CONTAINING=55, IR_STARTINGWITH=56, IR_FINISHINGWITH=57, 
		LOGICAL_OR=58, LOGICAL_AND=59, LOGICAL_NOT=60, LOGICAL_IGNORE=61, LOGICAL_TRUE=62, 
		LOGICAL_FALSE=63, MATH_MULTIPLY=64, MATH_DIVISION=65, MATH_PLUS=66, MATH_MINUS=67, 
		AGGR_COUNT=68, AGGR_SUM=69, AGGR_MIN=70, AGGR_MAX=71, AGGR_AVERAGE=72, 
		AGGR_MODE=73, AGGR_MEAN=74, AGGR_MEDIAN=75, CMP_EQUAL=76, BRACKET_ROUND_OPENED=77, 
		BRACKET_ROUND_CLOSED=78, BRACKET_SQUARE_OPENED=79, BRACKET_SQUARE_CLOSED=80, 
		BRACKET_CURLY_OPENED=81, BRACKET_CURLY_CLOSED=82, SEPARATOR=83, DATE=84, 
		INT=85, SIMPLE_ID=86, ENHANCED_ID=87, WHITESPACE=88;
	public static final String[] tokenNames = {
		"<INVALID>", "MARKED_ID", "VALUE", "NULL_VALUE", "POS_START_INCL", "POS_END_INCL", 
		"POS_START_EXCL", "POS_END_EXCL", "STMT_GET", "STMT_SELECT", "STMT_INSERT", 
		"STMT_OPEN", "STMT_LOAD", "STMT_UNLOAD", "STMT_ALIVE", "STMT_ADD", "STMT_DROP", 
		"STMT_MODIFY", "STMT_GRANT", "STMT_REVOKE", "STMT_ASSIGN", "STMT_REMOVE", 
		"PROP_AUTOLOAD", "PROP_FORCE", "PROP_PASSWORD", "TYPE_TIMESERIES", "TYPE_RECORDS", 
		"TYPE_MODELS", "TYPE_VERSION", "TYPE_PERMISSIONS", "TYPE_ROLES", "TYPE_USERS", 
		"TYPE_PERMISSION", "TYPE_ROLE", "TYPE_USER", "OP_FROM", "OP_OF", "OP_TO", 
		"OP_IN", "OP_INTO", "OP_SET", "OP_VALUES", "OP_ALIAS", "OP_GROUPBY", "OP_FILTERBY", 
		"OP_TRANSPOSE", "OP_IDONLY", "OP_WITH", "IR_EQUALTO", "IR_BEFORE", "IR_AFTER", 
		"IR_MEETING", "IR_OVERLAPPING", "IR_DURING", "IR_WITHIN", "IR_CONTAINING", 
		"IR_STARTINGWITH", "IR_FINISHINGWITH", "LOGICAL_OR", "LOGICAL_AND", "LOGICAL_NOT", 
		"LOGICAL_IGNORE", "LOGICAL_TRUE", "LOGICAL_FALSE", "'*'", "'/'", "'+'", 
		"'-'", "AGGR_COUNT", "AGGR_SUM", "AGGR_MIN", "AGGR_MAX", "AGGR_AVERAGE", 
		"AGGR_MODE", "AGGR_MEAN", "AGGR_MEDIAN", "'='", "'('", "')'", "'['", "']'", 
		"'{'", "'}'", "','", "DATE", "INT", "SIMPLE_ID", "ENHANCED_ID", "WHITESPACE"
	};
	public static final int
		RULE_root = 0, RULE_exprAdd = 1, RULE_exprWithPassword = 2, RULE_exprWithPermissions = 3, 
		RULE_exprWithRoles = 4, RULE_exprDrop = 5, RULE_exprModify = 6, RULE_exprSetPassword = 7, 
		RULE_exprGrant = 8, RULE_exprRevoke = 9, RULE_exprAssign = 10, RULE_exprAssignSingleRole = 11, 
		RULE_exprAssignMultipleRoles = 12, RULE_exprRemove = 13, RULE_exprRemoveSingleRole = 14, 
		RULE_exprRemoveMultipleRoles = 15, RULE_exprGet = 16, RULE_exprAlive = 17, 
		RULE_exprLoad = 18, RULE_exprLoadSetProperty = 19, RULE_exprUnload = 20, 
		RULE_exprUnloadSetProperty = 21, RULE_exprInsert = 22, RULE_exprStructure = 23, 
		RULE_exprValues = 24, RULE_exprSelect = 25, RULE_exprSelectRecords = 26, 
		RULE_exprSelectTimeSeries = 27, RULE_exprMeasure = 28, RULE_exprInterval = 29, 
		RULE_exprComp = 30, RULE_exprGroup = 31, RULE_exprAggregate = 32, RULE_compNamedMeasure = 33, 
		RULE_compMeasure = 34, RULE_compMeasureAtom = 35, RULE_compDescriptorEqual = 36, 
		RULE_compDescValueTupel = 37, RULE_compGroupIgnore = 38, RULE_compAggrFunction = 39, 
		RULE_compDescriptorFormula = 40, RULE_compDescriptorFormulaAtom = 41, 
		RULE_compStructureElement = 42, RULE_compValueElement = 43, RULE_selectorModelId = 44, 
		RULE_selectorDescriptorId = 45, RULE_selectorAlias = 46, RULE_selectorDateInterval = 47, 
		RULE_selectorIntInterval = 48, RULE_selectorDateIntervalWithNull = 49, 
		RULE_selectorIntIntervalWithNull = 50, RULE_selectorDateValueOrNull = 51, 
		RULE_selectorIntValueOrNull = 52, RULE_selectorOpenInterval = 53, RULE_selectorCloseInterval = 54, 
		RULE_selectorDescValue = 55, RULE_selectorAggrFunctionName = 56, RULE_selectorFirstMathOperator = 57, 
		RULE_selectorSecondMathOperator = 58, RULE_selectorIntervalDef = 59, RULE_selectorBoolean = 60, 
		RULE_selectorIntervalRelation = 61, RULE_selectorValueList = 62;
	public static final String[] ruleNames = {
		"root", "exprAdd", "exprWithPassword", "exprWithPermissions", "exprWithRoles", 
		"exprDrop", "exprModify", "exprSetPassword", "exprGrant", "exprRevoke", 
		"exprAssign", "exprAssignSingleRole", "exprAssignMultipleRoles", "exprRemove", 
		"exprRemoveSingleRole", "exprRemoveMultipleRoles", "exprGet", "exprAlive", 
		"exprLoad", "exprLoadSetProperty", "exprUnload", "exprUnloadSetProperty", 
		"exprInsert", "exprStructure", "exprValues", "exprSelect", "exprSelectRecords", 
		"exprSelectTimeSeries", "exprMeasure", "exprInterval", "exprComp", "exprGroup", 
		"exprAggregate", "compNamedMeasure", "compMeasure", "compMeasureAtom", 
		"compDescriptorEqual", "compDescValueTupel", "compGroupIgnore", "compAggrFunction", 
		"compDescriptorFormula", "compDescriptorFormulaAtom", "compStructureElement", 
		"compValueElement", "selectorModelId", "selectorDescriptorId", "selectorAlias", 
		"selectorDateInterval", "selectorIntInterval", "selectorDateIntervalWithNull", 
		"selectorIntIntervalWithNull", "selectorDateValueOrNull", "selectorIntValueOrNull", 
		"selectorOpenInterval", "selectorCloseInterval", "selectorDescValue", 
		"selectorAggrFunctionName", "selectorFirstMathOperator", "selectorSecondMathOperator", 
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
			setState(139);
			switch (_input.LA(1)) {
			case STMT_INSERT:
				{
				setState(126); exprInsert();
				}
				break;
			case STMT_SELECT:
				{
				setState(127); exprSelect();
				}
				break;
			case STMT_OPEN:
			case STMT_LOAD:
				{
				setState(128); exprLoad();
				}
				break;
			case STMT_UNLOAD:
				{
				setState(129); exprUnload();
				}
				break;
			case STMT_ALIVE:
				{
				setState(130); exprAlive();
				}
				break;
			case STMT_GET:
				{
				setState(131); exprGet();
				}
				break;
			case STMT_ADD:
				{
				setState(132); exprAdd();
				}
				break;
			case STMT_DROP:
				{
				setState(133); exprDrop();
				}
				break;
			case STMT_MODIFY:
				{
				setState(134); exprModify();
				}
				break;
			case STMT_GRANT:
				{
				setState(135); exprGrant();
				}
				break;
			case STMT_REVOKE:
				{
				setState(136); exprRevoke();
				}
				break;
			case STMT_ASSIGN:
				{
				setState(137); exprAssign();
				}
				break;
			case STMT_REMOVE:
				{
				setState(138); exprRemove();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(141); match(EOF);
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
			setState(143); match(STMT_ADD);
			setState(164);
			switch (_input.LA(1)) {
			case TYPE_USER:
				{
				setState(144); match(TYPE_USER);
				setState(145); match(VALUE);
				setState(146); exprWithPassword();
				setState(157);
				_la = _input.LA(1);
				if (_la==OP_WITH) {
					{
					setState(155);
					switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
					case 1:
						{
						setState(147); exprWithRoles();
						setState(149);
						_la = _input.LA(1);
						if (_la==OP_WITH) {
							{
							setState(148); exprWithPermissions();
							}
						}

						}
						break;

					case 2:
						{
						setState(151); exprWithPermissions();
						setState(153);
						_la = _input.LA(1);
						if (_la==OP_WITH) {
							{
							setState(152); exprWithRoles();
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
				setState(159); match(TYPE_ROLE);
				setState(160); match(VALUE);
				setState(162);
				_la = _input.LA(1);
				if (_la==OP_WITH) {
					{
					setState(161); exprWithPermissions();
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
			setState(166); match(OP_WITH);
			setState(167); match(PROP_PASSWORD);
			setState(168); match(VALUE);
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
			setState(170); match(OP_WITH);
			setState(171); match(TYPE_PERMISSIONS);
			setState(172); selectorValueList();
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
			setState(174); match(OP_WITH);
			setState(175); match(TYPE_ROLES);
			setState(176); selectorValueList();
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
			setState(178); match(STMT_DROP);
			setState(179);
			_la = _input.LA(1);
			if ( !(_la==TYPE_ROLE || _la==TYPE_USER) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(180); match(VALUE);
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
			setState(182); match(STMT_MODIFY);
			setState(183); match(TYPE_USER);
			setState(184); match(VALUE);
			setState(185); exprSetPassword();
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
			setState(187); match(OP_SET);
			setState(188); match(PROP_PASSWORD);
			setState(189); match(OP_TO);
			setState(190); match(VALUE);
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
			setState(192); match(STMT_GRANT);
			setState(193); match(TYPE_PERMISSIONS);
			setState(194); selectorValueList();
			setState(195); match(OP_TO);
			setState(196);
			_la = _input.LA(1);
			if ( !(_la==TYPE_ROLE || _la==TYPE_USER) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(197); match(VALUE);
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
			setState(199); match(STMT_REVOKE);
			setState(200); match(TYPE_PERMISSIONS);
			setState(201); selectorValueList();
			setState(202); match(OP_FROM);
			setState(203);
			_la = _input.LA(1);
			if ( !(_la==TYPE_ROLE || _la==TYPE_USER) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(204); match(VALUE);
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
			setState(206); match(STMT_ASSIGN);
			setState(209);
			switch (_input.LA(1)) {
			case TYPE_ROLE:
				{
				setState(207); exprAssignSingleRole();
				}
				break;
			case TYPE_ROLES:
				{
				setState(208); exprAssignMultipleRoles();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(211); match(OP_TO);
			setState(212); match(TYPE_USER);
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
			setState(215); match(TYPE_ROLE);
			setState(216); match(VALUE);
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
			setState(221); match(STMT_REMOVE);
			setState(224);
			switch (_input.LA(1)) {
			case TYPE_ROLE:
				{
				setState(222); exprRemoveSingleRole();
				}
				break;
			case TYPE_ROLES:
				{
				setState(223); exprRemoveMultipleRoles();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(226); match(OP_FROM);
			setState(227); match(TYPE_USER);
			setState(228); match(VALUE);
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
			setState(230); match(TYPE_ROLE);
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
			setState(233); match(TYPE_ROLES);
			setState(234); selectorValueList();
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
			setState(236); match(STMT_GET);
			setState(237);
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
			setState(239); match(STMT_ALIVE);
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
			setState(241);
			_la = _input.LA(1);
			if ( !(_la==STMT_OPEN || _la==STMT_LOAD) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(245);
			switch (_input.LA(1)) {
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				{
				setState(242); selectorModelId();
				}
				break;
			case OP_FROM:
				{
				{
				setState(243); match(OP_FROM);
				setState(244); match(VALUE);
				}
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(256);
			_la = _input.LA(1);
			if (_la==OP_SET) {
				{
				setState(247); match(OP_SET);
				setState(248); exprLoadSetProperty();
				setState(253);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(249); match(SEPARATOR);
					setState(250); exprLoadSetProperty();
					}
					}
					setState(255);
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
			setState(264);
			switch (_input.LA(1)) {
			case PROP_AUTOLOAD:
				enterOuterAlt(_localctx, 1);
				{
				{
				setState(258); match(PROP_AUTOLOAD);
				setState(259); match(CMP_EQUAL);
				setState(260); selectorBoolean();
				}
				}
				break;
			case PROP_FORCE:
				enterOuterAlt(_localctx, 2);
				{
				{
				setState(261); match(PROP_FORCE);
				setState(262); match(CMP_EQUAL);
				setState(263); selectorBoolean();
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
			setState(266); match(STMT_UNLOAD);
			setState(267); selectorModelId();
			setState(277);
			_la = _input.LA(1);
			if (_la==OP_SET) {
				{
				setState(268); match(OP_SET);
				setState(269); exprUnloadSetProperty();
				setState(274);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(270); match(SEPARATOR);
					setState(271); exprUnloadSetProperty();
					}
					}
					setState(276);
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
			setState(279); match(PROP_AUTOLOAD);
			setState(280); match(CMP_EQUAL);
			setState(281); selectorBoolean();
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
			setState(283); match(STMT_INSERT);
			setState(284); match(OP_INTO);
			setState(285); selectorModelId();
			setState(286); exprStructure();
			setState(287); match(OP_VALUES);
			setState(288); exprValues();
			setState(293);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(289); match(SEPARATOR);
				setState(290); exprValues();
				}
				}
				setState(295);
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
			setState(296); match(BRACKET_ROUND_OPENED);
			setState(297); compStructureElement();
			setState(302);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(298); match(SEPARATOR);
				setState(299); compStructureElement();
				}
				}
				setState(304);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(305); match(BRACKET_ROUND_CLOSED);
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
			setState(307); match(BRACKET_ROUND_OPENED);
			setState(308); compValueElement();
			setState(313);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(309); match(SEPARATOR);
				setState(310); compValueElement();
				}
				}
				setState(315);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(316); match(BRACKET_ROUND_CLOSED);
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
		enterRule(_localctx, 50, RULE_exprSelect);
		try {
			setState(320);
			switch ( getInterpreter().adaptivePredict(_input,18,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(318); exprSelectRecords();
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(319); exprSelectTimeSeries();
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
		enterRule(_localctx, 52, RULE_exprSelectRecords);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(322); match(STMT_SELECT);
			setState(328);
			switch (_input.LA(1)) {
			case TYPE_RECORDS:
				{
				setState(323); match(TYPE_RECORDS);
				}
				break;
			case OP_IDONLY:
			case AGGR_COUNT:
				{
				setState(324);
				_la = _input.LA(1);
				if ( !(_la==OP_IDONLY || _la==AGGR_COUNT) ) {
				_errHandler.recoverInline(this);
				}
				consume();
				setState(325); match(BRACKET_ROUND_OPENED);
				setState(326); match(TYPE_RECORDS);
				setState(327); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(330); match(OP_FROM);
			setState(331); selectorModelId();
			setState(335);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << IR_EQUALTO) | (1L << IR_BEFORE) | (1L << IR_AFTER) | (1L << IR_MEETING) | (1L << IR_OVERLAPPING) | (1L << IR_DURING) | (1L << IR_WITHIN) | (1L << IR_CONTAINING) | (1L << IR_STARTINGWITH) | (1L << IR_FINISHINGWITH))) != 0)) {
				{
				setState(332); selectorIntervalRelation();
				setState(333); exprInterval();
				}
			}

			setState(339);
			_la = _input.LA(1);
			if (_la==OP_FILTERBY) {
				{
				setState(337); match(OP_FILTERBY);
				setState(338); exprComp(0);
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
		enterRule(_localctx, 54, RULE_exprSelectTimeSeries);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(341); match(STMT_SELECT);
			setState(347);
			switch (_input.LA(1)) {
			case TYPE_TIMESERIES:
				{
				setState(342); match(TYPE_TIMESERIES);
				}
				break;
			case OP_TRANSPOSE:
				{
				setState(343); match(OP_TRANSPOSE);
				setState(344); match(BRACKET_ROUND_OPENED);
				setState(345); match(TYPE_TIMESERIES);
				setState(346); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(351);
			_la = _input.LA(1);
			if (_la==OP_OF) {
				{
				setState(349); match(OP_OF);
				setState(350); exprMeasure();
				}
			}

			setState(353); match(OP_FROM);
			setState(354); selectorModelId();
			setState(357);
			_la = _input.LA(1);
			if (_la==OP_IN) {
				{
				setState(355); match(OP_IN);
				setState(356); exprInterval();
				}
			}

			setState(361);
			_la = _input.LA(1);
			if (_la==OP_FILTERBY) {
				{
				setState(359); match(OP_FILTERBY);
				setState(360); exprComp(0);
				}
			}

			setState(365);
			_la = _input.LA(1);
			if (_la==OP_GROUPBY) {
				{
				setState(363); match(OP_GROUPBY);
				setState(364); exprGroup();
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
		public CompNamedMeasureContext compNamedMeasure(int i) {
			return getRuleContext(CompNamedMeasureContext.class,i);
		}
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public List<CompNamedMeasureContext> compNamedMeasure() {
			return getRuleContexts(CompNamedMeasureContext.class);
		}
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
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
		enterRule(_localctx, 56, RULE_exprMeasure);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(367); compNamedMeasure();
			setState(372);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(368); match(SEPARATOR);
				setState(369); compNamedMeasure();
				}
				}
				setState(374);
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
		enterRule(_localctx, 58, RULE_exprInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(375); selectorOpenInterval();
			setState(378);
			switch (_input.LA(1)) {
			case DATE:
				{
				setState(376); selectorDateInterval();
				}
				break;
			case INT:
				{
				setState(377); selectorIntInterval();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(380); selectorCloseInterval();
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
		int _startState = 60;
		enterRecursionRule(_localctx, RULE_exprComp);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(390);
			switch (_input.LA(1)) {
			case LOGICAL_NOT:
				{
				setState(383); match(LOGICAL_NOT);
				setState(384); exprComp(2);
				}
				break;
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				{
				setState(385); compDescriptorEqual();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(386); match(BRACKET_ROUND_OPENED);
				setState(387); exprComp(0);
				setState(388); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(397);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,30,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new ExprCompContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_exprComp);
					setState(392);
					if (!(1 >= _localctx._p)) throw new FailedPredicateException(this, "1 >= $_p");
					setState(393);
					_la = _input.LA(1);
					if ( !(_la==LOGICAL_OR || _la==LOGICAL_AND) ) {
					_errHandler.recoverInline(this);
					}
					consume();
					setState(394); exprComp(2);
					}
					} 
				}
				setState(399);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,30,_ctx);
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
		enterRule(_localctx, 62, RULE_exprGroup);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(400); exprAggregate();
			setState(403);
			_la = _input.LA(1);
			if (_la==LOGICAL_IGNORE) {
				{
				setState(401); match(LOGICAL_IGNORE);
				setState(402); compGroupIgnore();
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
		enterRule(_localctx, 64, RULE_exprAggregate);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(405); selectorDescriptorId();
			setState(410);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(406); match(SEPARATOR);
				setState(407); selectorDescriptorId();
				}
				}
				setState(412);
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

	public static class CompNamedMeasureContext extends ParserRuleContext {
		public SelectorAliasContext selectorAlias() {
			return getRuleContext(SelectorAliasContext.class,0);
		}
		public CompMeasureContext compMeasure() {
			return getRuleContext(CompMeasureContext.class,0);
		}
		public TerminalNode OP_ALIAS() { return getToken(QueryGrammarParser.OP_ALIAS, 0); }
		public CompNamedMeasureContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compNamedMeasure; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompNamedMeasure(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompNamedMeasure(this);
		}
	}

	public final CompNamedMeasureContext compNamedMeasure() throws RecognitionException {
		CompNamedMeasureContext _localctx = new CompNamedMeasureContext(_ctx, getState());
		enterRule(_localctx, 66, RULE_compNamedMeasure);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(413); compMeasure(0);
			setState(416);
			_la = _input.LA(1);
			if (_la==OP_ALIAS) {
				{
				setState(414); match(OP_ALIAS);
				setState(415); selectorAlias();
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

	public static class CompMeasureContext extends ParserRuleContext {
		public int _p;
		public CompMeasureContext compMeasure() {
			return getRuleContext(CompMeasureContext.class,0);
		}
		public CompMeasureAtomContext compMeasureAtom() {
			return getRuleContext(CompMeasureAtomContext.class,0);
		}
		public SelectorSecondMathOperatorContext selectorSecondMathOperator() {
			return getRuleContext(SelectorSecondMathOperatorContext.class,0);
		}
		public CompMeasureContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public CompMeasureContext(ParserRuleContext parent, int invokingState, int _p) {
			super(parent, invokingState);
			this._p = _p;
		}
		@Override public int getRuleIndex() { return RULE_compMeasure; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompMeasure(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompMeasure(this);
		}
	}

	public final CompMeasureContext compMeasure(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		CompMeasureContext _localctx = new CompMeasureContext(_ctx, _parentState, _p);
		CompMeasureContext _prevctx = _localctx;
		int _startState = 68;
		enterRecursionRule(_localctx, RULE_compMeasure);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(419); compMeasureAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(427);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,34,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompMeasureContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compMeasure);
					setState(421);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(422); selectorSecondMathOperator();
					setState(423); compMeasureAtom(0);
					}
					} 
				}
				setState(429);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,34,_ctx);
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

	public static class CompMeasureAtomContext extends ParserRuleContext {
		public int _p;
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public CompMeasureAtomContext compMeasureAtom(int i) {
			return getRuleContext(CompMeasureAtomContext.class,i);
		}
		public CompAggrFunctionContext compAggrFunction() {
			return getRuleContext(CompAggrFunctionContext.class,0);
		}
		public SelectorFirstMathOperatorContext selectorFirstMathOperator() {
			return getRuleContext(SelectorFirstMathOperatorContext.class,0);
		}
		public List<CompMeasureAtomContext> compMeasureAtom() {
			return getRuleContexts(CompMeasureAtomContext.class);
		}
		public CompMeasureContext compMeasure() {
			return getRuleContext(CompMeasureContext.class,0);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public CompMeasureAtomContext(ParserRuleContext parent, int invokingState) { super(parent, invokingState); }
		public CompMeasureAtomContext(ParserRuleContext parent, int invokingState, int _p) {
			super(parent, invokingState);
			this._p = _p;
		}
		@Override public int getRuleIndex() { return RULE_compMeasureAtom; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompMeasureAtom(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompMeasureAtom(this);
		}
	}

	public final CompMeasureAtomContext compMeasureAtom(int _p) throws RecognitionException {
		ParserRuleContext _parentctx = _ctx;
		int _parentState = getState();
		CompMeasureAtomContext _localctx = new CompMeasureAtomContext(_ctx, _parentState, _p);
		CompMeasureAtomContext _prevctx = _localctx;
		int _startState = 70;
		enterRecursionRule(_localctx, RULE_compMeasureAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(436);
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
				setState(431); compAggrFunction();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(432); match(BRACKET_ROUND_OPENED);
				setState(433); compMeasure(0);
				setState(434); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(444);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,36,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompMeasureAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compMeasureAtom);
					setState(438);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(439); selectorFirstMathOperator();
					setState(440); compMeasureAtom(0);
					}
					} 
				}
				setState(446);
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

	public static class CompDescriptorEqualContext extends ParserRuleContext {
		public SelectorDescValueContext selectorDescValue() {
			return getRuleContext(SelectorDescValueContext.class,0);
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
		enterRule(_localctx, 72, RULE_compDescriptorEqual);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(447); selectorDescriptorId();
			setState(448); match(CMP_EQUAL);
			setState(449); selectorDescValue();
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
		public List<SelectorDescValueContext> selectorDescValue() {
			return getRuleContexts(SelectorDescValueContext.class);
		}
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public SelectorDescValueContext selectorDescValue(int i) {
			return getRuleContext(SelectorDescValueContext.class,i);
		}
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
		enterRule(_localctx, 74, RULE_compDescValueTupel);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(451); match(BRACKET_ROUND_OPENED);
			setState(452); selectorDescValue();
			setState(457);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(453); match(SEPARATOR);
				setState(454); selectorDescValue();
				}
				}
				setState(459);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(460); match(BRACKET_ROUND_CLOSED);
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
		enterRule(_localctx, 76, RULE_compGroupIgnore);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(462); match(BRACKET_CURLY_OPENED);
			setState(463); compDescValueTupel();
			setState(468);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(464); match(SEPARATOR);
				setState(465); compDescValueTupel();
				}
				}
				setState(470);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(471); match(BRACKET_CURLY_CLOSED);
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

	public static class CompAggrFunctionContext extends ParserRuleContext {
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public SelectorAggrFunctionNameContext selectorAggrFunctionName() {
			return getRuleContext(SelectorAggrFunctionNameContext.class,0);
		}
		public CompDescriptorFormulaContext compDescriptorFormula() {
			return getRuleContext(CompDescriptorFormulaContext.class,0);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public CompAggrFunctionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_compAggrFunction; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterCompAggrFunction(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitCompAggrFunction(this);
		}
	}

	public final CompAggrFunctionContext compAggrFunction() throws RecognitionException {
		CompAggrFunctionContext _localctx = new CompAggrFunctionContext(_ctx, getState());
		enterRule(_localctx, 78, RULE_compAggrFunction);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(473); selectorAggrFunctionName();
			setState(474); match(BRACKET_ROUND_OPENED);
			setState(475); compDescriptorFormula(0);
			setState(476); match(BRACKET_ROUND_CLOSED);
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
		int _startState = 80;
		enterRecursionRule(_localctx, RULE_compDescriptorFormula);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(479); compDescriptorFormulaAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(487);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,39,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDescriptorFormulaContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDescriptorFormula);
					setState(481);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(482); selectorSecondMathOperator();
					setState(483); compDescriptorFormulaAtom(0);
					}
					} 
				}
				setState(489);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,39,_ctx);
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
		int _startState = 82;
		enterRecursionRule(_localctx, RULE_compDescriptorFormulaAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(496);
			switch (_input.LA(1)) {
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				{
				setState(491); selectorDescriptorId();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(492); match(BRACKET_ROUND_OPENED);
				setState(493); compDescriptorFormula(0);
				setState(494); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(504);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,41,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDescriptorFormulaAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDescriptorFormulaAtom);
					setState(498);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(499); selectorFirstMathOperator();
					setState(500); compDescriptorFormulaAtom(0);
					}
					} 
				}
				setState(506);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,41,_ctx);
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
		enterRule(_localctx, 84, RULE_compStructureElement);
		try {
			setState(509);
			switch (_input.LA(1)) {
			case POS_START_INCL:
			case POS_END_INCL:
			case POS_START_EXCL:
			case POS_END_EXCL:
				enterOuterAlt(_localctx, 1);
				{
				setState(507); selectorIntervalDef();
				}
				break;
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				enterOuterAlt(_localctx, 2);
				{
				setState(508); selectorDescriptorId();
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
		public SelectorDescValueContext selectorDescValue() {
			return getRuleContext(SelectorDescValueContext.class,0);
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
		enterRule(_localctx, 86, RULE_compValueElement);
		try {
			setState(514);
			switch ( getInterpreter().adaptivePredict(_input,43,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(511); selectorDateValueOrNull();
				}
				break;

			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(512); selectorIntValueOrNull();
				}
				break;

			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(513); selectorDescValue();
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
		enterRule(_localctx, 88, RULE_selectorModelId);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(516);
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
		enterRule(_localctx, 90, RULE_selectorDescriptorId);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(518);
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
		enterRule(_localctx, 92, RULE_selectorAlias);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(520);
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
		enterRule(_localctx, 94, RULE_selectorDateInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(522); match(DATE);
			setState(523); match(SEPARATOR);
			setState(524); match(DATE);
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
		enterRule(_localctx, 96, RULE_selectorIntInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(526); match(INT);
			setState(527); match(SEPARATOR);
			setState(528); match(INT);
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
		enterRule(_localctx, 98, RULE_selectorDateIntervalWithNull);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(530);
			_la = _input.LA(1);
			if ( !(_la==NULL_VALUE || _la==DATE) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(531); match(SEPARATOR);
			setState(532);
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
		enterRule(_localctx, 100, RULE_selectorIntIntervalWithNull);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(534);
			_la = _input.LA(1);
			if ( !(_la==NULL_VALUE || _la==INT) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(535); match(SEPARATOR);
			setState(536);
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
		enterRule(_localctx, 102, RULE_selectorDateValueOrNull);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(538);
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
		enterRule(_localctx, 104, RULE_selectorIntValueOrNull);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(540);
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
		enterRule(_localctx, 106, RULE_selectorOpenInterval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(542);
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
		enterRule(_localctx, 108, RULE_selectorCloseInterval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(544);
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

	public static class SelectorDescValueContext extends ParserRuleContext {
		public TerminalNode VALUE() { return getToken(QueryGrammarParser.VALUE, 0); }
		public TerminalNode NULL_VALUE() { return getToken(QueryGrammarParser.NULL_VALUE, 0); }
		public SelectorDescValueContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorDescValue; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorDescValue(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorDescValue(this);
		}
	}

	public final SelectorDescValueContext selectorDescValue() throws RecognitionException {
		SelectorDescValueContext _localctx = new SelectorDescValueContext(_ctx, getState());
		enterRule(_localctx, 110, RULE_selectorDescValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(546);
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

	public static class SelectorAggrFunctionNameContext extends ParserRuleContext {
		public TerminalNode AGGR_SUM() { return getToken(QueryGrammarParser.AGGR_SUM, 0); }
		public TerminalNode SIMPLE_ID() { return getToken(QueryGrammarParser.SIMPLE_ID, 0); }
		public TerminalNode AGGR_MEAN() { return getToken(QueryGrammarParser.AGGR_MEAN, 0); }
		public TerminalNode AGGR_AVERAGE() { return getToken(QueryGrammarParser.AGGR_AVERAGE, 0); }
		public TerminalNode AGGR_MIN() { return getToken(QueryGrammarParser.AGGR_MIN, 0); }
		public TerminalNode AGGR_MODE() { return getToken(QueryGrammarParser.AGGR_MODE, 0); }
		public TerminalNode AGGR_MAX() { return getToken(QueryGrammarParser.AGGR_MAX, 0); }
		public TerminalNode AGGR_MEDIAN() { return getToken(QueryGrammarParser.AGGR_MEDIAN, 0); }
		public TerminalNode AGGR_COUNT() { return getToken(QueryGrammarParser.AGGR_COUNT, 0); }
		public SelectorAggrFunctionNameContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorAggrFunctionName; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorAggrFunctionName(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorAggrFunctionName(this);
		}
	}

	public final SelectorAggrFunctionNameContext selectorAggrFunctionName() throws RecognitionException {
		SelectorAggrFunctionNameContext _localctx = new SelectorAggrFunctionNameContext(_ctx, getState());
		enterRule(_localctx, 112, RULE_selectorAggrFunctionName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(548);
			_la = _input.LA(1);
			if ( !(((((_la - 68)) & ~0x3f) == 0 && ((1L << (_la - 68)) & ((1L << (AGGR_COUNT - 68)) | (1L << (AGGR_SUM - 68)) | (1L << (AGGR_MIN - 68)) | (1L << (AGGR_MAX - 68)) | (1L << (AGGR_AVERAGE - 68)) | (1L << (AGGR_MODE - 68)) | (1L << (AGGR_MEAN - 68)) | (1L << (AGGR_MEDIAN - 68)) | (1L << (SIMPLE_ID - 68)))) != 0)) ) {
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
		enterRule(_localctx, 114, RULE_selectorFirstMathOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(550);
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
		enterRule(_localctx, 116, RULE_selectorSecondMathOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(552);
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
		enterRule(_localctx, 118, RULE_selectorIntervalDef);
		int _la;
		try {
			setState(556);
			switch (_input.LA(1)) {
			case POS_START_INCL:
			case POS_START_EXCL:
				enterOuterAlt(_localctx, 1);
				{
				setState(554);
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
				setState(555);
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
		enterRule(_localctx, 120, RULE_selectorBoolean);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(558);
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
		enterRule(_localctx, 122, RULE_selectorIntervalRelation);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(560);
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
		enterRule(_localctx, 124, RULE_selectorValueList);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(562); match(VALUE);
			setState(567);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(563); match(SEPARATOR);
				setState(564); match(VALUE);
				}
				}
				setState(569);
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
		case 30: return exprComp_sempred((ExprCompContext)_localctx, predIndex);

		case 34: return compMeasure_sempred((CompMeasureContext)_localctx, predIndex);

		case 35: return compMeasureAtom_sempred((CompMeasureAtomContext)_localctx, predIndex);

		case 40: return compDescriptorFormula_sempred((CompDescriptorFormulaContext)_localctx, predIndex);

		case 41: return compDescriptorFormulaAtom_sempred((CompDescriptorFormulaAtomContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean exprComp_sempred(ExprCompContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0: return 1 >= _localctx._p;
		}
		return true;
	}
	private boolean compMeasureAtom_sempred(CompMeasureAtomContext _localctx, int predIndex) {
		switch (predIndex) {
		case 2: return 2 >= _localctx._p;
		}
		return true;
	}
	private boolean compDescriptorFormula_sempred(CompDescriptorFormulaContext _localctx, int predIndex) {
		switch (predIndex) {
		case 3: return 2 >= _localctx._p;
		}
		return true;
	}
	private boolean compDescriptorFormulaAtom_sempred(CompDescriptorFormulaAtomContext _localctx, int predIndex) {
		switch (predIndex) {
		case 4: return 2 >= _localctx._p;
		}
		return true;
	}
	private boolean compMeasure_sempred(CompMeasureContext _localctx, int predIndex) {
		switch (predIndex) {
		case 1: return 2 >= _localctx._p;
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\3Z\u023d\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t+\4"+
		",\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t\61\4\62\t\62\4\63\t\63\4\64\t"+
		"\64\4\65\t\65\4\66\t\66\4\67\t\67\48\t8\49\t9\4:\t:\4;\t;\4<\t<\4=\t="+
		"\4>\t>\4?\t?\4@\t@\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2"+
		"\5\2\u008e\n\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\5\3\u0098\n\3\3\3\3\3\5"+
		"\3\u009c\n\3\5\3\u009e\n\3\5\3\u00a0\n\3\3\3\3\3\3\3\5\3\u00a5\n\3\5\3"+
		"\u00a7\n\3\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\6\3\6\3\6\3\6\3\7\3\7\3\7"+
		"\3\7\3\b\3\b\3\b\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\n\3\n\3\n\3\n\3\n\3\n\3"+
		"\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\f\3\f\3\f\5\f\u00d4\n\f\3\f\3"+
		"\f\3\f\3\f\3\r\3\r\3\r\3\16\3\16\3\16\3\17\3\17\3\17\5\17\u00e3\n\17\3"+
		"\17\3\17\3\17\3\17\3\20\3\20\3\20\3\21\3\21\3\21\3\22\3\22\3\22\3\23\3"+
		"\23\3\24\3\24\3\24\3\24\5\24\u00f8\n\24\3\24\3\24\3\24\3\24\7\24\u00fe"+
		"\n\24\f\24\16\24\u0101\13\24\5\24\u0103\n\24\3\25\3\25\3\25\3\25\3\25"+
		"\3\25\5\25\u010b\n\25\3\26\3\26\3\26\3\26\3\26\3\26\7\26\u0113\n\26\f"+
		"\26\16\26\u0116\13\26\5\26\u0118\n\26\3\27\3\27\3\27\3\27\3\30\3\30\3"+
		"\30\3\30\3\30\3\30\3\30\3\30\7\30\u0126\n\30\f\30\16\30\u0129\13\30\3"+
		"\31\3\31\3\31\3\31\7\31\u012f\n\31\f\31\16\31\u0132\13\31\3\31\3\31\3"+
		"\32\3\32\3\32\3\32\7\32\u013a\n\32\f\32\16\32\u013d\13\32\3\32\3\32\3"+
		"\33\3\33\5\33\u0143\n\33\3\34\3\34\3\34\3\34\3\34\3\34\5\34\u014b\n\34"+
		"\3\34\3\34\3\34\3\34\3\34\5\34\u0152\n\34\3\34\3\34\5\34\u0156\n\34\3"+
		"\35\3\35\3\35\3\35\3\35\3\35\5\35\u015e\n\35\3\35\3\35\5\35\u0162\n\35"+
		"\3\35\3\35\3\35\3\35\5\35\u0168\n\35\3\35\3\35\5\35\u016c\n\35\3\35\3"+
		"\35\5\35\u0170\n\35\3\36\3\36\3\36\7\36\u0175\n\36\f\36\16\36\u0178\13"+
		"\36\3\37\3\37\3\37\5\37\u017d\n\37\3\37\3\37\3 \3 \3 \3 \3 \3 \3 \3 \5"+
		" \u0189\n \3 \3 \3 \7 \u018e\n \f \16 \u0191\13 \3!\3!\3!\5!\u0196\n!"+
		"\3\"\3\"\3\"\7\"\u019b\n\"\f\"\16\"\u019e\13\"\3#\3#\3#\5#\u01a3\n#\3"+
		"$\3$\3$\3$\3$\3$\3$\7$\u01ac\n$\f$\16$\u01af\13$\3%\3%\3%\3%\3%\3%\5%"+
		"\u01b7\n%\3%\3%\3%\3%\7%\u01bd\n%\f%\16%\u01c0\13%\3&\3&\3&\3&\3\'\3\'"+
		"\3\'\3\'\7\'\u01ca\n\'\f\'\16\'\u01cd\13\'\3\'\3\'\3(\3(\3(\3(\7(\u01d5"+
		"\n(\f(\16(\u01d8\13(\3(\3(\3)\3)\3)\3)\3)\3*\3*\3*\3*\3*\3*\3*\7*\u01e8"+
		"\n*\f*\16*\u01eb\13*\3+\3+\3+\3+\3+\3+\5+\u01f3\n+\3+\3+\3+\3+\7+\u01f9"+
		"\n+\f+\16+\u01fc\13+\3,\3,\5,\u0200\n,\3-\3-\3-\5-\u0205\n-\3.\3.\3/\3"+
		"/\3\60\3\60\3\61\3\61\3\61\3\61\3\62\3\62\3\62\3\62\3\63\3\63\3\63\3\63"+
		"\3\64\3\64\3\64\3\64\3\65\3\65\3\66\3\66\3\67\3\67\38\38\39\39\3:\3:\3"+
		";\3;\3<\3<\3=\3=\5=\u022f\n=\3>\3>\3?\3?\3@\3@\3@\7@\u0238\n@\f@\16@\u023b"+
		"\13@\3@\2A\2\4\6\b\n\f\16\20\22\24\26\30\32\34\36 \"$&(*,.\60\62\64\66"+
		"8:<>@BDFHJLNPRTVXZ\\^`bdfhjlnprtvxz|~\2\24\3\2#$\3\2\35!\3\2\r\16\4\2"+
		"\60\60FF\3\2<=\4\2\3\3XY\4\2\5\5VV\4\2\5\5WW\4\2OOQQ\4\2PPRR\3\2\4\5\4"+
		"\2FMXX\3\2BC\3\2DE\4\2\6\6\b\b\4\2\7\7\t\t\3\2@A\3\2\62;\u0238\2\u008d"+
		"\3\2\2\2\4\u0091\3\2\2\2\6\u00a8\3\2\2\2\b\u00ac\3\2\2\2\n\u00b0\3\2\2"+
		"\2\f\u00b4\3\2\2\2\16\u00b8\3\2\2\2\20\u00bd\3\2\2\2\22\u00c2\3\2\2\2"+
		"\24\u00c9\3\2\2\2\26\u00d0\3\2\2\2\30\u00d9\3\2\2\2\32\u00dc\3\2\2\2\34"+
		"\u00df\3\2\2\2\36\u00e8\3\2\2\2 \u00eb\3\2\2\2\"\u00ee\3\2\2\2$\u00f1"+
		"\3\2\2\2&\u00f3\3\2\2\2(\u010a\3\2\2\2*\u010c\3\2\2\2,\u0119\3\2\2\2."+
		"\u011d\3\2\2\2\60\u012a\3\2\2\2\62\u0135\3\2\2\2\64\u0142\3\2\2\2\66\u0144"+
		"\3\2\2\28\u0157\3\2\2\2:\u0171\3\2\2\2<\u0179\3\2\2\2>\u0188\3\2\2\2@"+
		"\u0192\3\2\2\2B\u0197\3\2\2\2D\u019f\3\2\2\2F\u01a4\3\2\2\2H\u01b6\3\2"+
		"\2\2J\u01c1\3\2\2\2L\u01c5\3\2\2\2N\u01d0\3\2\2\2P\u01db\3\2\2\2R\u01e0"+
		"\3\2\2\2T\u01f2\3\2\2\2V\u01ff\3\2\2\2X\u0204\3\2\2\2Z\u0206\3\2\2\2\\"+
		"\u0208\3\2\2\2^\u020a\3\2\2\2`\u020c\3\2\2\2b\u0210\3\2\2\2d\u0214\3\2"+
		"\2\2f\u0218\3\2\2\2h\u021c\3\2\2\2j\u021e\3\2\2\2l\u0220\3\2\2\2n\u0222"+
		"\3\2\2\2p\u0224\3\2\2\2r\u0226\3\2\2\2t\u0228\3\2\2\2v\u022a\3\2\2\2x"+
		"\u022e\3\2\2\2z\u0230\3\2\2\2|\u0232\3\2\2\2~\u0234\3\2\2\2\u0080\u008e"+
		"\5.\30\2\u0081\u008e\5\64\33\2\u0082\u008e\5&\24\2\u0083\u008e\5*\26\2"+
		"\u0084\u008e\5$\23\2\u0085\u008e\5\"\22\2\u0086\u008e\5\4\3\2\u0087\u008e"+
		"\5\f\7\2\u0088\u008e\5\16\b\2\u0089\u008e\5\22\n\2\u008a\u008e\5\24\13"+
		"\2\u008b\u008e\5\26\f\2\u008c\u008e\5\34\17\2\u008d\u0080\3\2\2\2\u008d"+
		"\u0081\3\2\2\2\u008d\u0082\3\2\2\2\u008d\u0083\3\2\2\2\u008d\u0084\3\2"+
		"\2\2\u008d\u0085\3\2\2\2\u008d\u0086\3\2\2\2\u008d\u0087\3\2\2\2\u008d"+
		"\u0088\3\2\2\2\u008d\u0089\3\2\2\2\u008d\u008a\3\2\2\2\u008d\u008b\3\2"+
		"\2\2\u008d\u008c\3\2\2\2\u008e\u008f\3\2\2\2\u008f\u0090\7\2\2\3\u0090"+
		"\3\3\2\2\2\u0091\u00a6\7\21\2\2\u0092\u0093\7$\2\2\u0093\u0094\7\4\2\2"+
		"\u0094\u009f\5\6\4\2\u0095\u0097\5\n\6\2\u0096\u0098\5\b\5\2\u0097\u0096"+
		"\3\2\2\2\u0097\u0098\3\2\2\2\u0098\u009e\3\2\2\2\u0099\u009b\5\b\5\2\u009a"+
		"\u009c\5\n\6\2\u009b\u009a\3\2\2\2\u009b\u009c\3\2\2\2\u009c\u009e\3\2"+
		"\2\2\u009d\u0095\3\2\2\2\u009d\u0099\3\2\2\2\u009e\u00a0\3\2\2\2\u009f"+
		"\u009d\3\2\2\2\u009f\u00a0\3\2\2\2\u00a0\u00a7\3\2\2\2\u00a1\u00a2\7#"+
		"\2\2\u00a2\u00a4\7\4\2\2\u00a3\u00a5\5\b\5\2\u00a4\u00a3\3\2\2\2\u00a4"+
		"\u00a5\3\2\2\2\u00a5\u00a7\3\2\2\2\u00a6\u0092\3\2\2\2\u00a6\u00a1\3\2"+
		"\2\2\u00a7\5\3\2\2\2\u00a8\u00a9\7\61\2\2\u00a9\u00aa\7\32\2\2\u00aa\u00ab"+
		"\7\4\2\2\u00ab\7\3\2\2\2\u00ac\u00ad\7\61\2\2\u00ad\u00ae\7\37\2\2\u00ae"+
		"\u00af\5~@\2\u00af\t\3\2\2\2\u00b0\u00b1\7\61\2\2\u00b1\u00b2\7 \2\2\u00b2"+
		"\u00b3\5~@\2\u00b3\13\3\2\2\2\u00b4\u00b5\7\22\2\2\u00b5\u00b6\t\2\2\2"+
		"\u00b6\u00b7\7\4\2\2\u00b7\r\3\2\2\2\u00b8\u00b9\7\23\2\2\u00b9\u00ba"+
		"\7$\2\2\u00ba\u00bb\7\4\2\2\u00bb\u00bc\5\20\t\2\u00bc\17\3\2\2\2\u00bd"+
		"\u00be\7*\2\2\u00be\u00bf\7\32\2\2\u00bf\u00c0\7\'\2\2\u00c0\u00c1\7\4"+
		"\2\2\u00c1\21\3\2\2\2\u00c2\u00c3\7\24\2\2\u00c3\u00c4\7\37\2\2\u00c4"+
		"\u00c5\5~@\2\u00c5\u00c6\7\'\2\2\u00c6\u00c7\t\2\2\2\u00c7\u00c8\7\4\2"+
		"\2\u00c8\23\3\2\2\2\u00c9\u00ca\7\25\2\2\u00ca\u00cb\7\37\2\2\u00cb\u00cc"+
		"\5~@\2\u00cc\u00cd\7%\2\2\u00cd\u00ce\t\2\2\2\u00ce\u00cf\7\4\2\2\u00cf"+
		"\25\3\2\2\2\u00d0\u00d3\7\26\2\2\u00d1\u00d4\5\30\r\2\u00d2\u00d4\5\32"+
		"\16\2\u00d3\u00d1\3\2\2\2\u00d3\u00d2\3\2\2\2\u00d4\u00d5\3\2\2\2\u00d5"+
		"\u00d6\7\'\2\2\u00d6\u00d7\7$\2\2\u00d7\u00d8\7\4\2\2\u00d8\27\3\2\2\2"+
		"\u00d9\u00da\7#\2\2\u00da\u00db\7\4\2\2\u00db\31\3\2\2\2\u00dc\u00dd\7"+
		" \2\2\u00dd\u00de\5~@\2\u00de\33\3\2\2\2\u00df\u00e2\7\27\2\2\u00e0\u00e3"+
		"\5\36\20\2\u00e1\u00e3\5 \21\2\u00e2\u00e0\3\2\2\2\u00e2\u00e1\3\2\2\2"+
		"\u00e3\u00e4\3\2\2\2\u00e4\u00e5\7%\2\2\u00e5\u00e6\7$\2\2\u00e6\u00e7"+
		"\7\4\2\2\u00e7\35\3\2\2\2\u00e8\u00e9\7#\2\2\u00e9\u00ea\7\4\2\2\u00ea"+
		"\37\3\2\2\2\u00eb\u00ec\7 \2\2\u00ec\u00ed\5~@\2\u00ed!\3\2\2\2\u00ee"+
		"\u00ef\7\n\2\2\u00ef\u00f0\t\3\2\2\u00f0#\3\2\2\2\u00f1\u00f2\7\20\2\2"+
		"\u00f2%\3\2\2\2\u00f3\u00f7\t\4\2\2\u00f4\u00f8\5Z.\2\u00f5\u00f6\7%\2"+
		"\2\u00f6\u00f8\7\4\2\2\u00f7\u00f4\3\2\2\2\u00f7\u00f5\3\2\2\2\u00f8\u0102"+
		"\3\2\2\2\u00f9\u00fa\7*\2\2\u00fa\u00ff\5(\25\2\u00fb\u00fc\7U\2\2\u00fc"+
		"\u00fe\5(\25\2\u00fd\u00fb\3\2\2\2\u00fe\u0101\3\2\2\2\u00ff\u00fd\3\2"+
		"\2\2\u00ff\u0100\3\2\2\2\u0100\u0103\3\2\2\2\u0101\u00ff\3\2\2\2\u0102"+
		"\u00f9\3\2\2\2\u0102\u0103\3\2\2\2\u0103\'\3\2\2\2\u0104\u0105\7\30\2"+
		"\2\u0105\u0106\7N\2\2\u0106\u010b\5z>\2\u0107\u0108\7\31\2\2\u0108\u0109"+
		"\7N\2\2\u0109\u010b\5z>\2\u010a\u0104\3\2\2\2\u010a\u0107\3\2\2\2\u010b"+
		")\3\2\2\2\u010c\u010d\7\17\2\2\u010d\u0117\5Z.\2\u010e\u010f\7*\2\2\u010f"+
		"\u0114\5,\27\2\u0110\u0111\7U\2\2\u0111\u0113\5,\27\2\u0112\u0110\3\2"+
		"\2\2\u0113\u0116\3\2\2\2\u0114\u0112\3\2\2\2\u0114\u0115\3\2\2\2\u0115"+
		"\u0118\3\2\2\2\u0116\u0114\3\2\2\2\u0117\u010e\3\2\2\2\u0117\u0118\3\2"+
		"\2\2\u0118+\3\2\2\2\u0119\u011a\7\30\2\2\u011a\u011b\7N\2\2\u011b\u011c"+
		"\5z>\2\u011c-\3\2\2\2\u011d\u011e\7\f\2\2\u011e\u011f\7)\2\2\u011f\u0120"+
		"\5Z.\2\u0120\u0121\5\60\31\2\u0121\u0122\7+\2\2\u0122\u0127\5\62\32\2"+
		"\u0123\u0124\7U\2\2\u0124\u0126\5\62\32\2\u0125\u0123\3\2\2\2\u0126\u0129"+
		"\3\2\2\2\u0127\u0125\3\2\2\2\u0127\u0128\3\2\2\2\u0128/\3\2\2\2\u0129"+
		"\u0127\3\2\2\2\u012a\u012b\7O\2\2\u012b\u0130\5V,\2\u012c\u012d\7U\2\2"+
		"\u012d\u012f\5V,\2\u012e\u012c\3\2\2\2\u012f\u0132\3\2\2\2\u0130\u012e"+
		"\3\2\2\2\u0130\u0131\3\2\2\2\u0131\u0133\3\2\2\2\u0132\u0130\3\2\2\2\u0133"+
		"\u0134\7P\2\2\u0134\61\3\2\2\2\u0135\u0136\7O\2\2\u0136\u013b\5X-\2\u0137"+
		"\u0138\7U\2\2\u0138\u013a\5X-\2\u0139\u0137\3\2\2\2\u013a\u013d\3\2\2"+
		"\2\u013b\u0139\3\2\2\2\u013b\u013c\3\2\2\2\u013c\u013e\3\2\2\2\u013d\u013b"+
		"\3\2\2\2\u013e\u013f\7P\2\2\u013f\63\3\2\2\2\u0140\u0143\5\66\34\2\u0141"+
		"\u0143\58\35\2\u0142\u0140\3\2\2\2\u0142\u0141\3\2\2\2\u0143\65\3\2\2"+
		"\2\u0144\u014a\7\13\2\2\u0145\u014b\7\34\2\2\u0146\u0147\t\5\2\2\u0147"+
		"\u0148\7O\2\2\u0148\u0149\7\34\2\2\u0149\u014b\7P\2\2\u014a\u0145\3\2"+
		"\2\2\u014a\u0146\3\2\2\2\u014b\u014c\3\2\2\2\u014c\u014d\7%\2\2\u014d"+
		"\u0151\5Z.\2\u014e\u014f\5|?\2\u014f\u0150\5<\37\2\u0150\u0152\3\2\2\2"+
		"\u0151\u014e\3\2\2\2\u0151\u0152\3\2\2\2\u0152\u0155\3\2\2\2\u0153\u0154"+
		"\7.\2\2\u0154\u0156\5> \2\u0155\u0153\3\2\2\2\u0155\u0156\3\2\2\2\u0156"+
		"\67\3\2\2\2\u0157\u015d\7\13\2\2\u0158\u015e\7\33\2\2\u0159\u015a\7/\2"+
		"\2\u015a\u015b\7O\2\2\u015b\u015c\7\33\2\2\u015c\u015e\7P\2\2\u015d\u0158"+
		"\3\2\2\2\u015d\u0159\3\2\2\2\u015e\u0161\3\2\2\2\u015f\u0160\7&\2\2\u0160"+
		"\u0162\5:\36\2\u0161\u015f\3\2\2\2\u0161\u0162\3\2\2\2\u0162\u0163\3\2"+
		"\2\2\u0163\u0164\7%\2\2\u0164\u0167\5Z.\2\u0165\u0166\7(\2\2\u0166\u0168"+
		"\5<\37\2\u0167\u0165\3\2\2\2\u0167\u0168\3\2\2\2\u0168\u016b\3\2\2\2\u0169"+
		"\u016a\7.\2\2\u016a\u016c\5> \2\u016b\u0169\3\2\2\2\u016b\u016c\3\2\2"+
		"\2\u016c\u016f\3\2\2\2\u016d\u016e\7-\2\2\u016e\u0170\5@!\2\u016f\u016d"+
		"\3\2\2\2\u016f\u0170\3\2\2\2\u01709\3\2\2\2\u0171\u0176\5D#\2\u0172\u0173"+
		"\7U\2\2\u0173\u0175\5D#\2\u0174\u0172\3\2\2\2\u0175\u0178\3\2\2\2\u0176"+
		"\u0174\3\2\2\2\u0176\u0177\3\2\2\2\u0177;\3\2\2\2\u0178\u0176\3\2\2\2"+
		"\u0179\u017c\5l\67\2\u017a\u017d\5`\61\2\u017b\u017d\5b\62\2\u017c\u017a"+
		"\3\2\2\2\u017c\u017b\3\2\2\2\u017d\u017e\3\2\2\2\u017e\u017f\5n8\2\u017f"+
		"=\3\2\2\2\u0180\u0181\b \1\2\u0181\u0182\7>\2\2\u0182\u0189\5> \2\u0183"+
		"\u0189\5J&\2\u0184\u0185\7O\2\2\u0185\u0186\5> \2\u0186\u0187\7P\2\2\u0187"+
		"\u0189\3\2\2\2\u0188\u0180\3\2\2\2\u0188\u0183\3\2\2\2\u0188\u0184\3\2"+
		"\2\2\u0189\u018f\3\2\2\2\u018a\u018b\6 \2\3\u018b\u018c\t\6\2\2\u018c"+
		"\u018e\5> \2\u018d\u018a\3\2\2\2\u018e\u0191\3\2\2\2\u018f\u018d\3\2\2"+
		"\2\u018f\u0190\3\2\2\2\u0190?\3\2\2\2\u0191\u018f\3\2\2\2\u0192\u0195"+
		"\5B\"\2\u0193\u0194\7?\2\2\u0194\u0196\5N(\2\u0195\u0193\3\2\2\2\u0195"+
		"\u0196\3\2\2\2\u0196A\3\2\2\2\u0197\u019c\5\\/\2\u0198\u0199\7U\2\2\u0199"+
		"\u019b\5\\/\2\u019a\u0198\3\2\2\2\u019b\u019e\3\2\2\2\u019c\u019a\3\2"+
		"\2\2\u019c\u019d\3\2\2\2\u019dC\3\2\2\2\u019e\u019c\3\2\2\2\u019f\u01a2"+
		"\5F$\2\u01a0\u01a1\7,\2\2\u01a1\u01a3\5^\60\2\u01a2\u01a0\3\2\2\2\u01a2"+
		"\u01a3\3\2\2\2\u01a3E\3\2\2\2\u01a4\u01a5\b$\1\2\u01a5\u01a6\5H%\2\u01a6"+
		"\u01ad\3\2\2\2\u01a7\u01a8\6$\3\3\u01a8\u01a9\5v<\2\u01a9\u01aa\5H%\2"+
		"\u01aa\u01ac\3\2\2\2\u01ab\u01a7\3\2\2\2\u01ac\u01af\3\2\2\2\u01ad\u01ab"+
		"\3\2\2\2\u01ad\u01ae\3\2\2\2\u01aeG\3\2\2\2\u01af\u01ad\3\2\2\2\u01b0"+
		"\u01b1\b%\1\2\u01b1\u01b7\5P)\2\u01b2\u01b3\7O\2\2\u01b3\u01b4\5F$\2\u01b4"+
		"\u01b5\7P\2\2\u01b5\u01b7\3\2\2\2\u01b6\u01b0\3\2\2\2\u01b6\u01b2\3\2"+
		"\2\2\u01b7\u01be\3\2\2\2\u01b8\u01b9\6%\4\3\u01b9\u01ba\5t;\2\u01ba\u01bb"+
		"\5H%\2\u01bb\u01bd\3\2\2\2\u01bc\u01b8\3\2\2\2\u01bd\u01c0\3\2\2\2\u01be"+
		"\u01bc\3\2\2\2\u01be\u01bf\3\2\2\2\u01bfI\3\2\2\2\u01c0\u01be\3\2\2\2"+
		"\u01c1\u01c2\5\\/\2\u01c2\u01c3\7N\2\2\u01c3\u01c4\5p9\2\u01c4K\3\2\2"+
		"\2\u01c5\u01c6\7O\2\2\u01c6\u01cb\5p9\2\u01c7\u01c8\7U\2\2\u01c8\u01ca"+
		"\5p9\2\u01c9\u01c7\3\2\2\2\u01ca\u01cd\3\2\2\2\u01cb\u01c9\3\2\2\2\u01cb"+
		"\u01cc\3\2\2\2\u01cc\u01ce\3\2\2\2\u01cd\u01cb\3\2\2\2\u01ce\u01cf\7P"+
		"\2\2\u01cfM\3\2\2\2\u01d0\u01d1\7S\2\2\u01d1\u01d6\5L\'\2\u01d2\u01d3"+
		"\7U\2\2\u01d3\u01d5\5L\'\2\u01d4\u01d2\3\2\2\2\u01d5\u01d8\3\2\2\2\u01d6"+
		"\u01d4\3\2\2\2\u01d6\u01d7\3\2\2\2\u01d7\u01d9\3\2\2\2\u01d8\u01d6\3\2"+
		"\2\2\u01d9\u01da\7T\2\2\u01daO\3\2\2\2\u01db\u01dc\5r:\2\u01dc\u01dd\7"+
		"O\2\2\u01dd\u01de\5R*\2\u01de\u01df\7P\2\2\u01dfQ\3\2\2\2\u01e0\u01e1"+
		"\b*\1\2\u01e1\u01e2\5T+\2\u01e2\u01e9\3\2\2\2\u01e3\u01e4\6*\5\3\u01e4"+
		"\u01e5\5v<\2\u01e5\u01e6\5T+\2\u01e6\u01e8\3\2\2\2\u01e7\u01e3\3\2\2\2"+
		"\u01e8\u01eb\3\2\2\2\u01e9\u01e7\3\2\2\2\u01e9\u01ea\3\2\2\2\u01eaS\3"+
		"\2\2\2\u01eb\u01e9\3\2\2\2\u01ec\u01ed\b+\1\2\u01ed\u01f3\5\\/\2\u01ee"+
		"\u01ef\7O\2\2\u01ef\u01f0\5R*\2\u01f0\u01f1\7P\2\2\u01f1\u01f3\3\2\2\2"+
		"\u01f2\u01ec\3\2\2\2\u01f2\u01ee\3\2\2\2\u01f3\u01fa\3\2\2\2\u01f4\u01f5"+
		"\6+\6\3\u01f5\u01f6\5t;\2\u01f6\u01f7\5T+\2\u01f7\u01f9\3\2\2\2\u01f8"+
		"\u01f4\3\2\2\2\u01f9\u01fc\3\2\2\2\u01fa\u01f8\3\2\2\2\u01fa\u01fb\3\2"+
		"\2\2\u01fbU\3\2\2\2\u01fc\u01fa\3\2\2\2\u01fd\u0200\5x=\2\u01fe\u0200"+
		"\5\\/\2\u01ff\u01fd\3\2\2\2\u01ff\u01fe\3\2\2\2\u0200W\3\2\2\2\u0201\u0205"+
		"\5h\65\2\u0202\u0205\5j\66\2\u0203\u0205\5p9\2\u0204\u0201\3\2\2\2\u0204"+
		"\u0202\3\2\2\2\u0204\u0203\3\2\2\2\u0205Y\3\2\2\2\u0206\u0207\t\7\2\2"+
		"\u0207[\3\2\2\2\u0208\u0209\t\7\2\2\u0209]\3\2\2\2\u020a\u020b\t\7\2\2"+
		"\u020b_\3\2\2\2\u020c\u020d\7V\2\2\u020d\u020e\7U\2\2\u020e\u020f\7V\2"+
		"\2\u020fa\3\2\2\2\u0210\u0211\7W\2\2\u0211\u0212\7U\2\2\u0212\u0213\7"+
		"W\2\2\u0213c\3\2\2\2\u0214\u0215\t\b\2\2\u0215\u0216\7U\2\2\u0216\u0217"+
		"\t\b\2\2\u0217e\3\2\2\2\u0218\u0219\t\t\2\2\u0219\u021a\7U\2\2\u021a\u021b"+
		"\t\t\2\2\u021bg\3\2\2\2\u021c\u021d\t\b\2\2\u021di\3\2\2\2\u021e\u021f"+
		"\t\t\2\2\u021fk\3\2\2\2\u0220\u0221\t\n\2\2\u0221m\3\2\2\2\u0222\u0223"+
		"\t\13\2\2\u0223o\3\2\2\2\u0224\u0225\t\f\2\2\u0225q\3\2\2\2\u0226\u0227"+
		"\t\r\2\2\u0227s\3\2\2\2\u0228\u0229\t\16\2\2\u0229u\3\2\2\2\u022a\u022b"+
		"\t\17\2\2\u022bw\3\2\2\2\u022c\u022f\t\20\2\2\u022d\u022f\t\21\2\2\u022e"+
		"\u022c\3\2\2\2\u022e\u022d\3\2\2\2\u022fy\3\2\2\2\u0230\u0231\t\22\2\2"+
		"\u0231{\3\2\2\2\u0232\u0233\t\23\2\2\u0233}\3\2\2\2\u0234\u0239\7\4\2"+
		"\2\u0235\u0236\7U\2\2\u0236\u0238\7\4\2\2\u0237\u0235\3\2\2\2\u0238\u023b"+
		"\3\2\2\2\u0239\u0237\3\2\2\2\u0239\u023a\3\2\2\2\u023a\177\3\2\2\2\u023b"+
		"\u0239\3\2\2\2\60\u008d\u0097\u009b\u009d\u009f\u00a4\u00a6\u00d3\u00e2"+
		"\u00f7\u00ff\u0102\u010a\u0114\u0117\u0127\u0130\u013b\u0142\u014a\u0151"+
		"\u0155\u015d\u0161\u0167\u016b\u016f\u0176\u017c\u0188\u018f\u0195\u019c"+
		"\u01a2\u01ad\u01b6\u01be\u01cb\u01d6\u01e9\u01f2\u01fa\u01ff\u0204\u022e"+
		"\u0239";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}