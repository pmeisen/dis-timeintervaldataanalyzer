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
		MARKED_ID=1, DESC_VALUE=2, NULL_VALUE=3, POS_START_INCL=4, POS_END_INCL=5, 
		POS_START_EXCL=6, POS_END_EXCL=7, STMT_SELECT=8, STMT_INSERT=9, TYPE_TIMESERIES=10, 
		TYPE_RECORDS=11, OP_FROM=12, OP_OF=13, OP_IN=14, OP_INTO=15, OP_VALUES=16, 
		OP_ALIAS=17, OP_GROUPBY=18, OP_FILTERBY=19, LOGICAL_OR=20, LOGICAL_AND=21, 
		LOGICAL_NOT=22, LOGICAL_IGNORE=23, MATH_MULTIPLY=24, MATH_DIVISION=25, 
		MATH_PLUS=26, MATH_MINUS=27, AGGR_COUNT=28, AGGR_SUM=29, AGGR_MIN=30, 
		AGGR_MAX=31, AGGR_AVERAGE=32, AGGR_MODE=33, AGGR_MEAN=34, AGGR_MEDIAN=35, 
		CMP_EQUAL=36, BRACKET_ROUND_OPENED=37, BRACKET_ROUND_CLOSED=38, BRACKET_SQUARE_OPENED=39, 
		BRACKET_SQUARE_CLOSED=40, BRACKET_CURLY_OPENED=41, BRACKET_CURLY_CLOSED=42, 
		SEPARATOR=43, DATE=44, INT=45, SIMPLE_ID=46, ENHANCED_ID=47, WHITESPACE=48;
	public static final String[] tokenNames = {
		"<INVALID>", "MARKED_ID", "DESC_VALUE", "NULL_VALUE", "POS_START_INCL", 
		"POS_END_INCL", "POS_START_EXCL", "POS_END_EXCL", "STMT_SELECT", "STMT_INSERT", 
		"TYPE_TIMESERIES", "TYPE_RECORDS", "OP_FROM", "OP_OF", "OP_IN", "OP_INTO", 
		"OP_VALUES", "OP_ALIAS", "OP_GROUPBY", "OP_FILTERBY", "LOGICAL_OR", "LOGICAL_AND", 
		"LOGICAL_NOT", "LOGICAL_IGNORE", "'*'", "'/'", "'+'", "'-'", "AGGR_COUNT", 
		"AGGR_SUM", "AGGR_MIN", "AGGR_MAX", "AGGR_AVERAGE", "AGGR_MODE", "AGGR_MEAN", 
		"AGGR_MEDIAN", "'='", "'('", "')'", "'['", "']'", "'{'", "'}'", "','", 
		"DATE", "INT", "SIMPLE_ID", "ENHANCED_ID", "WHITESPACE"
	};
	public static final int
		RULE_root = 0, RULE_exprInsert = 1, RULE_exprStructure = 2, RULE_exprValues = 3, 
		RULE_exprSelect = 4, RULE_exprMeasure = 5, RULE_exprInterval = 6, RULE_exprComp = 7, 
		RULE_exprGroup = 8, RULE_exprAggregate = 9, RULE_compNamedMeasure = 10, 
		RULE_compMeasure = 11, RULE_compMeasureAtom = 12, RULE_compDescriptorEqual = 13, 
		RULE_compDescValueTupel = 14, RULE_compGroupIgnore = 15, RULE_compAggrFunction = 16, 
		RULE_compDescriptorFormula = 17, RULE_compDescriptorFormulaAtom = 18, 
		RULE_selectorModelId = 19, RULE_selectorDescriptorId = 20, RULE_selectorAlias = 21, 
		RULE_selectorSelectType = 22, RULE_selectorDateInterval = 23, RULE_selectorIntInterval = 24, 
		RULE_selectorDateIntervalWithNull = 25, RULE_selectorIntIntervalWithNull = 26, 
		RULE_selectorOpenInterval = 27, RULE_selectorCloseInterval = 28, RULE_selectorDescValue = 29, 
		RULE_selectorAggrFunctionName = 30, RULE_selectorFirstMathOperator = 31, 
		RULE_selectorSecondMathOperator = 32, RULE_selectorIntervalDef = 33;
	public static final String[] ruleNames = {
		"root", "exprInsert", "exprStructure", "exprValues", "exprSelect", "exprMeasure", 
		"exprInterval", "exprComp", "exprGroup", "exprAggregate", "compNamedMeasure", 
		"compMeasure", "compMeasureAtom", "compDescriptorEqual", "compDescValueTupel", 
		"compGroupIgnore", "compAggrFunction", "compDescriptorFormula", "compDescriptorFormulaAtom", 
		"selectorModelId", "selectorDescriptorId", "selectorAlias", "selectorSelectType", 
		"selectorDateInterval", "selectorIntInterval", "selectorDateIntervalWithNull", 
		"selectorIntIntervalWithNull", "selectorOpenInterval", "selectorCloseInterval", 
		"selectorDescValue", "selectorAggrFunctionName", "selectorFirstMathOperator", 
		"selectorSecondMathOperator", "selectorIntervalDef"
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
		public TerminalNode EOF() { return getToken(QueryGrammarParser.EOF, 0); }
		public ExprSelectContext exprSelect() {
			return getRuleContext(ExprSelectContext.class,0);
		}
		public ExprInsertContext exprInsert() {
			return getRuleContext(ExprInsertContext.class,0);
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
			setState(70);
			switch (_input.LA(1)) {
			case STMT_INSERT:
				{
				setState(68); exprInsert();
				}
				break;
			case STMT_SELECT:
				{
				setState(69); exprSelect();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(72); match(EOF);
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 2, RULE_exprInsert);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(74); match(STMT_INSERT);
			setState(75); match(OP_INTO);
			setState(76); selectorModelId();
			setState(77); exprStructure();
			setState(78); match(OP_VALUES);
			setState(79); exprValues();
			setState(84);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(80); match(SEPARATOR);
				setState(81); exprValues();
				}
				}
				setState(86);
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
		public SelectorDescriptorIdContext selectorDescriptorId(int i) {
			return getRuleContext(SelectorDescriptorIdContext.class,i);
		}
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public SelectorIntervalDefContext selectorIntervalDef() {
			return getRuleContext(SelectorIntervalDefContext.class,0);
		}
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public List<SelectorDescriptorIdContext> selectorDescriptorId() {
			return getRuleContexts(SelectorDescriptorIdContext.class);
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
		enterRule(_localctx, 4, RULE_exprStructure);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(87); match(BRACKET_ROUND_OPENED);
			setState(88); selectorIntervalDef();
			setState(89); match(SEPARATOR);
			setState(90); selectorDescriptorId();
			setState(95);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(91); match(SEPARATOR);
				setState(92); selectorDescriptorId();
				}
				}
				setState(97);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(98); match(BRACKET_ROUND_CLOSED);
			}
		}
		catch (RecognitionException re) {
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
		public SelectorDateIntervalWithNullContext selectorDateIntervalWithNull() {
			return getRuleContext(SelectorDateIntervalWithNullContext.class,0);
		}
		public List<SelectorDescValueContext> selectorDescValue() {
			return getRuleContexts(SelectorDescValueContext.class);
		}
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public SelectorIntIntervalWithNullContext selectorIntIntervalWithNull() {
			return getRuleContext(SelectorIntIntervalWithNullContext.class,0);
		}
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public SelectorDescValueContext selectorDescValue(int i) {
			return getRuleContext(SelectorDescValueContext.class,i);
		}
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
		enterRule(_localctx, 6, RULE_exprValues);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(100); match(BRACKET_ROUND_OPENED);
			setState(103);
			switch ( getInterpreter().adaptivePredict(_input,3,_ctx) ) {
			case 1:
				{
				setState(101); selectorDateIntervalWithNull();
				}
				break;

			case 2:
				{
				setState(102); selectorIntIntervalWithNull();
				}
				break;
			}
			setState(105); match(SEPARATOR);
			setState(106); selectorDescValue();
			setState(111);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(107); match(SEPARATOR);
				setState(108); selectorDescValue();
				}
				}
				setState(113);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(114); match(BRACKET_ROUND_CLOSED);
			}
		}
		catch (RecognitionException re) {
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
		public TerminalNode STMT_SELECT() { return getToken(QueryGrammarParser.STMT_SELECT, 0); }
		public TerminalNode OP_FILTERBY() { return getToken(QueryGrammarParser.OP_FILTERBY, 0); }
		public TerminalNode OP_OF() { return getToken(QueryGrammarParser.OP_OF, 0); }
		public TerminalNode OP_IN() { return getToken(QueryGrammarParser.OP_IN, 0); }
		public SelectorSelectTypeContext selectorSelectType() {
			return getRuleContext(SelectorSelectTypeContext.class,0);
		}
		public TerminalNode OP_GROUPBY() { return getToken(QueryGrammarParser.OP_GROUPBY, 0); }
		public ExprMeasureContext exprMeasure() {
			return getRuleContext(ExprMeasureContext.class,0);
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
		enterRule(_localctx, 8, RULE_exprSelect);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(116); match(STMT_SELECT);
			setState(117); selectorSelectType();
			setState(120);
			_la = _input.LA(1);
			if (_la==OP_OF) {
				{
				setState(118); match(OP_OF);
				setState(119); exprMeasure();
				}
			}

			setState(122); match(OP_FROM);
			setState(123); selectorModelId();
			setState(126);
			_la = _input.LA(1);
			if (_la==OP_IN) {
				{
				setState(124); match(OP_IN);
				setState(125); exprInterval();
				}
			}

			setState(130);
			_la = _input.LA(1);
			if (_la==OP_FILTERBY) {
				{
				setState(128); match(OP_FILTERBY);
				setState(129); exprComp(0);
				}
			}

			setState(134);
			_la = _input.LA(1);
			if (_la==OP_GROUPBY) {
				{
				setState(132); match(OP_GROUPBY);
				setState(133); exprGroup();
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
		enterRule(_localctx, 10, RULE_exprMeasure);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(136); compNamedMeasure();
			setState(141);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(137); match(SEPARATOR);
				setState(138); compNamedMeasure();
				}
				}
				setState(143);
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
		enterRule(_localctx, 12, RULE_exprInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(144); selectorOpenInterval();
			setState(147);
			switch (_input.LA(1)) {
			case DATE:
				{
				setState(145); selectorDateInterval();
				}
				break;
			case INT:
				{
				setState(146); selectorIntInterval();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(149); selectorCloseInterval();
			}
		}
		catch (RecognitionException re) {
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
		int _startState = 14;
		enterRecursionRule(_localctx, RULE_exprComp);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(159);
			switch (_input.LA(1)) {
			case LOGICAL_NOT:
				{
				setState(152); match(LOGICAL_NOT);
				setState(153); exprComp(2);
				}
				break;
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				{
				setState(154); compDescriptorEqual();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(155); match(BRACKET_ROUND_OPENED);
				setState(156); exprComp(0);
				setState(157); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(166);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,12,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new ExprCompContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_exprComp);
					setState(161);
					if (!(1 >= _localctx._p)) throw new FailedPredicateException(this, "1 >= $_p");
					setState(162);
					_la = _input.LA(1);
					if ( !(_la==LOGICAL_OR || _la==LOGICAL_AND) ) {
					_errHandler.recoverInline(this);
					}
					consume();
					setState(163); exprComp(2);
					}
					} 
				}
				setState(168);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,12,_ctx);
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
		enterRule(_localctx, 16, RULE_exprGroup);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(169); exprAggregate();
			setState(172);
			_la = _input.LA(1);
			if (_la==LOGICAL_IGNORE) {
				{
				setState(170); match(LOGICAL_IGNORE);
				setState(171); compGroupIgnore();
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
		enterRule(_localctx, 18, RULE_exprAggregate);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(174); selectorDescriptorId();
			setState(179);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(175); match(SEPARATOR);
				setState(176); selectorDescriptorId();
				}
				}
				setState(181);
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
		enterRule(_localctx, 20, RULE_compNamedMeasure);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(182); compMeasure(0);
			setState(185);
			_la = _input.LA(1);
			if (_la==OP_ALIAS) {
				{
				setState(183); match(OP_ALIAS);
				setState(184); selectorAlias();
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
		int _startState = 22;
		enterRecursionRule(_localctx, RULE_compMeasure);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(188); compMeasureAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(196);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,16,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompMeasureContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compMeasure);
					setState(190);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(191); selectorSecondMathOperator();
					setState(192); compMeasureAtom(0);
					}
					} 
				}
				setState(198);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,16,_ctx);
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
		int _startState = 24;
		enterRecursionRule(_localctx, RULE_compMeasureAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(205);
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
				setState(200); compAggrFunction();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(201); match(BRACKET_ROUND_OPENED);
				setState(202); compMeasure(0);
				setState(203); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(213);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,18,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompMeasureAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compMeasureAtom);
					setState(207);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(208); selectorFirstMathOperator();
					setState(209); compMeasureAtom(0);
					}
					} 
				}
				setState(215);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,18,_ctx);
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
		enterRule(_localctx, 26, RULE_compDescriptorEqual);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(216); selectorDescriptorId();
			setState(217); match(CMP_EQUAL);
			setState(218); selectorDescValue();
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 28, RULE_compDescValueTupel);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(220); match(BRACKET_ROUND_OPENED);
			setState(221); selectorDescValue();
			setState(226);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(222); match(SEPARATOR);
				setState(223); selectorDescValue();
				}
				}
				setState(228);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(229); match(BRACKET_ROUND_CLOSED);
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 30, RULE_compGroupIgnore);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(231); match(BRACKET_CURLY_OPENED);
			setState(232); compDescValueTupel();
			setState(237);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(233); match(SEPARATOR);
				setState(234); compDescValueTupel();
				}
				}
				setState(239);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(240); match(BRACKET_CURLY_CLOSED);
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 32, RULE_compAggrFunction);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(242); selectorAggrFunctionName();
			setState(243); match(BRACKET_ROUND_OPENED);
			setState(244); compDescriptorFormula(0);
			setState(245); match(BRACKET_ROUND_CLOSED);
			}
		}
		catch (RecognitionException re) {
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
		int _startState = 34;
		enterRecursionRule(_localctx, RULE_compDescriptorFormula);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			{
			setState(248); compDescriptorFormulaAtom(0);
			}
			_ctx.stop = _input.LT(-1);
			setState(256);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,21,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDescriptorFormulaContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDescriptorFormula);
					setState(250);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(251); selectorSecondMathOperator();
					setState(252); compDescriptorFormulaAtom(0);
					}
					} 
				}
				setState(258);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,21,_ctx);
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
		int _startState = 36;
		enterRecursionRule(_localctx, RULE_compDescriptorFormulaAtom);
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(265);
			switch (_input.LA(1)) {
			case MARKED_ID:
			case SIMPLE_ID:
			case ENHANCED_ID:
				{
				setState(260); selectorDescriptorId();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(261); match(BRACKET_ROUND_OPENED);
				setState(262); compDescriptorFormula(0);
				setState(263); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(273);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,23,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new CompDescriptorFormulaAtomContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_compDescriptorFormulaAtom);
					setState(267);
					if (!(2 >= _localctx._p)) throw new FailedPredicateException(this, "2 >= $_p");
					setState(268); selectorFirstMathOperator();
					setState(269); compDescriptorFormulaAtom(0);
					}
					} 
				}
				setState(275);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,23,_ctx);
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
		enterRule(_localctx, 38, RULE_selectorModelId);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(276);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << MARKED_ID) | (1L << SIMPLE_ID) | (1L << ENHANCED_ID))) != 0)) ) {
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
		enterRule(_localctx, 40, RULE_selectorDescriptorId);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(278);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << MARKED_ID) | (1L << SIMPLE_ID) | (1L << ENHANCED_ID))) != 0)) ) {
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
		enterRule(_localctx, 42, RULE_selectorAlias);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(280);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << MARKED_ID) | (1L << SIMPLE_ID) | (1L << ENHANCED_ID))) != 0)) ) {
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

	public static class SelectorSelectTypeContext extends ParserRuleContext {
		public TerminalNode TYPE_TIMESERIES() { return getToken(QueryGrammarParser.TYPE_TIMESERIES, 0); }
		public TerminalNode TYPE_RECORDS() { return getToken(QueryGrammarParser.TYPE_RECORDS, 0); }
		public SelectorSelectTypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorSelectType; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorSelectType(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorSelectType(this);
		}
	}

	public final SelectorSelectTypeContext selectorSelectType() throws RecognitionException {
		SelectorSelectTypeContext _localctx = new SelectorSelectTypeContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_selectorSelectType);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(282);
			_la = _input.LA(1);
			if ( !(_la==TYPE_TIMESERIES || _la==TYPE_RECORDS) ) {
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
		enterRule(_localctx, 46, RULE_selectorDateInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(284); match(DATE);
			setState(285); match(SEPARATOR);
			setState(286); match(DATE);
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 48, RULE_selectorIntInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(288); match(INT);
			setState(289); match(SEPARATOR);
			setState(290); match(INT);
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 50, RULE_selectorDateIntervalWithNull);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(292);
			_la = _input.LA(1);
			if ( !(_la==NULL_VALUE || _la==DATE) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(293); match(SEPARATOR);
			setState(294);
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
		enterRule(_localctx, 52, RULE_selectorIntIntervalWithNull);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(296);
			_la = _input.LA(1);
			if ( !(_la==NULL_VALUE || _la==INT) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(297); match(SEPARATOR);
			setState(298);
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
		enterRule(_localctx, 54, RULE_selectorOpenInterval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(300);
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
		enterRule(_localctx, 56, RULE_selectorCloseInterval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(302);
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
		public TerminalNode DESC_VALUE() { return getToken(QueryGrammarParser.DESC_VALUE, 0); }
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
		enterRule(_localctx, 58, RULE_selectorDescValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(304);
			_la = _input.LA(1);
			if ( !(_la==DESC_VALUE || _la==NULL_VALUE) ) {
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
		enterRule(_localctx, 60, RULE_selectorAggrFunctionName);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(306);
			_la = _input.LA(1);
			if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << AGGR_COUNT) | (1L << AGGR_SUM) | (1L << AGGR_MIN) | (1L << AGGR_MAX) | (1L << AGGR_AVERAGE) | (1L << AGGR_MODE) | (1L << AGGR_MEAN) | (1L << AGGR_MEDIAN) | (1L << SIMPLE_ID))) != 0)) ) {
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
		enterRule(_localctx, 62, RULE_selectorFirstMathOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(308);
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
		enterRule(_localctx, 64, RULE_selectorSecondMathOperator);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(310);
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
		public TerminalNode SEPARATOR() { return getToken(QueryGrammarParser.SEPARATOR, 0); }
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
		enterRule(_localctx, 66, RULE_selectorIntervalDef);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(312);
			_la = _input.LA(1);
			if ( !(_la==POS_START_INCL || _la==POS_START_EXCL) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(313); match(SEPARATOR);
			setState(314);
			_la = _input.LA(1);
			if ( !(_la==POS_END_INCL || _la==POS_END_EXCL) ) {
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

	public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
		switch (ruleIndex) {
		case 7: return exprComp_sempred((ExprCompContext)_localctx, predIndex);

		case 11: return compMeasure_sempred((CompMeasureContext)_localctx, predIndex);

		case 12: return compMeasureAtom_sempred((CompMeasureAtomContext)_localctx, predIndex);

		case 17: return compDescriptorFormula_sempred((CompDescriptorFormulaContext)_localctx, predIndex);

		case 18: return compDescriptorFormulaAtom_sempred((CompDescriptorFormulaAtomContext)_localctx, predIndex);
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
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\3\62\u013f\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\3\2\3\2\5\2I\n\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3"+
		"\3\3\7\3U\n\3\f\3\16\3X\13\3\3\4\3\4\3\4\3\4\3\4\3\4\7\4`\n\4\f\4\16\4"+
		"c\13\4\3\4\3\4\3\5\3\5\3\5\5\5j\n\5\3\5\3\5\3\5\3\5\7\5p\n\5\f\5\16\5"+
		"s\13\5\3\5\3\5\3\6\3\6\3\6\3\6\5\6{\n\6\3\6\3\6\3\6\3\6\5\6\u0081\n\6"+
		"\3\6\3\6\5\6\u0085\n\6\3\6\3\6\5\6\u0089\n\6\3\7\3\7\3\7\7\7\u008e\n\7"+
		"\f\7\16\7\u0091\13\7\3\b\3\b\3\b\5\b\u0096\n\b\3\b\3\b\3\t\3\t\3\t\3\t"+
		"\3\t\3\t\3\t\3\t\5\t\u00a2\n\t\3\t\3\t\3\t\7\t\u00a7\n\t\f\t\16\t\u00aa"+
		"\13\t\3\n\3\n\3\n\5\n\u00af\n\n\3\13\3\13\3\13\7\13\u00b4\n\13\f\13\16"+
		"\13\u00b7\13\13\3\f\3\f\3\f\5\f\u00bc\n\f\3\r\3\r\3\r\3\r\3\r\3\r\3\r"+
		"\7\r\u00c5\n\r\f\r\16\r\u00c8\13\r\3\16\3\16\3\16\3\16\3\16\3\16\5\16"+
		"\u00d0\n\16\3\16\3\16\3\16\3\16\7\16\u00d6\n\16\f\16\16\16\u00d9\13\16"+
		"\3\17\3\17\3\17\3\17\3\20\3\20\3\20\3\20\7\20\u00e3\n\20\f\20\16\20\u00e6"+
		"\13\20\3\20\3\20\3\21\3\21\3\21\3\21\7\21\u00ee\n\21\f\21\16\21\u00f1"+
		"\13\21\3\21\3\21\3\22\3\22\3\22\3\22\3\22\3\23\3\23\3\23\3\23\3\23\3\23"+
		"\3\23\7\23\u0101\n\23\f\23\16\23\u0104\13\23\3\24\3\24\3\24\3\24\3\24"+
		"\3\24\5\24\u010c\n\24\3\24\3\24\3\24\3\24\7\24\u0112\n\24\f\24\16\24\u0115"+
		"\13\24\3\25\3\25\3\26\3\26\3\27\3\27\3\30\3\30\3\31\3\31\3\31\3\31\3\32"+
		"\3\32\3\32\3\32\3\33\3\33\3\33\3\33\3\34\3\34\3\34\3\34\3\35\3\35\3\36"+
		"\3\36\3\37\3\37\3 \3 \3!\3!\3\"\3\"\3#\3#\3#\3#\3#\2$\2\4\6\b\n\f\16\20"+
		"\22\24\26\30\32\34\36 \"$&(*,.\60\62\64\668:<>@BD\2\17\3\2\26\27\4\2\3"+
		"\3\60\61\3\2\f\r\4\2\5\5..\4\2\5\5//\4\2\'\'))\4\2((**\3\2\4\5\4\2\36"+
		"%\60\60\3\2\32\33\3\2\34\35\4\2\6\6\b\b\4\2\7\7\t\t\u0135\2H\3\2\2\2\4"+
		"L\3\2\2\2\6Y\3\2\2\2\bf\3\2\2\2\nv\3\2\2\2\f\u008a\3\2\2\2\16\u0092\3"+
		"\2\2\2\20\u00a1\3\2\2\2\22\u00ab\3\2\2\2\24\u00b0\3\2\2\2\26\u00b8\3\2"+
		"\2\2\30\u00bd\3\2\2\2\32\u00cf\3\2\2\2\34\u00da\3\2\2\2\36\u00de\3\2\2"+
		"\2 \u00e9\3\2\2\2\"\u00f4\3\2\2\2$\u00f9\3\2\2\2&\u010b\3\2\2\2(\u0116"+
		"\3\2\2\2*\u0118\3\2\2\2,\u011a\3\2\2\2.\u011c\3\2\2\2\60\u011e\3\2\2\2"+
		"\62\u0122\3\2\2\2\64\u0126\3\2\2\2\66\u012a\3\2\2\28\u012e\3\2\2\2:\u0130"+
		"\3\2\2\2<\u0132\3\2\2\2>\u0134\3\2\2\2@\u0136\3\2\2\2B\u0138\3\2\2\2D"+
		"\u013a\3\2\2\2FI\5\4\3\2GI\5\n\6\2HF\3\2\2\2HG\3\2\2\2IJ\3\2\2\2JK\7\2"+
		"\2\3K\3\3\2\2\2LM\7\13\2\2MN\7\21\2\2NO\5(\25\2OP\5\6\4\2PQ\7\22\2\2Q"+
		"V\5\b\5\2RS\7-\2\2SU\5\b\5\2TR\3\2\2\2UX\3\2\2\2VT\3\2\2\2VW\3\2\2\2W"+
		"\5\3\2\2\2XV\3\2\2\2YZ\7\'\2\2Z[\5D#\2[\\\7-\2\2\\a\5*\26\2]^\7-\2\2^"+
		"`\5*\26\2_]\3\2\2\2`c\3\2\2\2a_\3\2\2\2ab\3\2\2\2bd\3\2\2\2ca\3\2\2\2"+
		"de\7(\2\2e\7\3\2\2\2fi\7\'\2\2gj\5\64\33\2hj\5\66\34\2ig\3\2\2\2ih\3\2"+
		"\2\2jk\3\2\2\2kl\7-\2\2lq\5<\37\2mn\7-\2\2np\5<\37\2om\3\2\2\2ps\3\2\2"+
		"\2qo\3\2\2\2qr\3\2\2\2rt\3\2\2\2sq\3\2\2\2tu\7(\2\2u\t\3\2\2\2vw\7\n\2"+
		"\2wz\5.\30\2xy\7\17\2\2y{\5\f\7\2zx\3\2\2\2z{\3\2\2\2{|\3\2\2\2|}\7\16"+
		"\2\2}\u0080\5(\25\2~\177\7\20\2\2\177\u0081\5\16\b\2\u0080~\3\2\2\2\u0080"+
		"\u0081\3\2\2\2\u0081\u0084\3\2\2\2\u0082\u0083\7\25\2\2\u0083\u0085\5"+
		"\20\t\2\u0084\u0082\3\2\2\2\u0084\u0085\3\2\2\2\u0085\u0088\3\2\2\2\u0086"+
		"\u0087\7\24\2\2\u0087\u0089\5\22\n\2\u0088\u0086\3\2\2\2\u0088\u0089\3"+
		"\2\2\2\u0089\13\3\2\2\2\u008a\u008f\5\26\f\2\u008b\u008c\7-\2\2\u008c"+
		"\u008e\5\26\f\2\u008d\u008b\3\2\2\2\u008e\u0091\3\2\2\2\u008f\u008d\3"+
		"\2\2\2\u008f\u0090\3\2\2\2\u0090\r\3\2\2\2\u0091\u008f\3\2\2\2\u0092\u0095"+
		"\58\35\2\u0093\u0096\5\60\31\2\u0094\u0096\5\62\32\2\u0095\u0093\3\2\2"+
		"\2\u0095\u0094\3\2\2\2\u0096\u0097\3\2\2\2\u0097\u0098\5:\36\2\u0098\17"+
		"\3\2\2\2\u0099\u009a\b\t\1\2\u009a\u009b\7\30\2\2\u009b\u00a2\5\20\t\2"+
		"\u009c\u00a2\5\34\17\2\u009d\u009e\7\'\2\2\u009e\u009f\5\20\t\2\u009f"+
		"\u00a0\7(\2\2\u00a0\u00a2\3\2\2\2\u00a1\u0099\3\2\2\2\u00a1\u009c\3\2"+
		"\2\2\u00a1\u009d\3\2\2\2\u00a2\u00a8\3\2\2\2\u00a3\u00a4\6\t\2\3\u00a4"+
		"\u00a5\t\2\2\2\u00a5\u00a7\5\20\t\2\u00a6\u00a3\3\2\2\2\u00a7\u00aa\3"+
		"\2\2\2\u00a8\u00a6\3\2\2\2\u00a8\u00a9\3\2\2\2\u00a9\21\3\2\2\2\u00aa"+
		"\u00a8\3\2\2\2\u00ab\u00ae\5\24\13\2\u00ac\u00ad\7\31\2\2\u00ad\u00af"+
		"\5 \21\2\u00ae\u00ac\3\2\2\2\u00ae\u00af\3\2\2\2\u00af\23\3\2\2\2\u00b0"+
		"\u00b5\5*\26\2\u00b1\u00b2\7-\2\2\u00b2\u00b4\5*\26\2\u00b3\u00b1\3\2"+
		"\2\2\u00b4\u00b7\3\2\2\2\u00b5\u00b3\3\2\2\2\u00b5\u00b6\3\2\2\2\u00b6"+
		"\25\3\2\2\2\u00b7\u00b5\3\2\2\2\u00b8\u00bb\5\30\r\2\u00b9\u00ba\7\23"+
		"\2\2\u00ba\u00bc\5,\27\2\u00bb\u00b9\3\2\2\2\u00bb\u00bc\3\2\2\2\u00bc"+
		"\27\3\2\2\2\u00bd\u00be\b\r\1\2\u00be\u00bf\5\32\16\2\u00bf\u00c6\3\2"+
		"\2\2\u00c0\u00c1\6\r\3\3\u00c1\u00c2\5B\"\2\u00c2\u00c3\5\32\16\2\u00c3"+
		"\u00c5\3\2\2\2\u00c4\u00c0\3\2\2\2\u00c5\u00c8\3\2\2\2\u00c6\u00c4\3\2"+
		"\2\2\u00c6\u00c7\3\2\2\2\u00c7\31\3\2\2\2\u00c8\u00c6\3\2\2\2\u00c9\u00ca"+
		"\b\16\1\2\u00ca\u00d0\5\"\22\2\u00cb\u00cc\7\'\2\2\u00cc\u00cd\5\30\r"+
		"\2\u00cd\u00ce\7(\2\2\u00ce\u00d0\3\2\2\2\u00cf\u00c9\3\2\2\2\u00cf\u00cb"+
		"\3\2\2\2\u00d0\u00d7\3\2\2\2\u00d1\u00d2\6\16\4\3\u00d2\u00d3\5@!\2\u00d3"+
		"\u00d4\5\32\16\2\u00d4\u00d6\3\2\2\2\u00d5\u00d1\3\2\2\2\u00d6\u00d9\3"+
		"\2\2\2\u00d7\u00d5\3\2\2\2\u00d7\u00d8\3\2\2\2\u00d8\33\3\2\2\2\u00d9"+
		"\u00d7\3\2\2\2\u00da\u00db\5*\26\2\u00db\u00dc\7&\2\2\u00dc\u00dd\5<\37"+
		"\2\u00dd\35\3\2\2\2\u00de\u00df\7\'\2\2\u00df\u00e4\5<\37\2\u00e0\u00e1"+
		"\7-\2\2\u00e1\u00e3\5<\37\2\u00e2\u00e0\3\2\2\2\u00e3\u00e6\3\2\2\2\u00e4"+
		"\u00e2\3\2\2\2\u00e4\u00e5\3\2\2\2\u00e5\u00e7\3\2\2\2\u00e6\u00e4\3\2"+
		"\2\2\u00e7\u00e8\7(\2\2\u00e8\37\3\2\2\2\u00e9\u00ea\7+\2\2\u00ea\u00ef"+
		"\5\36\20\2\u00eb\u00ec\7-\2\2\u00ec\u00ee\5\36\20\2\u00ed\u00eb\3\2\2"+
		"\2\u00ee\u00f1\3\2\2\2\u00ef\u00ed\3\2\2\2\u00ef\u00f0\3\2\2\2\u00f0\u00f2"+
		"\3\2\2\2\u00f1\u00ef\3\2\2\2\u00f2\u00f3\7,\2\2\u00f3!\3\2\2\2\u00f4\u00f5"+
		"\5> \2\u00f5\u00f6\7\'\2\2\u00f6\u00f7\5$\23\2\u00f7\u00f8\7(\2\2\u00f8"+
		"#\3\2\2\2\u00f9\u00fa\b\23\1\2\u00fa\u00fb\5&\24\2\u00fb\u0102\3\2\2\2"+
		"\u00fc\u00fd\6\23\5\3\u00fd\u00fe\5B\"\2\u00fe\u00ff\5&\24\2\u00ff\u0101"+
		"\3\2\2\2\u0100\u00fc\3\2\2\2\u0101\u0104\3\2\2\2\u0102\u0100\3\2\2\2\u0102"+
		"\u0103\3\2\2\2\u0103%\3\2\2\2\u0104\u0102\3\2\2\2\u0105\u0106\b\24\1\2"+
		"\u0106\u010c\5*\26\2\u0107\u0108\7\'\2\2\u0108\u0109\5$\23\2\u0109\u010a"+
		"\7(\2\2\u010a\u010c\3\2\2\2\u010b\u0105\3\2\2\2\u010b\u0107\3\2\2\2\u010c"+
		"\u0113\3\2\2\2\u010d\u010e\6\24\6\3\u010e\u010f\5@!\2\u010f\u0110\5&\24"+
		"\2\u0110\u0112\3\2\2\2\u0111\u010d\3\2\2\2\u0112\u0115\3\2\2\2\u0113\u0111"+
		"\3\2\2\2\u0113\u0114\3\2\2\2\u0114\'\3\2\2\2\u0115\u0113\3\2\2\2\u0116"+
		"\u0117\t\3\2\2\u0117)\3\2\2\2\u0118\u0119\t\3\2\2\u0119+\3\2\2\2\u011a"+
		"\u011b\t\3\2\2\u011b-\3\2\2\2\u011c\u011d\t\4\2\2\u011d/\3\2\2\2\u011e"+
		"\u011f\7.\2\2\u011f\u0120\7-\2\2\u0120\u0121\7.\2\2\u0121\61\3\2\2\2\u0122"+
		"\u0123\7/\2\2\u0123\u0124\7-\2\2\u0124\u0125\7/\2\2\u0125\63\3\2\2\2\u0126"+
		"\u0127\t\5\2\2\u0127\u0128\7-\2\2\u0128\u0129\t\5\2\2\u0129\65\3\2\2\2"+
		"\u012a\u012b\t\6\2\2\u012b\u012c\7-\2\2\u012c\u012d\t\6\2\2\u012d\67\3"+
		"\2\2\2\u012e\u012f\t\7\2\2\u012f9\3\2\2\2\u0130\u0131\t\b\2\2\u0131;\3"+
		"\2\2\2\u0132\u0133\t\t\2\2\u0133=\3\2\2\2\u0134\u0135\t\n\2\2\u0135?\3"+
		"\2\2\2\u0136\u0137\t\13\2\2\u0137A\3\2\2\2\u0138\u0139\t\f\2\2\u0139C"+
		"\3\2\2\2\u013a\u013b\t\r\2\2\u013b\u013c\7-\2\2\u013c\u013d\t\16\2\2\u013d"+
		"E\3\2\2\2\32HVaiqz\u0080\u0084\u0088\u008f\u0095\u00a1\u00a8\u00ae\u00b5"+
		"\u00bb\u00c6\u00cf\u00d7\u00e4\u00ef\u0102\u010b\u0113";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}