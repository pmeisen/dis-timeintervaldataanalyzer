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
		STMT_SELECT=1, TYPE_TIMESERIES=2, TYPE_RECORDS=3, NULL_VALUE=4, DESC_VALUE=5, 
		OP_FROM=6, OP_IN=7, OP_GROUPBY=8, OP_FILTERBY=9, LOGICAL_OR=10, LOGICAL_AND=11, 
		LOGICAL_NOT=12, LOGICAL_IGNORE=13, CMP_EQUAL=14, BRACKET_ROUND_OPENED=15, 
		BRACKET_ROUND_CLOSED=16, BRACKET_SQUARE_OPENED=17, BRACKET_SQUARE_CLOSED=18, 
		BRACKET_CURLY_OPENED=19, BRACKET_CURLY_CLOSED=20, SEPARATOR=21, DATE=22, 
		INT=23, IDENTIFIER=24, WHITESPACE=25;
	public static final String[] tokenNames = {
		"<INVALID>", "STMT_SELECT", "TYPE_TIMESERIES", "TYPE_RECORDS", "NULL_VALUE", 
		"DESC_VALUE", "OP_FROM", "OP_IN", "OP_GROUPBY", "OP_FILTERBY", "LOGICAL_OR", 
		"LOGICAL_AND", "LOGICAL_NOT", "LOGICAL_IGNORE", "'='", "'('", "')'", "'['", 
		"']'", "'{'", "'}'", "','", "DATE", "INT", "IDENTIFIER", "WHITESPACE"
	};
	public static final int
		RULE_exprSelect = 0, RULE_exprGroup = 1, RULE_exprAggregate = 2, RULE_exprLogic = 3, 
		RULE_exprComp = 4, RULE_exprInterval = 5, RULE_compDescriptorEqual = 6, 
		RULE_compGroupIgnore = 7, RULE_selectorModelId = 8, RULE_selectorSelectType = 9, 
		RULE_selectorDateInterval = 10, RULE_selectorIntInterval = 11, RULE_selectorOpenInterval = 12, 
		RULE_selectorCloseInterval = 13, RULE_selectorDescValueTupel = 14, RULE_selectorDescValue = 15;
	public static final String[] ruleNames = {
		"exprSelect", "exprGroup", "exprAggregate", "exprLogic", "exprComp", "exprInterval", 
		"compDescriptorEqual", "compGroupIgnore", "selectorModelId", "selectorSelectType", 
		"selectorDateInterval", "selectorIntInterval", "selectorOpenInterval", 
		"selectorCloseInterval", "selectorDescValueTupel", "selectorDescValue"
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
	public static class ExprSelectContext extends ParserRuleContext {
		public TerminalNode EOF() { return getToken(QueryGrammarParser.EOF, 0); }
		public SelectorModelIdContext selectorModelId() {
			return getRuleContext(SelectorModelIdContext.class,0);
		}
		public ExprLogicContext exprLogic() {
			return getRuleContext(ExprLogicContext.class,0);
		}
		public ExprGroupContext exprGroup() {
			return getRuleContext(ExprGroupContext.class,0);
		}
		public TerminalNode OP_FROM() { return getToken(QueryGrammarParser.OP_FROM, 0); }
		public ExprIntervalContext exprInterval() {
			return getRuleContext(ExprIntervalContext.class,0);
		}
		public TerminalNode STMT_SELECT() { return getToken(QueryGrammarParser.STMT_SELECT, 0); }
		public TerminalNode OP_FILTERBY() { return getToken(QueryGrammarParser.OP_FILTERBY, 0); }
		public TerminalNode OP_IN() { return getToken(QueryGrammarParser.OP_IN, 0); }
		public SelectorSelectTypeContext selectorSelectType() {
			return getRuleContext(SelectorSelectTypeContext.class,0);
		}
		public TerminalNode OP_GROUPBY() { return getToken(QueryGrammarParser.OP_GROUPBY, 0); }
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
		enterRule(_localctx, 0, RULE_exprSelect);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(32); match(STMT_SELECT);
			setState(33); selectorSelectType();
			setState(34); match(OP_FROM);
			setState(35); selectorModelId();
			setState(38);
			_la = _input.LA(1);
			if (_la==OP_IN) {
				{
				setState(36); match(OP_IN);
				setState(37); exprInterval();
				}
			}

			setState(42);
			_la = _input.LA(1);
			if (_la==OP_FILTERBY) {
				{
				setState(40); match(OP_FILTERBY);
				setState(41); exprLogic();
				}
			}

			setState(46);
			_la = _input.LA(1);
			if (_la==OP_GROUPBY) {
				{
				setState(44); match(OP_GROUPBY);
				setState(45); exprGroup();
				}
			}

			setState(48); match(EOF);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
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
		enterRule(_localctx, 2, RULE_exprGroup);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(50); exprAggregate();
			setState(53);
			_la = _input.LA(1);
			if (_la==LOGICAL_IGNORE) {
				{
				setState(51); match(LOGICAL_IGNORE);
				setState(52); compGroupIgnore();
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
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public TerminalNode IDENTIFIER(int i) {
			return getToken(QueryGrammarParser.IDENTIFIER, i);
		}
		public List<TerminalNode> IDENTIFIER() { return getTokens(QueryGrammarParser.IDENTIFIER); }
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
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
		enterRule(_localctx, 4, RULE_exprAggregate);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(55); match(IDENTIFIER);
			setState(60);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(56); match(SEPARATOR);
				setState(57); match(IDENTIFIER);
				}
				}
				setState(62);
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

	public static class ExprLogicContext extends ParserRuleContext {
		public ExprCompContext exprComp(int i) {
			return getRuleContext(ExprCompContext.class,i);
		}
		public List<ExprCompContext> exprComp() {
			return getRuleContexts(ExprCompContext.class);
		}
		public ExprLogicContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_exprLogic; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterExprLogic(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitExprLogic(this);
		}
	}

	public final ExprLogicContext exprLogic() throws RecognitionException {
		ExprLogicContext _localctx = new ExprLogicContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_exprLogic);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(64); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(63); exprComp(0);
				}
				}
				setState(66); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( (((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << LOGICAL_NOT) | (1L << BRACKET_ROUND_OPENED) | (1L << IDENTIFIER))) != 0) );
			}
		}
		catch (RecognitionException re) {
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
		int _startState = 8;
		enterRecursionRule(_localctx, RULE_exprComp);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(76);
			switch (_input.LA(1)) {
			case LOGICAL_NOT:
				{
				setState(69); match(LOGICAL_NOT);
				setState(70); exprComp(2);
				}
				break;
			case IDENTIFIER:
				{
				setState(71); compDescriptorEqual();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(72); match(BRACKET_ROUND_OPENED);
				setState(73); exprComp(0);
				setState(74); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(83);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,7,_ctx);
			while ( _alt!=2 && _alt!=-1 ) {
				if ( _alt==1 ) {
					if ( _parseListeners!=null ) triggerExitRuleEvent();
					_prevctx = _localctx;
					{
					{
					_localctx = new ExprCompContext(_parentctx, _parentState, _p);
					pushNewRecursionContext(_localctx, _startState, RULE_exprComp);
					setState(78);
					if (!(1 >= _localctx._p)) throw new FailedPredicateException(this, "1 >= $_p");
					setState(79);
					_la = _input.LA(1);
					if ( !(_la==LOGICAL_OR || _la==LOGICAL_AND) ) {
					_errHandler.recoverInline(this);
					}
					consume();
					setState(80); exprComp(2);
					}
					} 
				}
				setState(85);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,7,_ctx);
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
		enterRule(_localctx, 10, RULE_exprInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(86); selectorOpenInterval();
			setState(89);
			switch (_input.LA(1)) {
			case DATE:
				{
				setState(87); selectorDateInterval();
				}
				break;
			case INT:
				{
				setState(88); selectorIntInterval();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(91); selectorCloseInterval();
			}
		}
		catch (RecognitionException re) {
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
		public SelectorDescValueContext selectorDescValue() {
			return getRuleContext(SelectorDescValueContext.class,0);
		}
		public TerminalNode IDENTIFIER() { return getToken(QueryGrammarParser.IDENTIFIER, 0); }
		public TerminalNode CMP_EQUAL() { return getToken(QueryGrammarParser.CMP_EQUAL, 0); }
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
		enterRule(_localctx, 12, RULE_compDescriptorEqual);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(93); match(IDENTIFIER);
			setState(94); match(CMP_EQUAL);
			setState(95); selectorDescValue();
			}
		}
		catch (RecognitionException re) {
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
		public List<SelectorDescValueTupelContext> selectorDescValueTupel() {
			return getRuleContexts(SelectorDescValueTupelContext.class);
		}
		public TerminalNode BRACKET_CURLY_OPENED() { return getToken(QueryGrammarParser.BRACKET_CURLY_OPENED, 0); }
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public TerminalNode BRACKET_CURLY_CLOSED() { return getToken(QueryGrammarParser.BRACKET_CURLY_CLOSED, 0); }
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public SelectorDescValueTupelContext selectorDescValueTupel(int i) {
			return getRuleContext(SelectorDescValueTupelContext.class,i);
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
		enterRule(_localctx, 14, RULE_compGroupIgnore);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(97); match(BRACKET_CURLY_OPENED);
			setState(98); selectorDescValueTupel();
			setState(103);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(99); match(SEPARATOR);
				setState(100); selectorDescValueTupel();
				}
				}
				setState(105);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(106); match(BRACKET_CURLY_CLOSED);
			}
		}
		catch (RecognitionException re) {
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
		public TerminalNode IDENTIFIER() { return getToken(QueryGrammarParser.IDENTIFIER, 0); }
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
		enterRule(_localctx, 16, RULE_selectorModelId);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(108); match(IDENTIFIER);
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 18, RULE_selectorSelectType);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(110);
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
		enterRule(_localctx, 20, RULE_selectorDateInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(112); match(DATE);
			setState(113); match(SEPARATOR);
			setState(114); match(DATE);
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 22, RULE_selectorIntInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(116); match(INT);
			setState(117); match(SEPARATOR);
			setState(118); match(INT);
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 24, RULE_selectorOpenInterval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(120);
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
		enterRule(_localctx, 26, RULE_selectorCloseInterval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(122);
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

	public static class SelectorDescValueTupelContext extends ParserRuleContext {
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
		public SelectorDescValueTupelContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selectorDescValueTupel; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).enterSelectorDescValueTupel(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof QueryGrammarListener ) ((QueryGrammarListener)listener).exitSelectorDescValueTupel(this);
		}
	}

	public final SelectorDescValueTupelContext selectorDescValueTupel() throws RecognitionException {
		SelectorDescValueTupelContext _localctx = new SelectorDescValueTupelContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_selectorDescValueTupel);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(124); match(BRACKET_ROUND_OPENED);
			setState(125); selectorDescValue();
			setState(130);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(126); match(SEPARATOR);
				setState(127); selectorDescValue();
				}
				}
				setState(132);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(133); match(BRACKET_ROUND_CLOSED);
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 30, RULE_selectorDescValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(135);
			_la = _input.LA(1);
			if ( !(_la==NULL_VALUE || _la==DESC_VALUE) ) {
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
		case 4: return exprComp_sempred((ExprCompContext)_localctx, predIndex);
		}
		return true;
	}
	private boolean exprComp_sempred(ExprCompContext _localctx, int predIndex) {
		switch (predIndex) {
		case 0: return 1 >= _localctx._p;
		}
		return true;
	}

	public static final String _serializedATN =
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\3\33\u008c\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\3\2\3\2"+
		"\3\2\3\2\3\2\3\2\5\2)\n\2\3\2\3\2\5\2-\n\2\3\2\3\2\5\2\61\n\2\3\2\3\2"+
		"\3\3\3\3\3\3\5\38\n\3\3\4\3\4\3\4\7\4=\n\4\f\4\16\4@\13\4\3\5\6\5C\n\5"+
		"\r\5\16\5D\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\5\6O\n\6\3\6\3\6\3\6\7\6T\n"+
		"\6\f\6\16\6W\13\6\3\7\3\7\3\7\5\7\\\n\7\3\7\3\7\3\b\3\b\3\b\3\b\3\t\3"+
		"\t\3\t\3\t\7\th\n\t\f\t\16\tk\13\t\3\t\3\t\3\n\3\n\3\13\3\13\3\f\3\f\3"+
		"\f\3\f\3\r\3\r\3\r\3\r\3\16\3\16\3\17\3\17\3\20\3\20\3\20\3\20\7\20\u0083"+
		"\n\20\f\20\16\20\u0086\13\20\3\20\3\20\3\21\3\21\3\21\2\22\2\4\6\b\n\f"+
		"\16\20\22\24\26\30\32\34\36 \2\7\3\2\f\r\3\2\4\5\4\2\21\21\23\23\4\2\22"+
		"\22\24\24\3\2\6\7\u0087\2\"\3\2\2\2\4\64\3\2\2\2\69\3\2\2\2\bB\3\2\2\2"+
		"\nN\3\2\2\2\fX\3\2\2\2\16_\3\2\2\2\20c\3\2\2\2\22n\3\2\2\2\24p\3\2\2\2"+
		"\26r\3\2\2\2\30v\3\2\2\2\32z\3\2\2\2\34|\3\2\2\2\36~\3\2\2\2 \u0089\3"+
		"\2\2\2\"#\7\3\2\2#$\5\24\13\2$%\7\b\2\2%(\5\22\n\2&\'\7\t\2\2\')\5\f\7"+
		"\2(&\3\2\2\2()\3\2\2\2),\3\2\2\2*+\7\13\2\2+-\5\b\5\2,*\3\2\2\2,-\3\2"+
		"\2\2-\60\3\2\2\2./\7\n\2\2/\61\5\4\3\2\60.\3\2\2\2\60\61\3\2\2\2\61\62"+
		"\3\2\2\2\62\63\7\2\2\3\63\3\3\2\2\2\64\67\5\6\4\2\65\66\7\17\2\2\668\5"+
		"\20\t\2\67\65\3\2\2\2\678\3\2\2\28\5\3\2\2\29>\7\32\2\2:;\7\27\2\2;=\7"+
		"\32\2\2<:\3\2\2\2=@\3\2\2\2><\3\2\2\2>?\3\2\2\2?\7\3\2\2\2@>\3\2\2\2A"+
		"C\5\n\6\2BA\3\2\2\2CD\3\2\2\2DB\3\2\2\2DE\3\2\2\2E\t\3\2\2\2FG\b\6\1\2"+
		"GH\7\16\2\2HO\5\n\6\2IO\5\16\b\2JK\7\21\2\2KL\5\n\6\2LM\7\22\2\2MO\3\2"+
		"\2\2NF\3\2\2\2NI\3\2\2\2NJ\3\2\2\2OU\3\2\2\2PQ\6\6\2\3QR\t\2\2\2RT\5\n"+
		"\6\2SP\3\2\2\2TW\3\2\2\2US\3\2\2\2UV\3\2\2\2V\13\3\2\2\2WU\3\2\2\2X[\5"+
		"\32\16\2Y\\\5\26\f\2Z\\\5\30\r\2[Y\3\2\2\2[Z\3\2\2\2\\]\3\2\2\2]^\5\34"+
		"\17\2^\r\3\2\2\2_`\7\32\2\2`a\7\20\2\2ab\5 \21\2b\17\3\2\2\2cd\7\25\2"+
		"\2di\5\36\20\2ef\7\27\2\2fh\5\36\20\2ge\3\2\2\2hk\3\2\2\2ig\3\2\2\2ij"+
		"\3\2\2\2jl\3\2\2\2ki\3\2\2\2lm\7\26\2\2m\21\3\2\2\2no\7\32\2\2o\23\3\2"+
		"\2\2pq\t\3\2\2q\25\3\2\2\2rs\7\30\2\2st\7\27\2\2tu\7\30\2\2u\27\3\2\2"+
		"\2vw\7\31\2\2wx\7\27\2\2xy\7\31\2\2y\31\3\2\2\2z{\t\4\2\2{\33\3\2\2\2"+
		"|}\t\5\2\2}\35\3\2\2\2~\177\7\21\2\2\177\u0084\5 \21\2\u0080\u0081\7\27"+
		"\2\2\u0081\u0083\5 \21\2\u0082\u0080\3\2\2\2\u0083\u0086\3\2\2\2\u0084"+
		"\u0082\3\2\2\2\u0084\u0085\3\2\2\2\u0085\u0087\3\2\2\2\u0086\u0084\3\2"+
		"\2\2\u0087\u0088\7\22\2\2\u0088\37\3\2\2\2\u0089\u008a\t\6\2\2\u008a!"+
		"\3\2\2\2\r(,\60\67>DNU[i\u0084";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}