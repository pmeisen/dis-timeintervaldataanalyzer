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
		MARKED_ID=1, DESC_VALUE=2, NULL_VALUE=3, STMT_SELECT=4, TYPE_TIMESERIES=5, 
		TYPE_RECORDS=6, OP_FROM=7, OP_IN=8, OP_GROUPBY=9, OP_FILTERBY=10, LOGICAL_OR=11, 
		LOGICAL_AND=12, LOGICAL_NOT=13, LOGICAL_IGNORE=14, CMP_EQUAL=15, BRACKET_ROUND_OPENED=16, 
		BRACKET_ROUND_CLOSED=17, BRACKET_SQUARE_OPENED=18, BRACKET_SQUARE_CLOSED=19, 
		BRACKET_CURLY_OPENED=20, BRACKET_CURLY_CLOSED=21, SEPARATOR=22, DATE=23, 
		INT=24, ID=25, WHITESPACE=26;
	public static final String[] tokenNames = {
		"<INVALID>", "MARKED_ID", "DESC_VALUE", "NULL_VALUE", "STMT_SELECT", "TYPE_TIMESERIES", 
		"TYPE_RECORDS", "OP_FROM", "OP_IN", "OP_GROUPBY", "OP_FILTERBY", "LOGICAL_OR", 
		"LOGICAL_AND", "LOGICAL_NOT", "LOGICAL_IGNORE", "'='", "'('", "')'", "'['", 
		"']'", "'{'", "'}'", "','", "DATE", "INT", "ID", "WHITESPACE"
	};
	public static final int
		RULE_exprSelect = 0, RULE_exprGroup = 1, RULE_exprAggregate = 2, RULE_exprLogic = 3, 
		RULE_exprComp = 4, RULE_exprInterval = 5, RULE_compDescriptorEqual = 6, 
		RULE_compGroupIgnore = 7, RULE_selectorModelId = 8, RULE_selectorDescriptorId = 9, 
		RULE_selectorSelectType = 10, RULE_selectorDateInterval = 11, RULE_selectorIntInterval = 12, 
		RULE_selectorOpenInterval = 13, RULE_selectorCloseInterval = 14, RULE_selectorDescValueTupel = 15, 
		RULE_selectorDescValue = 16;
	public static final String[] ruleNames = {
		"exprSelect", "exprGroup", "exprAggregate", "exprLogic", "exprComp", "exprInterval", 
		"compDescriptorEqual", "compGroupIgnore", "selectorModelId", "selectorDescriptorId", 
		"selectorSelectType", "selectorDateInterval", "selectorIntInterval", "selectorOpenInterval", 
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
			setState(34); match(STMT_SELECT);
			setState(35); selectorSelectType();
			setState(36); match(OP_FROM);
			setState(37); selectorModelId();
			setState(40);
			_la = _input.LA(1);
			if (_la==OP_IN) {
				{
				setState(38); match(OP_IN);
				setState(39); exprInterval();
				}
			}

			setState(44);
			_la = _input.LA(1);
			if (_la==OP_FILTERBY) {
				{
				setState(42); match(OP_FILTERBY);
				setState(43); exprLogic();
				}
			}

			setState(48);
			_la = _input.LA(1);
			if (_la==OP_GROUPBY) {
				{
				setState(46); match(OP_GROUPBY);
				setState(47); exprGroup();
				}
			}

			setState(50); match(EOF);
			}
		}
		catch (RecognitionException re) {
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
			setState(52); exprAggregate();
			setState(55);
			_la = _input.LA(1);
			if (_la==LOGICAL_IGNORE) {
				{
				setState(53); match(LOGICAL_IGNORE);
				setState(54); compGroupIgnore();
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
		enterRule(_localctx, 4, RULE_exprAggregate);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(57); selectorDescriptorId();
			setState(62);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(58); match(SEPARATOR);
				setState(59); selectorDescriptorId();
				}
				}
				setState(64);
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
			setState(66); 
			_errHandler.sync(this);
			_la = _input.LA(1);
			do {
				{
				{
				setState(65); exprComp(0);
				}
				}
				setState(68); 
				_errHandler.sync(this);
				_la = _input.LA(1);
			} while ( (((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << MARKED_ID) | (1L << LOGICAL_NOT) | (1L << BRACKET_ROUND_OPENED) | (1L << ID))) != 0) );
			}
		}
		catch (RecognitionException re) {
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
			setState(78);
			switch (_input.LA(1)) {
			case LOGICAL_NOT:
				{
				setState(71); match(LOGICAL_NOT);
				setState(72); exprComp(2);
				}
				break;
			case MARKED_ID:
			case ID:
				{
				setState(73); compDescriptorEqual();
				}
				break;
			case BRACKET_ROUND_OPENED:
				{
				setState(74); match(BRACKET_ROUND_OPENED);
				setState(75); exprComp(0);
				setState(76); match(BRACKET_ROUND_CLOSED);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			_ctx.stop = _input.LT(-1);
			setState(85);
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
					setState(80);
					if (!(1 >= _localctx._p)) throw new FailedPredicateException(this, "1 >= $_p");
					setState(81);
					_la = _input.LA(1);
					if ( !(_la==LOGICAL_OR || _la==LOGICAL_AND) ) {
					_errHandler.recoverInline(this);
					}
					consume();
					setState(82); exprComp(2);
					}
					} 
				}
				setState(87);
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
			setState(88); selectorOpenInterval();
			setState(91);
			switch (_input.LA(1)) {
			case DATE:
				{
				setState(89); selectorDateInterval();
				}
				break;
			case INT:
				{
				setState(90); selectorIntInterval();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(93); selectorCloseInterval();
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 12, RULE_compDescriptorEqual);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(95); selectorDescriptorId();
			setState(96); match(CMP_EQUAL);
			setState(97); selectorDescValue();
			}
		}
		catch (RecognitionException re) {
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
			setState(99); match(BRACKET_CURLY_OPENED);
			setState(100); selectorDescValueTupel();
			setState(105);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(101); match(SEPARATOR);
				setState(102); selectorDescValueTupel();
				}
				}
				setState(107);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(108); match(BRACKET_CURLY_CLOSED);
			}
		}
		catch (RecognitionException re) {
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
		public TerminalNode ID() { return getToken(QueryGrammarParser.ID, 0); }
		public TerminalNode MARKED_ID() { return getToken(QueryGrammarParser.MARKED_ID, 0); }
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
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(110);
			_la = _input.LA(1);
			if ( !(_la==MARKED_ID || _la==ID) ) {
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
		public TerminalNode ID() { return getToken(QueryGrammarParser.ID, 0); }
		public TerminalNode MARKED_ID() { return getToken(QueryGrammarParser.MARKED_ID, 0); }
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
		enterRule(_localctx, 18, RULE_selectorDescriptorId);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(112);
			_la = _input.LA(1);
			if ( !(_la==MARKED_ID || _la==ID) ) {
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
		enterRule(_localctx, 20, RULE_selectorSelectType);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(114);
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
		enterRule(_localctx, 22, RULE_selectorDateInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(116); match(DATE);
			setState(117); match(SEPARATOR);
			setState(118); match(DATE);
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 24, RULE_selectorIntInterval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(120); match(INT);
			setState(121); match(SEPARATOR);
			setState(122); match(INT);
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 26, RULE_selectorOpenInterval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(124);
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
		enterRule(_localctx, 28, RULE_selectorCloseInterval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(126);
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
		enterRule(_localctx, 30, RULE_selectorDescValueTupel);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(128); match(BRACKET_ROUND_OPENED);
			setState(129); selectorDescValue();
			setState(134);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==SEPARATOR) {
				{
				{
				setState(130); match(SEPARATOR);
				setState(131); selectorDescValue();
				}
				}
				setState(136);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(137); match(BRACKET_ROUND_CLOSED);
			}
		}
		catch (RecognitionException re) {
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
		enterRule(_localctx, 32, RULE_selectorDescValue);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(139);
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
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\3\34\u0090\4\2\t\2"+
		"\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\3\2\3\2\3\2\3\2\3\2\3\2\5\2+\n\2\3\2\3\2\5\2/\n\2\3\2\3\2\5\2\63\n\2"+
		"\3\2\3\2\3\3\3\3\3\3\5\3:\n\3\3\4\3\4\3\4\7\4?\n\4\f\4\16\4B\13\4\3\5"+
		"\6\5E\n\5\r\5\16\5F\3\6\3\6\3\6\3\6\3\6\3\6\3\6\3\6\5\6Q\n\6\3\6\3\6\3"+
		"\6\7\6V\n\6\f\6\16\6Y\13\6\3\7\3\7\3\7\5\7^\n\7\3\7\3\7\3\b\3\b\3\b\3"+
		"\b\3\t\3\t\3\t\3\t\7\tj\n\t\f\t\16\tm\13\t\3\t\3\t\3\n\3\n\3\13\3\13\3"+
		"\f\3\f\3\r\3\r\3\r\3\r\3\16\3\16\3\16\3\16\3\17\3\17\3\20\3\20\3\21\3"+
		"\21\3\21\3\21\7\21\u0087\n\21\f\21\16\21\u008a\13\21\3\21\3\21\3\22\3"+
		"\22\3\22\2\23\2\4\6\b\n\f\16\20\22\24\26\30\32\34\36 \"\2\b\3\2\r\16\4"+
		"\2\3\3\33\33\3\2\7\b\4\2\22\22\24\24\4\2\23\23\25\25\3\2\4\5\u008a\2$"+
		"\3\2\2\2\4\66\3\2\2\2\6;\3\2\2\2\bD\3\2\2\2\nP\3\2\2\2\fZ\3\2\2\2\16a"+
		"\3\2\2\2\20e\3\2\2\2\22p\3\2\2\2\24r\3\2\2\2\26t\3\2\2\2\30v\3\2\2\2\32"+
		"z\3\2\2\2\34~\3\2\2\2\36\u0080\3\2\2\2 \u0082\3\2\2\2\"\u008d\3\2\2\2"+
		"$%\7\6\2\2%&\5\26\f\2&\'\7\t\2\2\'*\5\22\n\2()\7\n\2\2)+\5\f\7\2*(\3\2"+
		"\2\2*+\3\2\2\2+.\3\2\2\2,-\7\f\2\2-/\5\b\5\2.,\3\2\2\2./\3\2\2\2/\62\3"+
		"\2\2\2\60\61\7\13\2\2\61\63\5\4\3\2\62\60\3\2\2\2\62\63\3\2\2\2\63\64"+
		"\3\2\2\2\64\65\7\2\2\3\65\3\3\2\2\2\669\5\6\4\2\678\7\20\2\28:\5\20\t"+
		"\29\67\3\2\2\29:\3\2\2\2:\5\3\2\2\2;@\5\24\13\2<=\7\30\2\2=?\5\24\13\2"+
		"><\3\2\2\2?B\3\2\2\2@>\3\2\2\2@A\3\2\2\2A\7\3\2\2\2B@\3\2\2\2CE\5\n\6"+
		"\2DC\3\2\2\2EF\3\2\2\2FD\3\2\2\2FG\3\2\2\2G\t\3\2\2\2HI\b\6\1\2IJ\7\17"+
		"\2\2JQ\5\n\6\2KQ\5\16\b\2LM\7\22\2\2MN\5\n\6\2NO\7\23\2\2OQ\3\2\2\2PH"+
		"\3\2\2\2PK\3\2\2\2PL\3\2\2\2QW\3\2\2\2RS\6\6\2\3ST\t\2\2\2TV\5\n\6\2U"+
		"R\3\2\2\2VY\3\2\2\2WU\3\2\2\2WX\3\2\2\2X\13\3\2\2\2YW\3\2\2\2Z]\5\34\17"+
		"\2[^\5\30\r\2\\^\5\32\16\2][\3\2\2\2]\\\3\2\2\2^_\3\2\2\2_`\5\36\20\2"+
		"`\r\3\2\2\2ab\5\24\13\2bc\7\21\2\2cd\5\"\22\2d\17\3\2\2\2ef\7\26\2\2f"+
		"k\5 \21\2gh\7\30\2\2hj\5 \21\2ig\3\2\2\2jm\3\2\2\2ki\3\2\2\2kl\3\2\2\2"+
		"ln\3\2\2\2mk\3\2\2\2no\7\27\2\2o\21\3\2\2\2pq\t\3\2\2q\23\3\2\2\2rs\t"+
		"\3\2\2s\25\3\2\2\2tu\t\4\2\2u\27\3\2\2\2vw\7\31\2\2wx\7\30\2\2xy\7\31"+
		"\2\2y\31\3\2\2\2z{\7\32\2\2{|\7\30\2\2|}\7\32\2\2}\33\3\2\2\2~\177\t\5"+
		"\2\2\177\35\3\2\2\2\u0080\u0081\t\6\2\2\u0081\37\3\2\2\2\u0082\u0083\7"+
		"\22\2\2\u0083\u0088\5\"\22\2\u0084\u0085\7\30\2\2\u0085\u0087\5\"\22\2"+
		"\u0086\u0084\3\2\2\2\u0087\u008a\3\2\2\2\u0088\u0086\3\2\2\2\u0088\u0089"+
		"\3\2\2\2\u0089\u008b\3\2\2\2\u008a\u0088\3\2\2\2\u008b\u008c\7\23\2\2"+
		"\u008c!\3\2\2\2\u008d\u008e\t\7\2\2\u008e#\3\2\2\2\r*.\629@FPW]k\u0088";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}