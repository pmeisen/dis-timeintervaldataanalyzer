// Generated from Y:\dis-timeintervaldataanalyzer\src\net\meisen\dissertation\parser\query\generated\QueryGrammar.g4 by ANTLR 4.1

package net.meisen.dissertation.parser.query.generated;

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
		STMT_SELECT=1, TYPE_TIMELINES=2, TYPE_RECORDS=3, OP_IN=4, OP_GROUPBY=5, 
		OP_FILTERBY=6, LOGICAL_OR=7, LOGICAL_AND=8, INTERVAL_START=9, INTERVAL_END=10, 
		CMP_LARGEREQUAL=11, CMP_SMALLEREQUAL=12, CMP_EQUAL=13, CMP_SMALLER=14, 
		CMP_LARGER=15, BRACKET_ROUND_OPENED=16, BRACKET_ROUND_CLOSED=17, BRACKET_SQUARE_OPENED=18, 
		BRACKET_SQUARE_CLOSED=19, SEPARATOR=20, IDENTIFIER=21, DATE=22, INT=23, 
		DESC_VALUE=24, DESC_NAME=25, WHITESPACE=26;
	public static final String[] tokenNames = {
		"<INVALID>", "STMT_SELECT", "TYPE_TIMELINES", "TYPE_RECORDS", "OP_IN", 
		"OP_GROUPBY", "OP_FILTERBY", "LOGICAL_OR", "LOGICAL_AND", "INTERVAL_START", 
		"INTERVAL_END", "'>='", "'<='", "'='", "'<'", "'>'", "'('", "')'", "'['", 
		"']'", "','", "IDENTIFIER", "DATE", "INT", "DESC_VALUE", "DESC_NAME", 
		"WHITESPACE"
	};
	public static final int
		RULE_select = 0, RULE_logic = 1, RULE_expression = 2, RULE_interval = 3, 
		RULE_selector_interval = 4, RULE_selector_descriptor = 5;
	public static final String[] ruleNames = {
		"select", "logic", "expression", "interval", "selector_interval", "selector_descriptor"
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
	public static class SelectContext extends ParserRuleContext {
		public TerminalNode TYPE_TIMELINES() { return getToken(QueryGrammarParser.TYPE_TIMELINES, 0); }
		public TerminalNode SEPARATOR(int i) {
			return getToken(QueryGrammarParser.SEPARATOR, i);
		}
		public TerminalNode STMT_SELECT() { return getToken(QueryGrammarParser.STMT_SELECT, 0); }
		public TerminalNode OP_FILTERBY() { return getToken(QueryGrammarParser.OP_FILTERBY, 0); }
		public LogicContext logic() {
			return getRuleContext(LogicContext.class,0);
		}
		public TerminalNode OP_IN() { return getToken(QueryGrammarParser.OP_IN, 0); }
		public List<Selector_descriptorContext> selector_descriptor() {
			return getRuleContexts(Selector_descriptorContext.class);
		}
		public List<TerminalNode> SEPARATOR() { return getTokens(QueryGrammarParser.SEPARATOR); }
		public TerminalNode OP_GROUPBY() { return getToken(QueryGrammarParser.OP_GROUPBY, 0); }
		public TerminalNode TYPE_RECORDS() { return getToken(QueryGrammarParser.TYPE_RECORDS, 0); }
		public IntervalContext interval() {
			return getRuleContext(IntervalContext.class,0);
		}
		public Selector_descriptorContext selector_descriptor(int i) {
			return getRuleContext(Selector_descriptorContext.class,i);
		}
		public SelectContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_select; }
	}

	public final SelectContext select() throws RecognitionException {
		SelectContext _localctx = new SelectContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_select);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(12); match(STMT_SELECT);
			setState(13);
			_la = _input.LA(1);
			if ( !(_la==TYPE_TIMELINES || _la==TYPE_RECORDS) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(14); match(OP_IN);
			setState(15); interval();
			setState(18);
			_la = _input.LA(1);
			if (_la==OP_FILTERBY) {
				{
				setState(16); match(OP_FILTERBY);
				setState(17); logic();
				}
			}

			setState(29);
			_la = _input.LA(1);
			if (_la==OP_GROUPBY) {
				{
				setState(20); match(OP_GROUPBY);
				setState(21); selector_descriptor();
				setState(26);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==SEPARATOR) {
					{
					{
					setState(22); match(SEPARATOR);
					setState(23); selector_descriptor();
					}
					}
					setState(28);
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

	public static class LogicContext extends ParserRuleContext {
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public TerminalNode LOGICAL_OR(int i) {
			return getToken(QueryGrammarParser.LOGICAL_OR, i);
		}
		public List<TerminalNode> LOGICAL_AND() { return getTokens(QueryGrammarParser.LOGICAL_AND); }
		public LogicContext logic(int i) {
			return getRuleContext(LogicContext.class,i);
		}
		public List<LogicContext> logic() {
			return getRuleContexts(LogicContext.class);
		}
		public TerminalNode LOGICAL_AND(int i) {
			return getToken(QueryGrammarParser.LOGICAL_AND, i);
		}
		public ExpressionContext expression() {
			return getRuleContext(ExpressionContext.class,0);
		}
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public List<TerminalNode> LOGICAL_OR() { return getTokens(QueryGrammarParser.LOGICAL_OR); }
		public LogicContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_logic; }
	}

	public final LogicContext logic() throws RecognitionException {
		LogicContext _localctx = new LogicContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_logic);
		int _la;
		try {
			int _alt;
			setState(43);
			switch (_input.LA(1)) {
			case INTERVAL_START:
			case INTERVAL_END:
			case IDENTIFIER:
			case DESC_NAME:
				enterOuterAlt(_localctx, 1);
				{
				setState(31); expression();
				setState(36);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
				while ( _alt!=2 && _alt!=-1 ) {
					if ( _alt==1 ) {
						{
						{
						setState(32);
						_la = _input.LA(1);
						if ( !(_la==LOGICAL_OR || _la==LOGICAL_AND) ) {
						_errHandler.recoverInline(this);
						}
						consume();
						setState(33); logic();
						}
						} 
					}
					setState(38);
					_errHandler.sync(this);
					_alt = getInterpreter().adaptivePredict(_input,3,_ctx);
				}
				}
				break;
			case BRACKET_ROUND_OPENED:
				enterOuterAlt(_localctx, 2);
				{
				setState(39); match(BRACKET_ROUND_OPENED);
				setState(40); logic();
				setState(41); match(BRACKET_ROUND_CLOSED);
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

	public static class ExpressionContext extends ParserRuleContext {
		public TerminalNode DATE() { return getToken(QueryGrammarParser.DATE, 0); }
		public TerminalNode DESC_VALUE() { return getToken(QueryGrammarParser.DESC_VALUE, 0); }
		public TerminalNode INT() { return getToken(QueryGrammarParser.INT, 0); }
		public Selector_intervalContext selector_interval() {
			return getRuleContext(Selector_intervalContext.class,0);
		}
		public TerminalNode CMP_LARGER() { return getToken(QueryGrammarParser.CMP_LARGER, 0); }
		public Selector_descriptorContext selector_descriptor() {
			return getRuleContext(Selector_descriptorContext.class,0);
		}
		public TerminalNode CMP_EQUAL() { return getToken(QueryGrammarParser.CMP_EQUAL, 0); }
		public TerminalNode CMP_LARGEREQUAL() { return getToken(QueryGrammarParser.CMP_LARGEREQUAL, 0); }
		public TerminalNode CMP_SMALLER() { return getToken(QueryGrammarParser.CMP_SMALLER, 0); }
		public TerminalNode CMP_SMALLEREQUAL() { return getToken(QueryGrammarParser.CMP_SMALLEREQUAL, 0); }
		public ExpressionContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expression; }
	}

	public final ExpressionContext expression() throws RecognitionException {
		ExpressionContext _localctx = new ExpressionContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_expression);
		int _la;
		try {
			setState(53);
			switch (_input.LA(1)) {
			case IDENTIFIER:
			case DESC_NAME:
				enterOuterAlt(_localctx, 1);
				{
				setState(45); selector_descriptor();
				setState(46); match(CMP_EQUAL);
				setState(47); match(DESC_VALUE);
				}
				break;
			case INTERVAL_START:
			case INTERVAL_END:
				enterOuterAlt(_localctx, 2);
				{
				setState(49); selector_interval();
				setState(50);
				_la = _input.LA(1);
				if ( !((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << CMP_LARGEREQUAL) | (1L << CMP_SMALLEREQUAL) | (1L << CMP_EQUAL) | (1L << CMP_SMALLER) | (1L << CMP_LARGER))) != 0)) ) {
				_errHandler.recoverInline(this);
				}
				consume();
				setState(51);
				_la = _input.LA(1);
				if ( !(_la==DATE || _la==INT) ) {
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

	public static class IntervalContext extends ParserRuleContext {
		public TerminalNode BRACKET_ROUND_CLOSED() { return getToken(QueryGrammarParser.BRACKET_ROUND_CLOSED, 0); }
		public List<TerminalNode> DATE() { return getTokens(QueryGrammarParser.DATE); }
		public TerminalNode BRACKET_SQUARE_CLOSED() { return getToken(QueryGrammarParser.BRACKET_SQUARE_CLOSED, 0); }
		public TerminalNode INT() { return getToken(QueryGrammarParser.INT, 0); }
		public TerminalNode SEPARATOR() { return getToken(QueryGrammarParser.SEPARATOR, 0); }
		public TerminalNode BRACKET_SQUARE_OPENED() { return getToken(QueryGrammarParser.BRACKET_SQUARE_OPENED, 0); }
		public TerminalNode BRACKET_ROUND_OPENED() { return getToken(QueryGrammarParser.BRACKET_ROUND_OPENED, 0); }
		public TerminalNode DATE(int i) {
			return getToken(QueryGrammarParser.DATE, i);
		}
		public IntervalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_interval; }
	}

	public final IntervalContext interval() throws RecognitionException {
		IntervalContext _localctx = new IntervalContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_interval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(55);
			_la = _input.LA(1);
			if ( !(_la==BRACKET_ROUND_OPENED || _la==BRACKET_SQUARE_OPENED) ) {
			_errHandler.recoverInline(this);
			}
			consume();
			setState(62);
			switch (_input.LA(1)) {
			case INT:
				{
				setState(56); match(INT);
				setState(57); match(SEPARATOR);
				setState(58); match(INT);
				}
				break;
			case DATE:
				{
				setState(59); match(DATE);
				setState(60); match(SEPARATOR);
				setState(61); match(DATE);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(64);
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

	public static class Selector_intervalContext extends ParserRuleContext {
		public TerminalNode INTERVAL_END() { return getToken(QueryGrammarParser.INTERVAL_END, 0); }
		public TerminalNode INTERVAL_START() { return getToken(QueryGrammarParser.INTERVAL_START, 0); }
		public Selector_intervalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selector_interval; }
	}

	public final Selector_intervalContext selector_interval() throws RecognitionException {
		Selector_intervalContext _localctx = new Selector_intervalContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_selector_interval);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(66);
			_la = _input.LA(1);
			if ( !(_la==INTERVAL_START || _la==INTERVAL_END) ) {
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

	public static class Selector_descriptorContext extends ParserRuleContext {
		public TerminalNode DESC_NAME() { return getToken(QueryGrammarParser.DESC_NAME, 0); }
		public TerminalNode IDENTIFIER() { return getToken(QueryGrammarParser.IDENTIFIER, 0); }
		public Selector_descriptorContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_selector_descriptor; }
	}

	public final Selector_descriptorContext selector_descriptor() throws RecognitionException {
		Selector_descriptorContext _localctx = new Selector_descriptorContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_selector_descriptor);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(68);
			_la = _input.LA(1);
			if ( !(_la==IDENTIFIER || _la==DESC_NAME) ) {
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

	public static final String _serializedATN =
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\3\34I\4\2\t\2\4\3\t"+
		"\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\3\2\3\2\3\2\3\2\3\2\3\2\5\2\25\n\2"+
		"\3\2\3\2\3\2\3\2\7\2\33\n\2\f\2\16\2\36\13\2\5\2 \n\2\3\3\3\3\3\3\7\3"+
		"%\n\3\f\3\16\3(\13\3\3\3\3\3\3\3\3\3\5\3.\n\3\3\4\3\4\3\4\3\4\3\4\3\4"+
		"\3\4\3\4\5\48\n\4\3\5\3\5\3\5\3\5\3\5\3\5\3\5\5\5A\n\5\3\5\3\5\3\6\3\6"+
		"\3\7\3\7\3\7\2\b\2\4\6\b\n\f\2\n\3\2\4\5\3\2\t\n\3\2\r\21\3\2\30\31\4"+
		"\2\22\22\24\24\4\2\23\23\25\25\3\2\13\f\4\2\27\27\33\33I\2\16\3\2\2\2"+
		"\4-\3\2\2\2\6\67\3\2\2\2\b9\3\2\2\2\nD\3\2\2\2\fF\3\2\2\2\16\17\7\3\2"+
		"\2\17\20\t\2\2\2\20\21\7\6\2\2\21\24\5\b\5\2\22\23\7\b\2\2\23\25\5\4\3"+
		"\2\24\22\3\2\2\2\24\25\3\2\2\2\25\37\3\2\2\2\26\27\7\7\2\2\27\34\5\f\7"+
		"\2\30\31\7\26\2\2\31\33\5\f\7\2\32\30\3\2\2\2\33\36\3\2\2\2\34\32\3\2"+
		"\2\2\34\35\3\2\2\2\35 \3\2\2\2\36\34\3\2\2\2\37\26\3\2\2\2\37 \3\2\2\2"+
		" \3\3\2\2\2!&\5\6\4\2\"#\t\3\2\2#%\5\4\3\2$\"\3\2\2\2%(\3\2\2\2&$\3\2"+
		"\2\2&\'\3\2\2\2\'.\3\2\2\2(&\3\2\2\2)*\7\22\2\2*+\5\4\3\2+,\7\23\2\2,"+
		".\3\2\2\2-!\3\2\2\2-)\3\2\2\2.\5\3\2\2\2/\60\5\f\7\2\60\61\7\17\2\2\61"+
		"\62\7\32\2\2\628\3\2\2\2\63\64\5\n\6\2\64\65\t\4\2\2\65\66\t\5\2\2\66"+
		"8\3\2\2\2\67/\3\2\2\2\67\63\3\2\2\28\7\3\2\2\29@\t\6\2\2:;\7\31\2\2;<"+
		"\7\26\2\2<A\7\31\2\2=>\7\30\2\2>?\7\26\2\2?A\7\30\2\2@:\3\2\2\2@=\3\2"+
		"\2\2AB\3\2\2\2BC\t\7\2\2C\t\3\2\2\2DE\t\b\2\2E\13\3\2\2\2FG\t\t\2\2G\r"+
		"\3\2\2\2\t\24\34\37&-\67@";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}