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
		STMT_SELECT=1, TYPE=2, OPERATOR=3, OPENED_BRACE=4, CLOSED_BRACE=5, SEPARATOR=6, 
		IDENTIFIER=7, DATE=8, INT=9, STRING=10, WHITESPACE=11;
	public static final String[] tokenNames = {
		"<INVALID>", "'SELECT'", "TYPE", "OPERATOR", "OPENED_BRACE", "CLOSED_BRACE", 
		"','", "IDENTIFIER", "DATE", "INT", "STRING", "WHITESPACE"
	};
	public static final int
		RULE_select = 0, RULE_interval = 1;
	public static final String[] ruleNames = {
		"select", "interval"
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
		public TerminalNode STMT_SELECT() { return getToken(QueryGrammarParser.STMT_SELECT, 0); }
		public IntervalContext interval() {
			return getRuleContext(IntervalContext.class,0);
		}
		public TerminalNode TYPE() { return getToken(QueryGrammarParser.TYPE, 0); }
		public TerminalNode OPERATOR() { return getToken(QueryGrammarParser.OPERATOR, 0); }
		public SelectContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_select; }
	}

	public final SelectContext select() throws RecognitionException {
		SelectContext _localctx = new SelectContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_select);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(4); match(STMT_SELECT);
			setState(5); match(TYPE);
			setState(6); match(OPERATOR);
			setState(7); interval();
			}
		}
		catch (RecognitionException re) {
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
		public List<TerminalNode> DATE() { return getTokens(QueryGrammarParser.DATE); }
		public TerminalNode INT() { return getToken(QueryGrammarParser.INT, 0); }
		public TerminalNode OPENED_BRACE() { return getToken(QueryGrammarParser.OPENED_BRACE, 0); }
		public TerminalNode SEPARATOR() { return getToken(QueryGrammarParser.SEPARATOR, 0); }
		public TerminalNode DATE(int i) {
			return getToken(QueryGrammarParser.DATE, i);
		}
		public TerminalNode CLOSED_BRACE() { return getToken(QueryGrammarParser.CLOSED_BRACE, 0); }
		public IntervalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_interval; }
	}

	public final IntervalContext interval() throws RecognitionException {
		IntervalContext _localctx = new IntervalContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_interval);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(9); match(OPENED_BRACE);
			setState(16);
			switch (_input.LA(1)) {
			case INT:
				{
				setState(10); match(INT);
				setState(11); match(SEPARATOR);
				setState(12); match(INT);
				}
				break;
			case DATE:
				{
				setState(13); match(DATE);
				setState(14); match(SEPARATOR);
				setState(15); match(DATE);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			setState(18); match(CLOSED_BRACE);
			}
		}
		catch (RecognitionException re) {
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
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\3\r\27\4\2\t\2\4\3"+
		"\t\3\3\2\3\2\3\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\3\3\5\3\23\n\3\3\3\3"+
		"\3\3\3\2\4\2\4\2\2\25\2\6\3\2\2\2\4\13\3\2\2\2\6\7\7\3\2\2\7\b\7\4\2\2"+
		"\b\t\7\5\2\2\t\n\5\4\3\2\n\3\3\2\2\2\13\22\7\6\2\2\f\r\7\13\2\2\r\16\7"+
		"\b\2\2\16\23\7\13\2\2\17\20\7\n\2\2\20\21\7\b\2\2\21\23\7\n\2\2\22\f\3"+
		"\2\2\2\22\17\3\2\2\2\23\24\3\2\2\2\24\25\7\7\2\2\25\5\3\2\2\2\3\22";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}