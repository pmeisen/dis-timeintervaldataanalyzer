// Generated from Y:\dis-timeintervaldataanalyzer\src\net\meisen\dissertation\parser\query\generated\QueryGrammar.g4 by ANTLR 4.1

package net.meisen.dissertation.parser.query.generated;

import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class QueryGrammarLexer extends Lexer {
	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		STMT_SELECT=1, TYPE=2, OPERATOR=3, OPENED_BRACE=4, CLOSED_BRACE=5, SEPARATOR=6, 
		IDENTIFIER=7, DATE=8, INT=9, STRING=10, WHITESPACE=11;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"'SELECT'", "TYPE", "OPERATOR", "OPENED_BRACE", "CLOSED_BRACE", "','", 
		"IDENTIFIER", "DATE", "INT", "STRING", "WHITESPACE"
	};
	public static final String[] ruleNames = {
		"STMT_SELECT", "TYPE", "TYPE_TIMELINES", "TYPE_RECORDS", "OPERATOR", "OP_IN", 
		"OPENED_BRACE", "CLOSED_BRACE", "SEPARATOR", "IDENTIFIER", "DATE", "INT", 
		"STRING", "WHITESPACE", "SYM_STRING", "SYM_QUOTE", "DIGIT", "ESC"
	};


	public QueryGrammarLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "QueryGrammar.g4"; }

	@Override
	public String[] getTokenNames() { return tokenNames; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	@Override
	public void action(RuleContext _localctx, int ruleIndex, int actionIndex) {
		switch (ruleIndex) {
		case 13: WHITESPACE_action((RuleContext)_localctx, actionIndex); break;
		}
	}
	private void WHITESPACE_action(RuleContext _localctx, int actionIndex) {
		switch (actionIndex) {
		case 0: skip();  break;
		}
	}

	public static final String _serializedATN =
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\2\r\u0091\b\1\4\2\t"+
		"\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\3\3\3\5\3\61\n\3\3\4\3\4\3\4"+
		"\3\4\3\4\3\4\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\6\3\6\3"+
		"\7\3\7\3\7\3\b\3\b\3\t\3\t\3\n\3\n\3\13\3\13\7\13R\n\13\f\13\16\13U\13"+
		"\13\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3"+
		"\f\3\f\3\f\3\f\5\fk\n\f\5\fm\n\f\5\fo\n\f\3\r\6\rr\n\r\r\r\16\rs\3\16"+
		"\3\16\3\16\7\16y\n\16\f\16\16\16|\13\16\3\16\3\16\3\17\6\17\u0081\n\17"+
		"\r\17\16\17\u0082\3\17\3\17\3\20\3\20\3\21\3\21\3\22\3\22\3\23\3\23\3"+
		"\23\5\23\u0090\n\23\3z\24\3\3\1\5\4\1\7\2\1\t\2\1\13\5\1\r\2\1\17\6\1"+
		"\21\7\1\23\b\1\25\t\1\27\n\1\31\13\1\33\f\1\35\r\2\37\2\1!\2\1#\2\1%\2"+
		"\1\3\2\b\4\2**]]\4\2++__\4\2C\\c|\7\2//\62;C\\aac|\5\2\13\f\17\17\"\""+
		"\3\2\62;\u0093\2\3\3\2\2\2\2\5\3\2\2\2\2\13\3\2\2\2\2\17\3\2\2\2\2\21"+
		"\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2"+
		"\2\2\2\35\3\2\2\2\3\'\3\2\2\2\5\60\3\2\2\2\7\62\3\2\2\2\t<\3\2\2\2\13"+
		"D\3\2\2\2\rF\3\2\2\2\17I\3\2\2\2\21K\3\2\2\2\23M\3\2\2\2\25O\3\2\2\2\27"+
		"V\3\2\2\2\31q\3\2\2\2\33u\3\2\2\2\35\u0080\3\2\2\2\37\u0086\3\2\2\2!\u0088"+
		"\3\2\2\2#\u008a\3\2\2\2%\u008c\3\2\2\2\'(\7U\2\2()\7G\2\2)*\7N\2\2*+\7"+
		"G\2\2+,\7E\2\2,-\7V\2\2-\4\3\2\2\2.\61\5\7\4\2/\61\5\t\5\2\60.\3\2\2\2"+
		"\60/\3\2\2\2\61\6\3\2\2\2\62\63\7V\2\2\63\64\7K\2\2\64\65\7O\2\2\65\66"+
		"\7G\2\2\66\67\7N\2\2\678\7K\2\289\7P\2\29:\7G\2\2:;\7U\2\2;\b\3\2\2\2"+
		"<=\7T\2\2=>\7G\2\2>?\7E\2\2?@\7Q\2\2@A\7T\2\2AB\7F\2\2BC\7U\2\2C\n\3\2"+
		"\2\2DE\5\r\7\2E\f\3\2\2\2FG\7K\2\2GH\7P\2\2H\16\3\2\2\2IJ\t\2\2\2J\20"+
		"\3\2\2\2KL\t\3\2\2L\22\3\2\2\2MN\7.\2\2N\24\3\2\2\2OS\t\4\2\2PR\t\5\2"+
		"\2QP\3\2\2\2RU\3\2\2\2SQ\3\2\2\2ST\3\2\2\2T\26\3\2\2\2US\3\2\2\2VW\5#"+
		"\22\2WX\5#\22\2XY\7\60\2\2YZ\5#\22\2Z[\5#\22\2[\\\7\60\2\2\\]\5#\22\2"+
		"]^\5#\22\2^_\5#\22\2_`\5#\22\2`n\7\"\2\2ab\5#\22\2bl\5#\22\2cd\7<\2\2"+
		"de\5#\22\2ej\5#\22\2fg\7<\2\2gh\5#\22\2hi\5#\22\2ik\3\2\2\2jf\3\2\2\2"+
		"jk\3\2\2\2km\3\2\2\2lc\3\2\2\2lm\3\2\2\2mo\3\2\2\2na\3\2\2\2no\3\2\2\2"+
		"o\30\3\2\2\2pr\5#\22\2qp\3\2\2\2rs\3\2\2\2sq\3\2\2\2st\3\2\2\2t\32\3\2"+
		"\2\2uz\5\37\20\2vy\5%\23\2wy\13\2\2\2xv\3\2\2\2xw\3\2\2\2y|\3\2\2\2z{"+
		"\3\2\2\2zx\3\2\2\2{}\3\2\2\2|z\3\2\2\2}~\5\37\20\2~\34\3\2\2\2\177\u0081"+
		"\t\6\2\2\u0080\177\3\2\2\2\u0081\u0082\3\2\2\2\u0082\u0080\3\2\2\2\u0082"+
		"\u0083\3\2\2\2\u0083\u0084\3\2\2\2\u0084\u0085\b\17\2\2\u0085\36\3\2\2"+
		"\2\u0086\u0087\7$\2\2\u0087 \3\2\2\2\u0088\u0089\7^\2\2\u0089\"\3\2\2"+
		"\2\u008a\u008b\t\7\2\2\u008b$\3\2\2\2\u008c\u008f\5!\21\2\u008d\u0090"+
		"\5\37\20\2\u008e\u0090\5!\21\2\u008f\u008d\3\2\2\2\u008f\u008e\3\2\2\2"+
		"\u0090&\3\2\2\2\r\2\60Sjlnsxz\u0082\u008f";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}