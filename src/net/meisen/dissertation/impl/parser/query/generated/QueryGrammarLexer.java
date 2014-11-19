// Generated from Y:\dis-timeintervaldataanalyzer\src\net\meisen\dissertation\impl\parser\query\generated\QueryGrammar.g4 by ANTLR 4.1

package net.meisen.dissertation.impl.parser.query.generated;

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
		MARKED_ID=1, VALUE=2, NULL_VALUE=3, POS_START_INCL=4, POS_END_INCL=5, 
		POS_START_EXCL=6, POS_END_EXCL=7, STMT_GET=8, STMT_SELECT=9, STMT_INSERT=10, 
		STMT_DELETE=11, STMT_OPEN=12, STMT_LOAD=13, STMT_UNLOAD=14, STMT_ALIVE=15, 
		STMT_ADD=16, STMT_DROP=17, STMT_MODIFY=18, STMT_GRANT=19, STMT_REVOKE=20, 
		STMT_ASSIGN=21, STMT_REMOVE=22, PROP_AUTOLOAD=23, PROP_FORCE=24, PROP_PASSWORD=25, 
		TYPE_TIMESERIES=26, TYPE_RECORDS=27, TYPE_MODELS=28, TYPE_VERSION=29, 
		TYPE_PERMISSIONS=30, TYPE_ROLES=31, TYPE_USERS=32, TYPE_PERMISSION=33, 
		TYPE_ROLE=34, TYPE_USER=35, OP_FROM=36, OP_OF=37, OP_TO=38, OP_IN=39, 
		OP_INTO=40, OP_SET=41, OP_VALUES=42, OP_ALIAS=43, OP_GROUPBY=44, OP_FILTERBY=45, 
		OP_TRANSPOSE=46, OP_IDONLY=47, OP_WITH=48, IR_EQUALTO=49, IR_BEFORE=50, 
		IR_AFTER=51, IR_MEETING=52, IR_OVERLAPPING=53, IR_DURING=54, IR_WITHIN=55, 
		IR_CONTAINING=56, IR_STARTINGWITH=57, IR_FINISHINGWITH=58, LOGICAL_OR=59, 
		LOGICAL_AND=60, LOGICAL_NOT=61, LOGICAL_IGNORE=62, LOGICAL_TRUE=63, LOGICAL_FALSE=64, 
		MATH_MULTIPLY=65, MATH_DIVISION=66, MATH_PLUS=67, MATH_MINUS=68, AGGR_COUNT=69, 
		AGGR_SUM=70, AGGR_MIN=71, AGGR_MAX=72, AGGR_AVERAGE=73, AGGR_MODE=74, 
		AGGR_MEAN=75, AGGR_MEDIAN=76, CMP_EQUAL=77, BRACKET_ROUND_OPENED=78, BRACKET_ROUND_CLOSED=79, 
		BRACKET_SQUARE_OPENED=80, BRACKET_SQUARE_CLOSED=81, BRACKET_CURLY_OPENED=82, 
		BRACKET_CURLY_CLOSED=83, DATE=84, INT=85, SEPARATOR=86, DIMSEPARATOR=87, 
		SIMPLE_ID=88, ENHANCED_ID=89, WHITESPACE=90;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"MARKED_ID", "VALUE", "NULL_VALUE", "POS_START_INCL", "POS_END_INCL", 
		"POS_START_EXCL", "POS_END_EXCL", "STMT_GET", "STMT_SELECT", "STMT_INSERT", 
		"STMT_DELETE", "STMT_OPEN", "STMT_LOAD", "STMT_UNLOAD", "STMT_ALIVE", 
		"STMT_ADD", "STMT_DROP", "STMT_MODIFY", "STMT_GRANT", "STMT_REVOKE", "STMT_ASSIGN", 
		"STMT_REMOVE", "PROP_AUTOLOAD", "PROP_FORCE", "PROP_PASSWORD", "TYPE_TIMESERIES", 
		"TYPE_RECORDS", "TYPE_MODELS", "TYPE_VERSION", "TYPE_PERMISSIONS", "TYPE_ROLES", 
		"TYPE_USERS", "TYPE_PERMISSION", "TYPE_ROLE", "TYPE_USER", "OP_FROM", 
		"OP_OF", "OP_TO", "OP_IN", "OP_INTO", "OP_SET", "OP_VALUES", "OP_ALIAS", 
		"OP_GROUPBY", "OP_FILTERBY", "OP_TRANSPOSE", "OP_IDONLY", "OP_WITH", "IR_EQUALTO", 
		"IR_BEFORE", "IR_AFTER", "IR_MEETING", "IR_OVERLAPPING", "IR_DURING", 
		"IR_WITHIN", "IR_CONTAINING", "IR_STARTINGWITH", "IR_FINISHINGWITH", "LOGICAL_OR", 
		"LOGICAL_AND", "LOGICAL_NOT", "LOGICAL_IGNORE", "LOGICAL_TRUE", "LOGICAL_FALSE", 
		"'*'", "'/'", "'+'", "'-'", "AGGR_COUNT", "AGGR_SUM", "AGGR_MIN", "AGGR_MAX", 
		"AGGR_AVERAGE", "AGGR_MODE", "AGGR_MEAN", "AGGR_MEDIAN", "'='", "'('", 
		"')'", "'['", "']'", "'{'", "'}'", "DATE", "INT", "','", "'.'", "SIMPLE_ID", 
		"ENHANCED_ID", "WHITESPACE"
	};
	public static final String[] ruleNames = {
		"MARKED_ID", "VALUE", "NULL_VALUE", "POS_START_INCL", "POS_END_INCL", 
		"POS_START_EXCL", "POS_END_EXCL", "STMT_GET", "STMT_SELECT", "STMT_INSERT", 
		"STMT_DELETE", "STMT_OPEN", "STMT_LOAD", "STMT_UNLOAD", "STMT_ALIVE", 
		"STMT_ADD", "STMT_DROP", "STMT_MODIFY", "STMT_GRANT", "STMT_REVOKE", "STMT_ASSIGN", 
		"STMT_REMOVE", "PROP_AUTOLOAD", "PROP_FORCE", "PROP_PASSWORD", "TYPE_TIMESERIES", 
		"TYPE_RECORDS", "TYPE_MODELS", "TYPE_VERSION", "TYPE_PERMISSIONS", "TYPE_ROLES", 
		"TYPE_USERS", "TYPE_PERMISSION", "TYPE_ROLE", "TYPE_USER", "OP_FROM", 
		"OP_OF", "OP_TO", "OP_IN", "OP_INTO", "OP_SET", "OP_VALUES", "OP_ALIAS", 
		"OP_GROUPBY", "OP_FILTERBY", "OP_TRANSPOSE", "OP_IDONLY", "OP_WITH", "IR_EQUALTO", 
		"IR_BEFORE", "IR_AFTER", "IR_MEETING", "IR_OVERLAPPING", "IR_DURING", 
		"IR_WITHIN", "IR_CONTAINING", "IR_STARTINGWITH", "IR_FINISHINGWITH", "LOGICAL_OR", 
		"LOGICAL_AND", "LOGICAL_NOT", "LOGICAL_IGNORE", "LOGICAL_TRUE", "LOGICAL_FALSE", 
		"MATH_MULTIPLY", "MATH_DIVISION", "MATH_PLUS", "MATH_MINUS", "AGGR_COUNT", 
		"AGGR_SUM", "AGGR_MIN", "AGGR_MAX", "AGGR_AVERAGE", "AGGR_MODE", "AGGR_MEAN", 
		"AGGR_MEDIAN", "CMP_EQUAL", "BRACKET_ROUND_OPENED", "BRACKET_ROUND_CLOSED", 
		"BRACKET_SQUARE_OPENED", "BRACKET_SQUARE_CLOSED", "BRACKET_CURLY_OPENED", 
		"BRACKET_CURLY_CLOSED", "DATE", "INT", "SEPARATOR", "DIMSEPARATOR", "SIMPLE_ID", 
		"ENHANCED_ID", "WHITESPACE", "SYM_ALL_MASK", "SYM_VALUE", "SYM_QUOTE", 
		"SYM_IDMARKER", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", 
		"L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", 
		"Z"
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
		case 89: WHITESPACE_action((RuleContext)_localctx, actionIndex); break;
		}
	}
	private void WHITESPACE_action(RuleContext _localctx, int actionIndex) {
		switch (actionIndex) {
		case 0: skip();  break;
		}
	}

	public static final String _serializedATN =
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\2\\\u03d3\b\1\4\2\t"+
		"\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\4#\t#\4$\t$\4%\t%\4&\t&\4\'\t\'\4(\t(\4)\t)\4*\t*\4+\t+\4"+
		",\t,\4-\t-\4.\t.\4/\t/\4\60\t\60\4\61\t\61\4\62\t\62\4\63\t\63\4\64\t"+
		"\64\4\65\t\65\4\66\t\66\4\67\t\67\48\t8\49\t9\4:\t:\4;\t;\4<\t<\4=\t="+
		"\4>\t>\4?\t?\4@\t@\4A\tA\4B\tB\4C\tC\4D\tD\4E\tE\4F\tF\4G\tG\4H\tH\4I"+
		"\tI\4J\tJ\4K\tK\4L\tL\4M\tM\4N\tN\4O\tO\4P\tP\4Q\tQ\4R\tR\4S\tS\4T\tT"+
		"\4U\tU\4V\tV\4W\tW\4X\tX\4Y\tY\4Z\tZ\4[\t[\4\\\t\\\4]\t]\4^\t^\4_\t_\4"+
		"`\t`\4a\ta\4b\tb\4c\tc\4d\td\4e\te\4f\tf\4g\tg\4h\th\4i\ti\4j\tj\4k\t"+
		"k\4l\tl\4m\tm\4n\tn\4o\to\4p\tp\4q\tq\4r\tr\4s\ts\4t\tt\4u\tu\4v\tv\4"+
		"w\tw\4x\tx\4y\ty\3\2\3\2\3\2\5\2\u00f7\n\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3"+
		"\5\3\u0100\n\3\3\3\7\3\u0103\n\3\f\3\16\3\u0106\13\3\3\3\3\3\3\4\3\4\3"+
		"\4\3\4\3\4\3\5\3\5\3\5\3\5\3\5\3\5\3\5\5\5\u0116\n\5\3\5\3\5\3\6\3\6\3"+
		"\6\3\6\3\6\5\6\u011f\n\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3"+
		"\b\3\b\3\b\3\b\3\b\3\b\3\b\3\t\3\t\3\t\3\t\3\n\3\n\3\n\3\n\3\n\3\n\3\n"+
		"\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\r\3"+
		"\r\3\r\3\r\3\r\3\16\3\16\3\16\3\16\3\16\3\17\3\17\3\17\3\17\3\17\3\17"+
		"\3\17\3\20\3\20\3\20\3\20\3\20\3\20\3\21\3\21\3\21\3\21\3\22\3\22\3\22"+
		"\3\22\3\22\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\24\3\24\3\24\3\24\3\24"+
		"\3\24\3\25\3\25\3\25\3\25\3\25\3\25\3\25\3\26\3\26\3\26\3\26\3\26\3\26"+
		"\3\26\3\27\3\27\3\27\3\27\3\27\3\27\3\27\3\30\3\30\3\30\3\30\3\30\3\30"+
		"\3\30\3\30\3\30\3\31\3\31\3\31\3\31\3\31\3\31\3\32\3\32\3\32\3\32\3\32"+
		"\3\32\3\32\3\32\3\32\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3\33"+
		"\3\33\3\34\3\34\3\34\3\34\3\34\3\34\3\34\3\34\3\35\3\35\3\35\3\35\3\35"+
		"\3\35\3\35\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\36\3\37\3\37\3\37\3\37"+
		"\3\37\3\37\3\37\3\37\3\37\3\37\3\37\3\37\3 \3 \3 \3 \3 \3 \3!\3!\3!\3"+
		"!\3!\3!\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3\"\3#\3#\3#\3#\3#\3$"+
		"\3$\3$\3$\3$\3%\3%\3%\3%\3%\3&\3&\3&\3\'\3\'\3\'\3(\3(\3(\3)\3)\3)\3)"+
		"\3)\3*\3*\3*\3*\3+\3+\3+\3+\3+\3+\3+\3,\3,\3,\3-\3-\3-\3-\3-\3-\3-\3-"+
		"\3-\3.\3.\3.\3.\3.\3.\3.\3.\3.\3.\3.\3.\3.\3.\3.\3.\5.\u022f\n.\3/\3/"+
		"\3/\3/\3/\3/\3/\3/\3/\3/\3\60\3\60\3\60\3\60\3\61\3\61\3\61\3\61\3\61"+
		"\3\62\3\62\3\62\3\62\3\62\3\62\3\62\3\62\3\63\3\63\3\63\3\63\3\63\3\63"+
		"\3\63\3\64\3\64\3\64\3\64\3\64\3\64\3\65\3\65\3\65\3\65\3\65\3\65\3\65"+
		"\3\65\3\66\3\66\3\66\3\66\3\66\3\66\3\66\3\66\3\66\3\66\3\66\3\66\3\67"+
		"\3\67\3\67\3\67\3\67\3\67\3\67\38\38\38\38\38\38\38\39\39\39\39\39\39"+
		"\39\39\39\39\39\3:\3:\3:\3:\3:\3:\3:\3:\3:\3:\3:\3:\3:\3;\3;\3;\3;\3;"+
		"\3;\3;\3;\3;\3;\3;\3;\3;\3;\3<\3<\3<\3<\3<\5<\u02a6\n<\3=\3=\3=\3=\3="+
		"\3=\5=\u02ae\n=\3>\3>\3>\3>\3>\5>\u02b5\n>\3?\3?\3?\3?\3?\3?\3?\3@\3@"+
		"\3@\3@\3@\3A\3A\3A\3A\3A\3A\3B\3B\3C\3C\3D\3D\3E\3E\3F\3F\3F\3F\3F\3F"+
		"\3G\3G\3G\3G\3H\3H\3H\3H\3I\3I\3I\3I\3J\3J\3J\3J\3J\3J\3J\3J\3K\3K\3K"+
		"\3K\3K\3L\3L\3L\3L\3L\3M\3M\3M\3M\3M\3M\3M\3N\3N\3O\3O\3P\3P\3Q\3Q\3R"+
		"\3R\3S\3S\3T\3T\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U"+
		"\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U"+
		"\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U"+
		"\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U"+
		"\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U\3U"+
		"\3U\3U\5U\u037a\nU\3V\6V\u037d\nV\rV\16V\u037e\3W\3W\3X\3X\3Y\6Y\u0386"+
		"\nY\rY\16Y\u0387\3Z\3Z\7Z\u038c\nZ\fZ\16Z\u038f\13Z\3[\6[\u0392\n[\r["+
		"\16[\u0393\3[\3[\3\\\3\\\3]\3]\3^\3^\3_\3_\3`\3`\3a\3a\3b\3b\3c\3c\3d"+
		"\3d\3e\3e\3f\3f\3g\3g\3h\3h\3i\3i\3j\3j\3k\3k\3l\3l\3m\3m\3n\3n\3o\3o"+
		"\3p\3p\3q\3q\3r\3r\3s\3s\3t\3t\3u\3u\3v\3v\3w\3w\3x\3x\3y\3y\3\u0104z"+
		"\3\3\1\5\4\1\7\5\1\t\6\1\13\7\1\r\b\1\17\t\1\21\n\1\23\13\1\25\f\1\27"+
		"\r\1\31\16\1\33\17\1\35\20\1\37\21\1!\22\1#\23\1%\24\1\'\25\1)\26\1+\27"+
		"\1-\30\1/\31\1\61\32\1\63\33\1\65\34\1\67\35\19\36\1;\37\1= \1?!\1A\""+
		"\1C#\1E$\1G%\1I&\1K\'\1M(\1O)\1Q*\1S+\1U,\1W-\1Y.\1[/\1]\60\1_\61\1a\62"+
		"\1c\63\1e\64\1g\65\1i\66\1k\67\1m8\1o9\1q:\1s;\1u<\1w=\1y>\1{?\1}@\1\177"+
		"A\1\u0081B\1\u0083C\1\u0085D\1\u0087E\1\u0089F\1\u008bG\1\u008dH\1\u008f"+
		"I\1\u0091J\1\u0093K\1\u0095L\1\u0097M\1\u0099N\1\u009bO\1\u009dP\1\u009f"+
		"Q\1\u00a1R\1\u00a3S\1\u00a5T\1\u00a7U\1\u00a9V\1\u00abW\1\u00adX\1\u00af"+
		"Y\1\u00b1Z\1\u00b3[\1\u00b5\\\2\u00b7\2\1\u00b9\2\1\u00bb\2\1\u00bd\2"+
		"\1\u00bf\2\1\u00c1\2\1\u00c3\2\1\u00c5\2\1\u00c7\2\1\u00c9\2\1\u00cb\2"+
		"\1\u00cd\2\1\u00cf\2\1\u00d1\2\1\u00d3\2\1\u00d5\2\1\u00d7\2\1\u00d9\2"+
		"\1\u00db\2\1\u00dd\2\1\u00df\2\1\u00e1\2\1\u00e3\2\1\u00e5\2\1\u00e7\2"+
		"\1\u00e9\2\1\u00eb\2\1\u00ed\2\1\u00ef\2\1\u00f1\2\1\3\2!\4\2))^^\3\2"+
		"\62;\4\2C\\c|\7\2//\62;C\\aac|\5\2\13\f\17\17\"\"\4\2CCcc\4\2DDdd\4\2"+
		"EEee\4\2FFff\4\2GGgg\4\2HHhh\4\2IIii\4\2JJjj\4\2KKkk\4\2LLll\4\2MMmm\4"+
		"\2NNnn\4\2OOoo\4\2PPpp\4\2QQqq\4\2RRrr\4\2SSss\4\2TTtt\4\2UUuu\4\2VVv"+
		"v\4\2WWww\4\2XXxx\4\2YYyy\4\2ZZzz\4\2[[{{\4\2\\\\||\u03ca\2\3\3\2\2\2"+
		"\2\5\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2"+
		"\2\2\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2"+
		"\2\33\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2"+
		"\2\2\'\3\2\2\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2\2\2\61\3\2\2"+
		"\2\2\63\3\2\2\2\2\65\3\2\2\2\2\67\3\2\2\2\29\3\2\2\2\2;\3\2\2\2\2=\3\2"+
		"\2\2\2?\3\2\2\2\2A\3\2\2\2\2C\3\2\2\2\2E\3\2\2\2\2G\3\2\2\2\2I\3\2\2\2"+
		"\2K\3\2\2\2\2M\3\2\2\2\2O\3\2\2\2\2Q\3\2\2\2\2S\3\2\2\2\2U\3\2\2\2\2W"+
		"\3\2\2\2\2Y\3\2\2\2\2[\3\2\2\2\2]\3\2\2\2\2_\3\2\2\2\2a\3\2\2\2\2c\3\2"+
		"\2\2\2e\3\2\2\2\2g\3\2\2\2\2i\3\2\2\2\2k\3\2\2\2\2m\3\2\2\2\2o\3\2\2\2"+
		"\2q\3\2\2\2\2s\3\2\2\2\2u\3\2\2\2\2w\3\2\2\2\2y\3\2\2\2\2{\3\2\2\2\2}"+
		"\3\2\2\2\2\177\3\2\2\2\2\u0081\3\2\2\2\2\u0083\3\2\2\2\2\u0085\3\2\2\2"+
		"\2\u0087\3\2\2\2\2\u0089\3\2\2\2\2\u008b\3\2\2\2\2\u008d\3\2\2\2\2\u008f"+
		"\3\2\2\2\2\u0091\3\2\2\2\2\u0093\3\2\2\2\2\u0095\3\2\2\2\2\u0097\3\2\2"+
		"\2\2\u0099\3\2\2\2\2\u009b\3\2\2\2\2\u009d\3\2\2\2\2\u009f\3\2\2\2\2\u00a1"+
		"\3\2\2\2\2\u00a3\3\2\2\2\2\u00a5\3\2\2\2\2\u00a7\3\2\2\2\2\u00a9\3\2\2"+
		"\2\2\u00ab\3\2\2\2\2\u00ad\3\2\2\2\2\u00af\3\2\2\2\2\u00b1\3\2\2\2\2\u00b3"+
		"\3\2\2\2\2\u00b5\3\2\2\2\3\u00f3\3\2\2\2\5\u00fa\3\2\2\2\7\u0109\3\2\2"+
		"\2\t\u010e\3\2\2\2\13\u0119\3\2\2\2\r\u0122\3\2\2\2\17\u012b\3\2\2\2\21"+
		"\u0132\3\2\2\2\23\u0136\3\2\2\2\25\u013d\3\2\2\2\27\u0144\3\2\2\2\31\u014b"+
		"\3\2\2\2\33\u0150\3\2\2\2\35\u0155\3\2\2\2\37\u015c\3\2\2\2!\u0162\3\2"+
		"\2\2#\u0166\3\2\2\2%\u016b\3\2\2\2\'\u0172\3\2\2\2)\u0178\3\2\2\2+\u017f"+
		"\3\2\2\2-\u0186\3\2\2\2/\u018d\3\2\2\2\61\u0196\3\2\2\2\63\u019c\3\2\2"+
		"\2\65\u01a5\3\2\2\2\67\u01b0\3\2\2\29\u01b8\3\2\2\2;\u01bf\3\2\2\2=\u01c7"+
		"\3\2\2\2?\u01d3\3\2\2\2A\u01d9\3\2\2\2C\u01df\3\2\2\2E\u01ea\3\2\2\2G"+
		"\u01ef\3\2\2\2I\u01f4\3\2\2\2K\u01f9\3\2\2\2M\u01fc\3\2\2\2O\u01ff\3\2"+
		"\2\2Q\u0202\3\2\2\2S\u0207\3\2\2\2U\u020b\3\2\2\2W\u0212\3\2\2\2Y\u0215"+
		"\3\2\2\2[\u022e\3\2\2\2]\u0230\3\2\2\2_\u023a\3\2\2\2a\u023e\3\2\2\2c"+
		"\u0243\3\2\2\2e\u024b\3\2\2\2g\u0252\3\2\2\2i\u0258\3\2\2\2k\u0260\3\2"+
		"\2\2m\u026c\3\2\2\2o\u0273\3\2\2\2q\u027a\3\2\2\2s\u0285\3\2\2\2u\u0292"+
		"\3\2\2\2w\u02a5\3\2\2\2y\u02ad\3\2\2\2{\u02b4\3\2\2\2}\u02b6\3\2\2\2\177"+
		"\u02bd\3\2\2\2\u0081\u02c2\3\2\2\2\u0083\u02c8\3\2\2\2\u0085\u02ca\3\2"+
		"\2\2\u0087\u02cc\3\2\2\2\u0089\u02ce\3\2\2\2\u008b\u02d0\3\2\2\2\u008d"+
		"\u02d6\3\2\2\2\u008f\u02da\3\2\2\2\u0091\u02de\3\2\2\2\u0093\u02e2\3\2"+
		"\2\2\u0095\u02ea\3\2\2\2\u0097\u02ef\3\2\2\2\u0099\u02f4\3\2\2\2\u009b"+
		"\u02fb\3\2\2\2\u009d\u02fd\3\2\2\2\u009f\u02ff\3\2\2\2\u00a1\u0301\3\2"+
		"\2\2\u00a3\u0303\3\2\2\2\u00a5\u0305\3\2\2\2\u00a7\u0307\3\2\2\2\u00a9"+
		"\u0379\3\2\2\2\u00ab\u037c\3\2\2\2\u00ad\u0380\3\2\2\2\u00af\u0382\3\2"+
		"\2\2\u00b1\u0385\3\2\2\2\u00b3\u0389\3\2\2\2\u00b5\u0391\3\2\2\2\u00b7"+
		"\u0397\3\2\2\2\u00b9\u0399\3\2\2\2\u00bb\u039b\3\2\2\2\u00bd\u039d\3\2"+
		"\2\2\u00bf\u039f\3\2\2\2\u00c1\u03a1\3\2\2\2\u00c3\u03a3\3\2\2\2\u00c5"+
		"\u03a5\3\2\2\2\u00c7\u03a7\3\2\2\2\u00c9\u03a9\3\2\2\2\u00cb\u03ab\3\2"+
		"\2\2\u00cd\u03ad\3\2\2\2\u00cf\u03af\3\2\2\2\u00d1\u03b1\3\2\2\2\u00d3"+
		"\u03b3\3\2\2\2\u00d5\u03b5\3\2\2\2\u00d7\u03b7\3\2\2\2\u00d9\u03b9\3\2"+
		"\2\2\u00db\u03bb\3\2\2\2\u00dd\u03bd\3\2\2\2\u00df\u03bf\3\2\2\2\u00e1"+
		"\u03c1\3\2\2\2\u00e3\u03c3\3\2\2\2\u00e5\u03c5\3\2\2\2\u00e7\u03c7\3\2"+
		"\2\2\u00e9\u03c9\3\2\2\2\u00eb\u03cb\3\2\2\2\u00ed\u03cd\3\2\2\2\u00ef"+
		"\u03cf\3\2\2\2\u00f1\u03d1\3\2\2\2\u00f3\u00f6\5\u00bd_\2\u00f4\u00f7"+
		"\5\u00b1Y\2\u00f5\u00f7\5\u00b3Z\2\u00f6\u00f4\3\2\2\2\u00f6\u00f5\3\2"+
		"\2\2\u00f7\u00f8\3\2\2\2\u00f8\u00f9\5\u00bd_\2\u00f9\4\3\2\2\2\u00fa"+
		"\u0104\5\u00b9]\2\u00fb\u00ff\5\u00bb^\2\u00fc\u0100\5\u00b9]\2\u00fd"+
		"\u0100\5\u00bb^\2\u00fe\u0100\5\u00b7\\\2\u00ff\u00fc\3\2\2\2\u00ff\u00fd"+
		"\3\2\2\2\u00ff\u00fe\3\2\2\2\u0100\u0103\3\2\2\2\u0101\u0103\n\2\2\2\u0102"+
		"\u00fb\3\2\2\2\u0102\u0101\3\2\2\2\u0103\u0106\3\2\2\2\u0104\u0105\3\2"+
		"\2\2\u0104\u0102\3\2\2\2\u0105\u0107\3\2\2\2\u0106\u0104\3\2\2\2\u0107"+
		"\u0108\5\u00b9]\2\u0108\6\3\2\2\2\u0109\u010a\5\u00d9m\2\u010a\u010b\5"+
		"\u00e7t\2\u010b\u010c\5\u00d5k\2\u010c\u010d\5\u00d5k\2\u010d\b\3\2\2"+
		"\2\u010e\u010f\5\u00a1Q\2\u010f\u0110\5\u00e3r\2\u0110\u0111\5\u00e5s"+
		"\2\u0111\u0112\5\u00bf`\2\u0112\u0113\5\u00e1q\2\u0113\u0115\5\u00e5s"+
		"\2\u0114\u0116\7-\2\2\u0115\u0114\3\2\2\2\u0115\u0116\3\2\2\2\u0116\u0117"+
		"\3\2\2\2\u0117\u0118\5\u00a3R\2\u0118\n\3\2\2\2\u0119\u011a\5\u00a1Q\2"+
		"\u011a\u011b\5\u00c7d\2\u011b\u011c\5\u00d9m\2\u011c\u011e\5\u00c5c\2"+
		"\u011d\u011f\7-\2\2\u011e\u011d\3\2\2\2\u011e\u011f\3\2\2\2\u011f\u0120"+
		"\3\2\2\2\u0120\u0121\5\u00a3R\2\u0121\f\3\2\2\2\u0122\u0123\5\u00a1Q\2"+
		"\u0123\u0124\5\u00e3r\2\u0124\u0125\5\u00e5s\2\u0125\u0126\5\u00bf`\2"+
		"\u0126\u0127\5\u00e1q\2\u0127\u0128\5\u00e5s\2\u0128\u0129\7/\2\2\u0129"+
		"\u012a\5\u00a3R\2\u012a\16\3\2\2\2\u012b\u012c\5\u00a1Q\2\u012c\u012d"+
		"\5\u00c7d\2\u012d\u012e\5\u00d9m\2\u012e\u012f\5\u00c5c\2\u012f\u0130"+
		"\7/\2\2\u0130\u0131\5\u00a3R\2\u0131\20\3\2\2\2\u0132\u0133\5\u00cbf\2"+
		"\u0133\u0134\5\u00c7d\2\u0134\u0135\5\u00e5s\2\u0135\22\3\2\2\2\u0136"+
		"\u0137\5\u00e3r\2\u0137\u0138\5\u00c7d\2\u0138\u0139\5\u00d5k\2\u0139"+
		"\u013a\5\u00c7d\2\u013a\u013b\5\u00c3b\2\u013b\u013c\5\u00e5s\2\u013c"+
		"\24\3\2\2\2\u013d\u013e\5\u00cfh\2\u013e\u013f\5\u00d9m\2\u013f\u0140"+
		"\5\u00e3r\2\u0140\u0141\5\u00c7d\2\u0141\u0142\5\u00e1q\2\u0142\u0143"+
		"\5\u00e5s\2\u0143\26\3\2\2\2\u0144\u0145\5\u00c5c\2\u0145\u0146\5\u00c7"+
		"d\2\u0146\u0147\5\u00d5k\2\u0147\u0148\5\u00c7d\2\u0148\u0149\5\u00e5"+
		"s\2\u0149\u014a\5\u00c7d\2\u014a\30\3\2\2\2\u014b\u014c\5\u00dbn\2\u014c"+
		"\u014d\5\u00ddo\2\u014d\u014e\5\u00c7d\2\u014e\u014f\5\u00d9m\2\u014f"+
		"\32\3\2\2\2\u0150\u0151\5\u00d5k\2\u0151\u0152\5\u00dbn\2\u0152\u0153"+
		"\5\u00bf`\2\u0153\u0154\5\u00c5c\2\u0154\34\3\2\2\2\u0155\u0156\5\u00e7"+
		"t\2\u0156\u0157\5\u00d9m\2\u0157\u0158\5\u00d5k\2\u0158\u0159\5\u00db"+
		"n\2\u0159\u015a\5\u00bf`\2\u015a\u015b\5\u00c5c\2\u015b\36\3\2\2\2\u015c"+
		"\u015d\5\u00bf`\2\u015d\u015e\5\u00d5k\2\u015e\u015f\5\u00cfh\2\u015f"+
		"\u0160\5\u00e9u\2\u0160\u0161\5\u00c7d\2\u0161 \3\2\2\2\u0162\u0163\5"+
		"\u00bf`\2\u0163\u0164\5\u00c5c\2\u0164\u0165\5\u00c5c\2\u0165\"\3\2\2"+
		"\2\u0166\u0167\5\u00c5c\2\u0167\u0168\5\u00e1q\2\u0168\u0169\5\u00dbn"+
		"\2\u0169\u016a\5\u00ddo\2\u016a$\3\2\2\2\u016b\u016c\5\u00d7l\2\u016c"+
		"\u016d\5\u00dbn\2\u016d\u016e\5\u00c5c\2\u016e\u016f\5\u00cfh\2\u016f"+
		"\u0170\5\u00c9e\2\u0170\u0171\5\u00efx\2\u0171&\3\2\2\2\u0172\u0173\5"+
		"\u00cbf\2\u0173\u0174\5\u00e1q\2\u0174\u0175\5\u00bf`\2\u0175\u0176\5"+
		"\u00d9m\2\u0176\u0177\5\u00e5s\2\u0177(\3\2\2\2\u0178\u0179\5\u00e1q\2"+
		"\u0179\u017a\5\u00c7d\2\u017a\u017b\5\u00e9u\2\u017b\u017c\5\u00dbn\2"+
		"\u017c\u017d\5\u00d3j\2\u017d\u017e\5\u00c7d\2\u017e*\3\2\2\2\u017f\u0180"+
		"\5\u00bf`\2\u0180\u0181\5\u00e3r\2\u0181\u0182\5\u00e3r\2\u0182\u0183"+
		"\5\u00cfh\2\u0183\u0184\5\u00cbf\2\u0184\u0185\5\u00d9m\2\u0185,\3\2\2"+
		"\2\u0186\u0187\5\u00e1q\2\u0187\u0188\5\u00c7d\2\u0188\u0189\5\u00d7l"+
		"\2\u0189\u018a\5\u00dbn\2\u018a\u018b\5\u00e9u\2\u018b\u018c\5\u00c7d"+
		"\2\u018c.\3\2\2\2\u018d\u018e\5\u00bf`\2\u018e\u018f\5\u00e7t\2\u018f"+
		"\u0190\5\u00e5s\2\u0190\u0191\5\u00dbn\2\u0191\u0192\5\u00d5k\2\u0192"+
		"\u0193\5\u00dbn\2\u0193\u0194\5\u00bf`\2\u0194\u0195\5\u00c5c\2\u0195"+
		"\60\3\2\2\2\u0196\u0197\5\u00c9e\2\u0197\u0198\5\u00dbn\2\u0198\u0199"+
		"\5\u00e1q\2\u0199\u019a\5\u00c3b\2\u019a\u019b\5\u00c7d\2\u019b\62\3\2"+
		"\2\2\u019c\u019d\5\u00ddo\2\u019d\u019e\5\u00bf`\2\u019e\u019f\5\u00e3"+
		"r\2\u019f\u01a0\5\u00e3r\2\u01a0\u01a1\5\u00ebv\2\u01a1\u01a2\5\u00db"+
		"n\2\u01a2\u01a3\5\u00e1q\2\u01a3\u01a4\5\u00c5c\2\u01a4\64\3\2\2\2\u01a5"+
		"\u01a6\5\u00e5s\2\u01a6\u01a7\5\u00cfh\2\u01a7\u01a8\5\u00d7l\2\u01a8"+
		"\u01a9\5\u00c7d\2\u01a9\u01aa\5\u00e3r\2\u01aa\u01ab\5\u00c7d\2\u01ab"+
		"\u01ac\5\u00e1q\2\u01ac\u01ad\5\u00cfh\2\u01ad\u01ae\5\u00c7d\2\u01ae"+
		"\u01af\5\u00e3r\2\u01af\66\3\2\2\2\u01b0\u01b1\5\u00e1q\2\u01b1\u01b2"+
		"\5\u00c7d\2\u01b2\u01b3\5\u00c3b\2\u01b3\u01b4\5\u00dbn\2\u01b4\u01b5"+
		"\5\u00e1q\2\u01b5\u01b6\5\u00c5c\2\u01b6\u01b7\5\u00e3r\2\u01b78\3\2\2"+
		"\2\u01b8\u01b9\5\u00d7l\2\u01b9\u01ba\5\u00dbn\2\u01ba\u01bb\5\u00c5c"+
		"\2\u01bb\u01bc\5\u00c7d\2\u01bc\u01bd\5\u00d5k\2\u01bd\u01be\5\u00e3r"+
		"\2\u01be:\3\2\2\2\u01bf\u01c0\5\u00e9u\2\u01c0\u01c1\5\u00c7d\2\u01c1"+
		"\u01c2\5\u00e1q\2\u01c2\u01c3\5\u00e3r\2\u01c3\u01c4\5\u00cfh\2\u01c4"+
		"\u01c5\5\u00dbn\2\u01c5\u01c6\5\u00d9m\2\u01c6<\3\2\2\2\u01c7\u01c8\5"+
		"\u00ddo\2\u01c8\u01c9\5\u00c7d\2\u01c9\u01ca\5\u00e1q\2\u01ca\u01cb\5"+
		"\u00d7l\2\u01cb\u01cc\5\u00cfh\2\u01cc\u01cd\5\u00e3r\2\u01cd\u01ce\5"+
		"\u00e3r\2\u01ce\u01cf\5\u00cfh\2\u01cf\u01d0\5\u00dbn\2\u01d0\u01d1\5"+
		"\u00d9m\2\u01d1\u01d2\5\u00e3r\2\u01d2>\3\2\2\2\u01d3\u01d4\5\u00e1q\2"+
		"\u01d4\u01d5\5\u00dbn\2\u01d5\u01d6\5\u00d5k\2\u01d6\u01d7\5\u00c7d\2"+
		"\u01d7\u01d8\5\u00e3r\2\u01d8@\3\2\2\2\u01d9\u01da\5\u00e7t\2\u01da\u01db"+
		"\5\u00e3r\2\u01db\u01dc\5\u00c7d\2\u01dc\u01dd\5\u00e1q\2\u01dd\u01de"+
		"\5\u00e3r\2\u01deB\3\2\2\2\u01df\u01e0\5\u00ddo\2\u01e0\u01e1\5\u00c7"+
		"d\2\u01e1\u01e2\5\u00e1q\2\u01e2\u01e3\5\u00d7l\2\u01e3\u01e4\5\u00cf"+
		"h\2\u01e4\u01e5\5\u00e3r\2\u01e5\u01e6\5\u00e3r\2\u01e6\u01e7\5\u00cf"+
		"h\2\u01e7\u01e8\5\u00dbn\2\u01e8\u01e9\5\u00d9m\2\u01e9D\3\2\2\2\u01ea"+
		"\u01eb\5\u00e1q\2\u01eb\u01ec\5\u00dbn\2\u01ec\u01ed\5\u00d5k\2\u01ed"+
		"\u01ee\5\u00c7d\2\u01eeF\3\2\2\2\u01ef\u01f0\5\u00e7t\2\u01f0\u01f1\5"+
		"\u00e3r\2\u01f1\u01f2\5\u00c7d\2\u01f2\u01f3\5\u00e1q\2\u01f3H\3\2\2\2"+
		"\u01f4\u01f5\5\u00c9e\2\u01f5\u01f6\5\u00e1q\2\u01f6\u01f7\5\u00dbn\2"+
		"\u01f7\u01f8\5\u00d7l\2\u01f8J\3\2\2\2\u01f9\u01fa\5\u00dbn\2\u01fa\u01fb"+
		"\5\u00c9e\2\u01fbL\3\2\2\2\u01fc\u01fd\5\u00e5s\2\u01fd\u01fe\5\u00db"+
		"n\2\u01feN\3\2\2\2\u01ff\u0200\5\u00cfh\2\u0200\u0201\5\u00d9m\2\u0201"+
		"P\3\2\2\2\u0202\u0203\5\u00cfh\2\u0203\u0204\5\u00d9m\2\u0204\u0205\5"+
		"\u00e5s\2\u0205\u0206\5\u00dbn\2\u0206R\3\2\2\2\u0207\u0208\5\u00e3r\2"+
		"\u0208\u0209\5\u00c7d\2\u0209\u020a\5\u00e5s\2\u020aT\3\2\2\2\u020b\u020c"+
		"\5\u00e9u\2\u020c\u020d\5\u00bf`\2\u020d\u020e\5\u00d5k\2\u020e\u020f"+
		"\5\u00e7t\2\u020f\u0210\5\u00c7d\2\u0210\u0211\5\u00e3r\2\u0211V\3\2\2"+
		"\2\u0212\u0213\5\u00bf`\2\u0213\u0214\5\u00e3r\2\u0214X\3\2\2\2\u0215"+
		"\u0216\5\u00cbf\2\u0216\u0217\5\u00e1q\2\u0217\u0218\5\u00dbn\2\u0218"+
		"\u0219\5\u00e7t\2\u0219\u021a\5\u00ddo\2\u021a\u021b\7\"\2\2\u021b\u021c"+
		"\5\u00c1a\2\u021c\u021d\5\u00efx\2\u021dZ\3\2\2\2\u021e\u021f\5\u00c9"+
		"e\2\u021f\u0220\5\u00cfh\2\u0220\u0221\5\u00d5k\2\u0221\u0222\5\u00e5"+
		"s\2\u0222\u0223\5\u00c7d\2\u0223\u0224\5\u00e1q\2\u0224\u0225\7\"\2\2"+
		"\u0225\u0226\5\u00c1a\2\u0226\u0227\5\u00efx\2\u0227\u022f\3\2\2\2\u0228"+
		"\u0229\5\u00ebv\2\u0229\u022a\5\u00cdg\2\u022a\u022b\5\u00c7d\2\u022b"+
		"\u022c\5\u00e1q\2\u022c\u022d\5\u00c7d\2\u022d\u022f\3\2\2\2\u022e\u021e"+
		"\3\2\2\2\u022e\u0228\3\2\2\2\u022f\\\3\2\2\2\u0230\u0231\5\u00e5s\2\u0231"+
		"\u0232\5\u00e1q\2\u0232\u0233\5\u00bf`\2\u0233\u0234\5\u00d9m\2\u0234"+
		"\u0235\5\u00e3r\2\u0235\u0236\5\u00ddo\2\u0236\u0237\5\u00dbn\2\u0237"+
		"\u0238\5\u00e3r\2\u0238\u0239\5\u00c7d\2\u0239^\3\2\2\2\u023a\u023b\5"+
		"\u00cfh\2\u023b\u023c\5\u00c5c\2\u023c\u023d\5\u00e3r\2\u023d`\3\2\2\2"+
		"\u023e\u023f\5\u00ebv\2\u023f\u0240\5\u00cfh\2\u0240\u0241\5\u00e5s\2"+
		"\u0241\u0242\5\u00cdg\2\u0242b\3\2\2\2\u0243\u0244\5\u00c7d\2\u0244\u0245"+
		"\5\u00dfp\2\u0245\u0246\5\u00e7t\2\u0246\u0247\5\u00bf`\2\u0247\u0248"+
		"\5\u00d5k\2\u0248\u0249\5\u00e5s\2\u0249\u024a\5\u00dbn\2\u024ad\3\2\2"+
		"\2\u024b\u024c\5\u00c1a\2\u024c\u024d\5\u00c7d\2\u024d\u024e\5\u00c9e"+
		"\2\u024e\u024f\5\u00dbn\2\u024f\u0250\5\u00e1q\2\u0250\u0251\5\u00c7d"+
		"\2\u0251f\3\2\2\2\u0252\u0253\5\u00bf`\2\u0253\u0254\5\u00c9e\2\u0254"+
		"\u0255\5\u00e5s\2\u0255\u0256\5\u00c7d\2\u0256\u0257\5\u00e1q\2\u0257"+
		"h\3\2\2\2\u0258\u0259\5\u00d7l\2\u0259\u025a\5\u00c7d\2\u025a\u025b\5"+
		"\u00c7d\2\u025b\u025c\5\u00e5s\2\u025c\u025d\5\u00cfh\2\u025d\u025e\5"+
		"\u00d9m\2\u025e\u025f\5\u00cbf\2\u025fj\3\2\2\2\u0260\u0261\5\u00dbn\2"+
		"\u0261\u0262\5\u00e9u\2\u0262\u0263\5\u00c7d\2\u0263\u0264\5\u00e1q\2"+
		"\u0264\u0265\5\u00d5k\2\u0265\u0266\5\u00bf`\2\u0266\u0267\5\u00ddo\2"+
		"\u0267\u0268\5\u00ddo\2\u0268\u0269\5\u00cfh\2\u0269\u026a\5\u00d9m\2"+
		"\u026a\u026b\5\u00cbf\2\u026bl\3\2\2\2\u026c\u026d\5\u00c5c\2\u026d\u026e"+
		"\5\u00e7t\2\u026e\u026f\5\u00e1q\2\u026f\u0270\5\u00cfh\2\u0270\u0271"+
		"\5\u00d9m\2\u0271\u0272\5\u00cbf\2\u0272n\3\2\2\2\u0273\u0274\5\u00eb"+
		"v\2\u0274\u0275\5\u00cfh\2\u0275\u0276\5\u00e5s\2\u0276\u0277\5\u00cd"+
		"g\2\u0277\u0278\5\u00cfh\2\u0278\u0279\5\u00d9m\2\u0279p\3\2\2\2\u027a"+
		"\u027b\5\u00c3b\2\u027b\u027c\5\u00dbn\2\u027c\u027d\5\u00d9m\2\u027d"+
		"\u027e\5\u00e5s\2\u027e\u027f\5\u00bf`\2\u027f\u0280\5\u00cfh\2\u0280"+
		"\u0281\5\u00d9m\2\u0281\u0282\5\u00cfh\2\u0282\u0283\5\u00d9m\2\u0283"+
		"\u0284\5\u00cbf\2\u0284r\3\2\2\2\u0285\u0286\5\u00e3r\2\u0286\u0287\5"+
		"\u00e5s\2\u0287\u0288\5\u00bf`\2\u0288\u0289\5\u00e1q\2\u0289\u028a\5"+
		"\u00e5s\2\u028a\u028b\5\u00cfh\2\u028b\u028c\5\u00d9m\2\u028c\u028d\5"+
		"\u00cbf\2\u028d\u028e\5\u00ebv\2\u028e\u028f\5\u00cfh\2\u028f\u0290\5"+
		"\u00e5s\2\u0290\u0291\5\u00cdg\2\u0291t\3\2\2\2\u0292\u0293\5\u00c9e\2"+
		"\u0293\u0294\5\u00cfh\2\u0294\u0295\5\u00d9m\2\u0295\u0296\5\u00cfh\2"+
		"\u0296\u0297\5\u00e3r\2\u0297\u0298\5\u00cdg\2\u0298\u0299\5\u00cfh\2"+
		"\u0299\u029a\5\u00d9m\2\u029a\u029b\5\u00cbf\2\u029b\u029c\5\u00ebv\2"+
		"\u029c\u029d\5\u00cfh\2\u029d\u029e\5\u00e5s\2\u029e\u029f\5\u00cdg\2"+
		"\u029fv\3\2\2\2\u02a0\u02a1\5\u00dbn\2\u02a1\u02a2\5\u00e1q\2\u02a2\u02a6"+
		"\3\2\2\2\u02a3\u02a4\7~\2\2\u02a4\u02a6\7~\2\2\u02a5\u02a0\3\2\2\2\u02a5"+
		"\u02a3\3\2\2\2\u02a6x\3\2\2\2\u02a7\u02a8\5\u00bf`\2\u02a8\u02a9\5\u00d9"+
		"m\2\u02a9\u02aa\5\u00c5c\2\u02aa\u02ae\3\2\2\2\u02ab\u02ac\7(\2\2\u02ac"+
		"\u02ae\7(\2\2\u02ad\u02a7\3\2\2\2\u02ad\u02ab\3\2\2\2\u02aez\3\2\2\2\u02af"+
		"\u02b0\5\u00d9m\2\u02b0\u02b1\5\u00dbn\2\u02b1\u02b2\5\u00e5s\2\u02b2"+
		"\u02b5\3\2\2\2\u02b3\u02b5\7#\2\2\u02b4\u02af\3\2\2\2\u02b4\u02b3\3\2"+
		"\2\2\u02b5|\3\2\2\2\u02b6\u02b7\5\u00cfh\2\u02b7\u02b8\5\u00cbf\2\u02b8"+
		"\u02b9\5\u00d9m\2\u02b9\u02ba\5\u00dbn\2\u02ba\u02bb\5\u00e1q\2\u02bb"+
		"\u02bc\5\u00c7d\2\u02bc~\3\2\2\2\u02bd\u02be\5\u00e5s\2\u02be\u02bf\5"+
		"\u00e1q\2\u02bf\u02c0\5\u00e7t\2\u02c0\u02c1\5\u00c7d\2\u02c1\u0080\3"+
		"\2\2\2\u02c2\u02c3\5\u00c9e\2\u02c3\u02c4\5\u00bf`\2\u02c4\u02c5\5\u00d5"+
		"k\2\u02c5\u02c6\5\u00e3r\2\u02c6\u02c7\5\u00c7d\2\u02c7\u0082\3\2\2\2"+
		"\u02c8\u02c9\7,\2\2\u02c9\u0084\3\2\2\2\u02ca\u02cb\7\61\2\2\u02cb\u0086"+
		"\3\2\2\2\u02cc\u02cd\7-\2\2\u02cd\u0088\3\2\2\2\u02ce\u02cf\7/\2\2\u02cf"+
		"\u008a\3\2\2\2\u02d0\u02d1\5\u00c3b\2\u02d1\u02d2\5\u00dbn\2\u02d2\u02d3"+
		"\5\u00e7t\2\u02d3\u02d4\5\u00d9m\2\u02d4\u02d5\5\u00e5s\2\u02d5\u008c"+
		"\3\2\2\2\u02d6\u02d7\5\u00e3r\2\u02d7\u02d8\5\u00e7t\2\u02d8\u02d9\5\u00d7"+
		"l\2\u02d9\u008e\3\2\2\2\u02da\u02db\5\u00d7l\2\u02db\u02dc\5\u00cfh\2"+
		"\u02dc\u02dd\5\u00d9m\2\u02dd\u0090\3\2\2\2\u02de\u02df\5\u00d7l\2\u02df"+
		"\u02e0\5\u00bf`\2\u02e0\u02e1\5\u00edw\2\u02e1\u0092\3\2\2\2\u02e2\u02e3"+
		"\5\u00bf`\2\u02e3\u02e4\5\u00e9u\2\u02e4\u02e5\5\u00c7d\2\u02e5\u02e6"+
		"\5\u00e1q\2\u02e6\u02e7\5\u00bf`\2\u02e7\u02e8\5\u00cbf\2\u02e8\u02e9"+
		"\5\u00c7d\2\u02e9\u0094\3\2\2\2\u02ea\u02eb\5\u00d7l\2\u02eb\u02ec\5\u00db"+
		"n\2\u02ec\u02ed\5\u00c5c\2\u02ed\u02ee\5\u00c7d\2\u02ee\u0096\3\2\2\2"+
		"\u02ef\u02f0\5\u00d7l\2\u02f0\u02f1\5\u00c7d\2\u02f1\u02f2\5\u00bf`\2"+
		"\u02f2\u02f3\5\u00d9m\2\u02f3\u0098\3\2\2\2\u02f4\u02f5\5\u00d7l\2\u02f5"+
		"\u02f6\5\u00c7d\2\u02f6\u02f7\5\u00c5c\2\u02f7\u02f8\5\u00cfh\2\u02f8"+
		"\u02f9\5\u00bf`\2\u02f9\u02fa\5\u00d9m\2\u02fa\u009a\3\2\2\2\u02fb\u02fc"+
		"\7?\2\2\u02fc\u009c\3\2\2\2\u02fd\u02fe\7*\2\2\u02fe\u009e\3\2\2\2\u02ff"+
		"\u0300\7+\2\2\u0300\u00a0\3\2\2\2\u0301\u0302\7]\2\2\u0302\u00a2\3\2\2"+
		"\2\u0303\u0304\7_\2\2\u0304\u00a4\3\2\2\2\u0305\u0306\7}\2\2\u0306\u00a6"+
		"\3\2\2\2\u0307\u0308\7\177\2\2\u0308\u00a8\3\2\2\2\u0309\u030a\t\3\2\2"+
		"\u030a\u030b\t\3\2\2\u030b\u030c\7\60\2\2\u030c\u030d\t\3\2\2\u030d\u030e"+
		"\t\3\2\2\u030e\u030f\7\60\2\2\u030f\u0310\t\3\2\2\u0310\u0311\t\3\2\2"+
		"\u0311\u0312\t\3\2\2\u0312\u0313\t\3\2\2\u0313\u0314\7\"\2\2\u0314\u0315"+
		"\t\3\2\2\u0315\u0316\t\3\2\2\u0316\u0317\7<\2\2\u0317\u0318\t\3\2\2\u0318"+
		"\u0319\t\3\2\2\u0319\u031a\7<\2\2\u031a\u031b\t\3\2\2\u031b\u037a\t\3"+
		"\2\2\u031c\u031d\t\3\2\2\u031d\u031e\t\3\2\2\u031e\u031f\7\60\2\2\u031f"+
		"\u0320\t\3\2\2\u0320\u0321\t\3\2\2\u0321\u0322\7\60\2\2\u0322\u0323\t"+
		"\3\2\2\u0323\u0324\t\3\2\2\u0324\u0325\t\3\2\2\u0325\u037a\t\3\2\2\u0326"+
		"\u0327\t\3\2\2\u0327\u0328\t\3\2\2\u0328\u0329\t\3\2\2\u0329\u032a\t\3"+
		"\2\2\u032a\u032b\7/\2\2\u032b\u032c\t\3\2\2\u032c\u032d\t\3\2\2\u032d"+
		"\u032e\7/\2\2\u032e\u032f\t\3\2\2\u032f\u0330\t\3\2\2\u0330\u0331\7\""+
		"\2\2\u0331\u0332\t\3\2\2\u0332\u0333\t\3\2\2\u0333\u0334\7<\2\2\u0334"+
		"\u0335\t\3\2\2\u0335\u0336\t\3\2\2\u0336\u0337\7<\2\2\u0337\u0338\t\3"+
		"\2\2\u0338\u037a\t\3\2\2\u0339\u033a\t\3\2\2\u033a\u033b\t\3\2\2\u033b"+
		"\u033c\t\3\2\2\u033c\u033d\t\3\2\2\u033d\u033e\7/\2\2\u033e\u033f\t\3"+
		"\2\2\u033f\u0340\t\3\2\2\u0340\u0341\7/\2\2\u0341\u0342\t\3\2\2\u0342"+
		"\u037a\t\3\2\2\u0343\u0344\t\3\2\2\u0344\u0345\t\3\2\2\u0345\u0346\t\3"+
		"\2\2\u0346\u0347\t\3\2\2\u0347\u0348\t\3\2\2\u0348\u0349\t\3\2\2\u0349"+
		"\u034a\t\3\2\2\u034a\u034b\t\3\2\2\u034b\u034c\7\"\2\2\u034c\u034d\t\3"+
		"\2\2\u034d\u034e\t\3\2\2\u034e\u034f\7<\2\2\u034f\u0350\t\3\2\2\u0350"+
		"\u0351\t\3\2\2\u0351\u0352\7<\2\2\u0352\u0353\t\3\2\2\u0353\u037a\t\3"+
		"\2\2\u0354\u0355\t\3\2\2\u0355\u0356\t\3\2\2\u0356\u0357\t\3\2\2\u0357"+
		"\u0358\t\3\2\2\u0358\u0359\t\3\2\2\u0359\u035a\t\3\2\2\u035a\u035b\t\3"+
		"\2\2\u035b\u037a\t\3\2\2\u035c\u035d\t\3\2\2\u035d\u035e\t\3\2\2\u035e"+
		"\u035f\t\3\2\2\u035f\u0360\t\3\2\2\u0360\u0361\7\60\2\2\u0361\u0362\t"+
		"\3\2\2\u0362\u0363\t\3\2\2\u0363\u0364\7\60\2\2\u0364\u0365\t\3\2\2\u0365"+
		"\u0366\t\3\2\2\u0366\u0367\7\"\2\2\u0367\u0368\t\3\2\2\u0368\u0369\t\3"+
		"\2\2\u0369\u036a\7<\2\2\u036a\u036b\t\3\2\2\u036b\u036c\t\3\2\2\u036c"+
		"\u036d\7<\2\2\u036d\u036e\t\3\2\2\u036e\u037a\t\3\2\2\u036f\u0370\t\3"+
		"\2\2\u0370\u0371\t\3\2\2\u0371\u0372\t\3\2\2\u0372\u0373\t\3\2\2\u0373"+
		"\u0374\7\60\2\2\u0374\u0375\t\3\2\2\u0375\u0376\t\3\2\2\u0376\u0377\7"+
		"\60\2\2\u0377\u0378\t\3\2\2\u0378\u037a\t\3\2\2\u0379\u0309\3\2\2\2\u0379"+
		"\u031c\3\2\2\2\u0379\u0326\3\2\2\2\u0379\u0339\3\2\2\2\u0379\u0343\3\2"+
		"\2\2\u0379\u0354\3\2\2\2\u0379\u035c\3\2\2\2\u0379\u036f\3\2\2\2\u037a"+
		"\u00aa\3\2\2\2\u037b\u037d\t\3\2\2\u037c\u037b\3\2\2\2\u037d\u037e\3\2"+
		"\2\2\u037e\u037c\3\2\2\2\u037e\u037f\3\2\2\2\u037f\u00ac\3\2\2\2\u0380"+
		"\u0381\7.\2\2\u0381\u00ae\3\2\2\2\u0382\u0383\7\60\2\2\u0383\u00b0\3\2"+
		"\2\2\u0384\u0386\t\4\2\2\u0385\u0384\3\2\2\2\u0386\u0387\3\2\2\2\u0387"+
		"\u0385\3\2\2\2\u0387\u0388\3\2\2\2\u0388\u00b2\3\2\2\2\u0389\u038d\t\4"+
		"\2\2\u038a\u038c\t\5\2\2\u038b\u038a\3\2\2\2\u038c\u038f\3\2\2\2\u038d"+
		"\u038b\3\2\2\2\u038d\u038e\3\2\2\2\u038e\u00b4\3\2\2\2\u038f\u038d\3\2"+
		"\2\2\u0390\u0392\t\6\2\2\u0391\u0390\3\2\2\2\u0392\u0393\3\2\2\2\u0393"+
		"\u0391\3\2\2\2\u0393\u0394\3\2\2\2\u0394\u0395\3\2\2\2\u0395\u0396\b["+
		"\2\2\u0396\u00b6\3\2\2\2\u0397\u0398\7,\2\2\u0398\u00b8\3\2\2\2\u0399"+
		"\u039a\7)\2\2\u039a\u00ba\3\2\2\2\u039b\u039c\7^\2\2\u039c\u00bc\3\2\2"+
		"\2\u039d\u039e\7$\2\2\u039e\u00be\3\2\2\2\u039f\u03a0\t\7\2\2\u03a0\u00c0"+
		"\3\2\2\2\u03a1\u03a2\t\b\2\2\u03a2\u00c2\3\2\2\2\u03a3\u03a4\t\t\2\2\u03a4"+
		"\u00c4\3\2\2\2\u03a5\u03a6\t\n\2\2\u03a6\u00c6\3\2\2\2\u03a7\u03a8\t\13"+
		"\2\2\u03a8\u00c8\3\2\2\2\u03a9\u03aa\t\f\2\2\u03aa\u00ca\3\2\2\2\u03ab"+
		"\u03ac\t\r\2\2\u03ac\u00cc\3\2\2\2\u03ad\u03ae\t\16\2\2\u03ae\u00ce\3"+
		"\2\2\2\u03af\u03b0\t\17\2\2\u03b0\u00d0\3\2\2\2\u03b1\u03b2\t\20\2\2\u03b2"+
		"\u00d2\3\2\2\2\u03b3\u03b4\t\21\2\2\u03b4\u00d4\3\2\2\2\u03b5\u03b6\t"+
		"\22\2\2\u03b6\u00d6\3\2\2\2\u03b7\u03b8\t\23\2\2\u03b8\u00d8\3\2\2\2\u03b9"+
		"\u03ba\t\24\2\2\u03ba\u00da\3\2\2\2\u03bb\u03bc\t\25\2\2\u03bc\u00dc\3"+
		"\2\2\2\u03bd\u03be\t\26\2\2\u03be\u00de\3\2\2\2\u03bf\u03c0\t\27\2\2\u03c0"+
		"\u00e0\3\2\2\2\u03c1\u03c2\t\30\2\2\u03c2\u00e2\3\2\2\2\u03c3\u03c4\t"+
		"\31\2\2\u03c4\u00e4\3\2\2\2\u03c5\u03c6\t\32\2\2\u03c6\u00e6\3\2\2\2\u03c7"+
		"\u03c8\t\33\2\2\u03c8\u00e8\3\2\2\2\u03c9\u03ca\t\34\2\2\u03ca\u00ea\3"+
		"\2\2\2\u03cb\u03cc\t\35\2\2\u03cc\u00ec\3\2\2\2\u03cd\u03ce\t\36\2\2\u03ce"+
		"\u00ee\3\2\2\2\u03cf\u03d0\t\37\2\2\u03d0\u00f0\3\2\2\2\u03d1\u03d2\t"+
		" \2\2\u03d2\u00f2\3\2\2\2\22\2\u00f6\u00ff\u0102\u0104\u0115\u011e\u022e"+
		"\u02a5\u02ad\u02b4\u0379\u037e\u0387\u038d\u0393";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}