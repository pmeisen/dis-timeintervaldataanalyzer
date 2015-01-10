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
		PROP_BULKLOAD=26, TYPE_TIMESERIES=27, TYPE_RECORDS=28, TYPE_MODELS=29, 
		TYPE_MODEL=30, TYPE_VERSION=31, TYPE_PERMISSIONS=32, TYPE_ROLES=33, TYPE_USERS=34, 
		TYPE_PERMISSION=35, TYPE_ROLE=36, TYPE_USER=37, OP_FROM=38, OP_OF=39, 
		OP_ON=40, OP_TO=41, OP_IN=42, OP_INTO=43, OP_SET=44, OP_VALUES=45, OP_ALIAS=46, 
		OP_GROUPBY=47, OP_FILTERBY=48, OP_TRANSPOSE=49, OP_IDONLY=50, OP_WITH=51, 
		IR_EQUALTO=52, IR_BEFORE=53, IR_AFTER=54, IR_MEETING=55, IR_OVERLAPPING=56, 
		IR_DURING=57, IR_WITHIN=58, IR_CONTAINING=59, IR_STARTINGWITH=60, IR_FINISHINGWITH=61, 
		LOGICAL_OR=62, LOGICAL_AND=63, LOGICAL_NOT=64, LOGICAL_IGNORE=65, LOGICAL_TRUE=66, 
		LOGICAL_FALSE=67, MATH_MULTIPLY=68, MATH_DIVISION=69, MATH_PLUS=70, MATH_MINUS=71, 
		AGGR_COUNTSTARTED=72, AGGR_COUNTFINISHED=73, AGGR_COUNT=74, AGGR_SUM=75, 
		AGGR_MIN=76, AGGR_MAX=77, AGGR_AVERAGE=78, AGGR_MODE=79, AGGR_MEAN=80, 
		AGGR_MEDIAN=81, CMP_EQUAL=82, BRACKET_ROUND_OPENED=83, BRACKET_ROUND_CLOSED=84, 
		BRACKET_SQUARE_OPENED=85, BRACKET_SQUARE_CLOSED=86, BRACKET_CURLY_OPENED=87, 
		BRACKET_CURLY_CLOSED=88, DATE=89, INT=90, SEPARATOR=91, DIMSEPARATOR=92, 
		SIMPLE_ID=93, ENHANCED_ID=94, WHITESPACE=95;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] tokenNames = {
		"<INVALID>",
		"MARKED_ID", "VALUE", "NULL_VALUE", "POS_START_INCL", "POS_END_INCL", 
		"POS_START_EXCL", "POS_END_EXCL", "STMT_GET", "STMT_SELECT", "STMT_INSERT", 
		"STMT_DELETE", "STMT_OPEN", "STMT_LOAD", "STMT_UNLOAD", "STMT_ALIVE", 
		"STMT_ADD", "STMT_DROP", "STMT_MODIFY", "STMT_GRANT", "STMT_REVOKE", "STMT_ASSIGN", 
		"STMT_REMOVE", "PROP_AUTOLOAD", "PROP_FORCE", "PROP_PASSWORD", "PROP_BULKLOAD", 
		"TYPE_TIMESERIES", "TYPE_RECORDS", "TYPE_MODELS", "TYPE_MODEL", "TYPE_VERSION", 
		"TYPE_PERMISSIONS", "TYPE_ROLES", "TYPE_USERS", "TYPE_PERMISSION", "TYPE_ROLE", 
		"TYPE_USER", "OP_FROM", "OP_OF", "OP_ON", "OP_TO", "OP_IN", "OP_INTO", 
		"OP_SET", "OP_VALUES", "OP_ALIAS", "OP_GROUPBY", "OP_FILTERBY", "OP_TRANSPOSE", 
		"OP_IDONLY", "OP_WITH", "IR_EQUALTO", "IR_BEFORE", "IR_AFTER", "IR_MEETING", 
		"IR_OVERLAPPING", "IR_DURING", "IR_WITHIN", "IR_CONTAINING", "IR_STARTINGWITH", 
		"IR_FINISHINGWITH", "LOGICAL_OR", "LOGICAL_AND", "LOGICAL_NOT", "LOGICAL_IGNORE", 
		"LOGICAL_TRUE", "LOGICAL_FALSE", "'*'", "'/'", "'+'", "'-'", "AGGR_COUNTSTARTED", 
		"AGGR_COUNTFINISHED", "AGGR_COUNT", "AGGR_SUM", "AGGR_MIN", "AGGR_MAX", 
		"AGGR_AVERAGE", "AGGR_MODE", "AGGR_MEAN", "AGGR_MEDIAN", "'='", "'('", 
		"')'", "'['", "']'", "'{'", "'}'", "DATE", "INT", "','", "'.'", "SIMPLE_ID", 
		"ENHANCED_ID", "WHITESPACE"
	};
	public static final String[] ruleNames = {
		"MARKED_ID", "VALUE", "NULL_VALUE", "POS_START_INCL", "POS_END_INCL", 
		"POS_START_EXCL", "POS_END_EXCL", "STMT_GET", "STMT_SELECT", "STMT_INSERT", 
		"STMT_DELETE", "STMT_OPEN", "STMT_LOAD", "STMT_UNLOAD", "STMT_ALIVE", 
		"STMT_ADD", "STMT_DROP", "STMT_MODIFY", "STMT_GRANT", "STMT_REVOKE", "STMT_ASSIGN", 
		"STMT_REMOVE", "PROP_AUTOLOAD", "PROP_FORCE", "PROP_PASSWORD", "PROP_BULKLOAD", 
		"TYPE_TIMESERIES", "TYPE_RECORDS", "TYPE_MODELS", "TYPE_MODEL", "TYPE_VERSION", 
		"TYPE_PERMISSIONS", "TYPE_ROLES", "TYPE_USERS", "TYPE_PERMISSION", "TYPE_ROLE", 
		"TYPE_USER", "OP_FROM", "OP_OF", "OP_ON", "OP_TO", "OP_IN", "OP_INTO", 
		"OP_SET", "OP_VALUES", "OP_ALIAS", "OP_GROUPBY", "OP_FILTERBY", "OP_TRANSPOSE", 
		"OP_IDONLY", "OP_WITH", "IR_EQUALTO", "IR_BEFORE", "IR_AFTER", "IR_MEETING", 
		"IR_OVERLAPPING", "IR_DURING", "IR_WITHIN", "IR_CONTAINING", "IR_STARTINGWITH", 
		"IR_FINISHINGWITH", "LOGICAL_OR", "LOGICAL_AND", "LOGICAL_NOT", "LOGICAL_IGNORE", 
		"LOGICAL_TRUE", "LOGICAL_FALSE", "MATH_MULTIPLY", "MATH_DIVISION", "MATH_PLUS", 
		"MATH_MINUS", "AGGR_COUNTSTARTED", "AGGR_COUNTFINISHED", "AGGR_COUNT", 
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
		case 94: WHITESPACE_action((RuleContext)_localctx, actionIndex); break;
		}
	}
	private void WHITESPACE_action(RuleContext _localctx, int actionIndex) {
		switch (actionIndex) {
		case 0: skip();  break;
		}
	}

	public static final String _serializedATN =
		"\3\uacf5\uee8c\u4f5d\u8b0d\u4a45\u78bd\u1b2f\u3378\2a\u040a\b\1\4\2\t"+
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
		"w\tw\4x\tx\4y\ty\4z\tz\4{\t{\4|\t|\4}\t}\4~\t~\3\2\3\2\3\2\5\2\u0101\n"+
		"\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\5\3\u010a\n\3\3\3\7\3\u010d\n\3\f\3\16"+
		"\3\u0110\13\3\3\3\3\3\3\4\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\5\3\5\3\5"+
		"\5\5\u0120\n\5\3\5\3\5\3\6\3\6\3\6\3\6\3\6\5\6\u0129\n\6\3\6\3\6\3\7\3"+
		"\7\3\7\3\7\3\7\3\7\3\7\3\7\3\7\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\t\3\t\3\t"+
		"\3\t\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3"+
		"\f\3\f\3\f\3\f\3\f\3\f\3\f\3\r\3\r\3\r\3\r\3\r\3\16\3\16\3\16\3\16\3\16"+
		"\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\20\3\20\3\20\3\20\3\20\3\20\3\21"+
		"\3\21\3\21\3\21\3\22\3\22\3\22\3\22\3\22\3\23\3\23\3\23\3\23\3\23\3\23"+
		"\3\23\3\24\3\24\3\24\3\24\3\24\3\24\3\25\3\25\3\25\3\25\3\25\3\25\3\25"+
		"\3\26\3\26\3\26\3\26\3\26\3\26\3\26\3\27\3\27\3\27\3\27\3\27\3\27\3\27"+
		"\3\30\3\30\3\30\3\30\3\30\3\30\3\30\3\30\3\30\3\31\3\31\3\31\3\31\3\31"+
		"\3\31\3\32\3\32\3\32\3\32\3\32\3\32\3\32\3\32\3\32\3\33\3\33\3\33\3\33"+
		"\3\33\3\33\3\33\3\33\3\33\3\34\3\34\3\34\3\34\3\34\3\34\3\34\3\34\3\34"+
		"\3\34\3\34\3\35\3\35\3\35\3\35\3\35\3\35\3\35\3\35\3\36\3\36\3\36\3\36"+
		"\3\36\3\36\3\36\3\37\3\37\3\37\3\37\3\37\3\37\3 \3 \3 \3 \3 \3 \3 \3 "+
		"\3!\3!\3!\3!\3!\3!\3!\3!\3!\3!\3!\3!\3\"\3\"\3\"\3\"\3\"\3\"\3#\3#\3#"+
		"\3#\3#\3#\3$\3$\3$\3$\3$\3$\3$\3$\3$\3$\3$\3%\3%\3%\3%\3%\3&\3&\3&\3&"+
		"\3&\3\'\3\'\3\'\3\'\3\'\3(\3(\3(\3)\3)\3)\3*\3*\3*\3+\3+\3+\3,\3,\3,\3"+
		",\3,\3-\3-\3-\3-\3.\3.\3.\3.\3.\3.\3.\3/\3/\3/\3\60\3\60\3\60\3\60\3\60"+
		"\3\60\3\60\3\60\3\60\3\61\3\61\3\61\3\61\3\61\3\61\3\61\3\61\3\61\3\61"+
		"\3\61\3\61\3\61\3\61\3\61\3\61\5\61\u024b\n\61\3\62\3\62\3\62\3\62\3\62"+
		"\3\62\3\62\3\62\3\62\3\62\3\63\3\63\3\63\3\63\3\64\3\64\3\64\3\64\3\64"+
		"\3\65\3\65\3\65\3\65\3\65\3\65\3\65\3\65\3\66\3\66\3\66\3\66\3\66\3\66"+
		"\3\66\3\67\3\67\3\67\3\67\3\67\3\67\38\38\38\38\38\38\38\38\39\39\39\3"+
		"9\39\39\39\39\39\39\39\39\3:\3:\3:\3:\3:\3:\3:\3;\3;\3;\3;\3;\3;\3;\3"+
		"<\3<\3<\3<\3<\3<\3<\3<\3<\3<\3<\3=\3=\3=\3=\3=\3=\3=\3=\3=\3=\3=\3=\3"+
		"=\3>\3>\3>\3>\3>\3>\3>\3>\3>\3>\3>\3>\3>\3>\3?\3?\3?\3?\3?\5?\u02c2\n"+
		"?\3@\3@\3@\3@\3@\3@\5@\u02ca\n@\3A\3A\3A\3A\3A\5A\u02d1\nA\3B\3B\3B\3"+
		"B\3B\3B\3B\3C\3C\3C\3C\3C\3D\3D\3D\3D\3D\3D\3E\3E\3F\3F\3G\3G\3H\3H\3"+
		"I\3I\3I\3I\3I\3I\3I\3I\3I\3I\3I\3I\3I\3J\3J\3J\3J\3J\3J\3J\3J\3J\3J\3"+
		"J\3J\3J\3J\3K\3K\3K\3K\3K\3K\3L\3L\3L\3L\3M\3M\3M\3M\3N\3N\3N\3N\3O\3"+
		"O\3O\3O\3O\3O\3O\3O\3P\3P\3P\3P\3P\3Q\3Q\3Q\3Q\3Q\3R\3R\3R\3R\3R\3R\3"+
		"R\3S\3S\3T\3T\3U\3U\3V\3V\3W\3W\3X\3X\3Y\3Y\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3"+
		"Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3"+
		"Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3"+
		"Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3"+
		"Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3"+
		"Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\3Z\5Z\u03b1\nZ\3[\6[\u03b4\n[\r[\16[\u03b5"+
		"\3\\\3\\\3]\3]\3^\6^\u03bd\n^\r^\16^\u03be\3_\3_\7_\u03c3\n_\f_\16_\u03c6"+
		"\13_\3`\6`\u03c9\n`\r`\16`\u03ca\3`\3`\3a\3a\3b\3b\3c\3c\3d\3d\3e\3e\3"+
		"f\3f\3g\3g\3h\3h\3i\3i\3j\3j\3k\3k\3l\3l\3m\3m\3n\3n\3o\3o\3p\3p\3q\3"+
		"q\3r\3r\3s\3s\3t\3t\3u\3u\3v\3v\3w\3w\3x\3x\3y\3y\3z\3z\3{\3{\3|\3|\3"+
		"}\3}\3~\3~\3\u010e\177\3\3\1\5\4\1\7\5\1\t\6\1\13\7\1\r\b\1\17\t\1\21"+
		"\n\1\23\13\1\25\f\1\27\r\1\31\16\1\33\17\1\35\20\1\37\21\1!\22\1#\23\1"+
		"%\24\1\'\25\1)\26\1+\27\1-\30\1/\31\1\61\32\1\63\33\1\65\34\1\67\35\1"+
		"9\36\1;\37\1= \1?!\1A\"\1C#\1E$\1G%\1I&\1K\'\1M(\1O)\1Q*\1S+\1U,\1W-\1"+
		"Y.\1[/\1]\60\1_\61\1a\62\1c\63\1e\64\1g\65\1i\66\1k\67\1m8\1o9\1q:\1s"+
		";\1u<\1w=\1y>\1{?\1}@\1\177A\1\u0081B\1\u0083C\1\u0085D\1\u0087E\1\u0089"+
		"F\1\u008bG\1\u008dH\1\u008fI\1\u0091J\1\u0093K\1\u0095L\1\u0097M\1\u0099"+
		"N\1\u009bO\1\u009dP\1\u009fQ\1\u00a1R\1\u00a3S\1\u00a5T\1\u00a7U\1\u00a9"+
		"V\1\u00abW\1\u00adX\1\u00afY\1\u00b1Z\1\u00b3[\1\u00b5\\\1\u00b7]\1\u00b9"+
		"^\1\u00bb_\1\u00bd`\1\u00bfa\2\u00c1\2\1\u00c3\2\1\u00c5\2\1\u00c7\2\1"+
		"\u00c9\2\1\u00cb\2\1\u00cd\2\1\u00cf\2\1\u00d1\2\1\u00d3\2\1\u00d5\2\1"+
		"\u00d7\2\1\u00d9\2\1\u00db\2\1\u00dd\2\1\u00df\2\1\u00e1\2\1\u00e3\2\1"+
		"\u00e5\2\1\u00e7\2\1\u00e9\2\1\u00eb\2\1\u00ed\2\1\u00ef\2\1\u00f1\2\1"+
		"\u00f3\2\1\u00f5\2\1\u00f7\2\1\u00f9\2\1\u00fb\2\1\3\2!\4\2))^^\3\2\62"+
		";\4\2C\\c|\7\2//\62;C\\aac|\5\2\13\f\17\17\"\"\4\2CCcc\4\2DDdd\4\2EEe"+
		"e\4\2FFff\4\2GGgg\4\2HHhh\4\2IIii\4\2JJjj\4\2KKkk\4\2LLll\4\2MMmm\4\2"+
		"NNnn\4\2OOoo\4\2PPpp\4\2QQqq\4\2RRrr\4\2SSss\4\2TTtt\4\2UUuu\4\2VVvv\4"+
		"\2WWww\4\2XXxx\4\2YYyy\4\2ZZzz\4\2[[{{\4\2\\\\||\u0401\2\3\3\2\2\2\2\5"+
		"\3\2\2\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2"+
		"\2\21\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33"+
		"\3\2\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\2"+
		"\'\3\2\2\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2\2\2\61\3\2\2\2\2"+
		"\63\3\2\2\2\2\65\3\2\2\2\2\67\3\2\2\2\29\3\2\2\2\2;\3\2\2\2\2=\3\2\2\2"+
		"\2?\3\2\2\2\2A\3\2\2\2\2C\3\2\2\2\2E\3\2\2\2\2G\3\2\2\2\2I\3\2\2\2\2K"+
		"\3\2\2\2\2M\3\2\2\2\2O\3\2\2\2\2Q\3\2\2\2\2S\3\2\2\2\2U\3\2\2\2\2W\3\2"+
		"\2\2\2Y\3\2\2\2\2[\3\2\2\2\2]\3\2\2\2\2_\3\2\2\2\2a\3\2\2\2\2c\3\2\2\2"+
		"\2e\3\2\2\2\2g\3\2\2\2\2i\3\2\2\2\2k\3\2\2\2\2m\3\2\2\2\2o\3\2\2\2\2q"+
		"\3\2\2\2\2s\3\2\2\2\2u\3\2\2\2\2w\3\2\2\2\2y\3\2\2\2\2{\3\2\2\2\2}\3\2"+
		"\2\2\2\177\3\2\2\2\2\u0081\3\2\2\2\2\u0083\3\2\2\2\2\u0085\3\2\2\2\2\u0087"+
		"\3\2\2\2\2\u0089\3\2\2\2\2\u008b\3\2\2\2\2\u008d\3\2\2\2\2\u008f\3\2\2"+
		"\2\2\u0091\3\2\2\2\2\u0093\3\2\2\2\2\u0095\3\2\2\2\2\u0097\3\2\2\2\2\u0099"+
		"\3\2\2\2\2\u009b\3\2\2\2\2\u009d\3\2\2\2\2\u009f\3\2\2\2\2\u00a1\3\2\2"+
		"\2\2\u00a3\3\2\2\2\2\u00a5\3\2\2\2\2\u00a7\3\2\2\2\2\u00a9\3\2\2\2\2\u00ab"+
		"\3\2\2\2\2\u00ad\3\2\2\2\2\u00af\3\2\2\2\2\u00b1\3\2\2\2\2\u00b3\3\2\2"+
		"\2\2\u00b5\3\2\2\2\2\u00b7\3\2\2\2\2\u00b9\3\2\2\2\2\u00bb\3\2\2\2\2\u00bd"+
		"\3\2\2\2\2\u00bf\3\2\2\2\3\u00fd\3\2\2\2\5\u0104\3\2\2\2\7\u0113\3\2\2"+
		"\2\t\u0118\3\2\2\2\13\u0123\3\2\2\2\r\u012c\3\2\2\2\17\u0135\3\2\2\2\21"+
		"\u013c\3\2\2\2\23\u0140\3\2\2\2\25\u0147\3\2\2\2\27\u014e\3\2\2\2\31\u0155"+
		"\3\2\2\2\33\u015a\3\2\2\2\35\u015f\3\2\2\2\37\u0166\3\2\2\2!\u016c\3\2"+
		"\2\2#\u0170\3\2\2\2%\u0175\3\2\2\2\'\u017c\3\2\2\2)\u0182\3\2\2\2+\u0189"+
		"\3\2\2\2-\u0190\3\2\2\2/\u0197\3\2\2\2\61\u01a0\3\2\2\2\63\u01a6\3\2\2"+
		"\2\65\u01af\3\2\2\2\67\u01b8\3\2\2\29\u01c3\3\2\2\2;\u01cb\3\2\2\2=\u01d2"+
		"\3\2\2\2?\u01d8\3\2\2\2A\u01e0\3\2\2\2C\u01ec\3\2\2\2E\u01f2\3\2\2\2G"+
		"\u01f8\3\2\2\2I\u0203\3\2\2\2K\u0208\3\2\2\2M\u020d\3\2\2\2O\u0212\3\2"+
		"\2\2Q\u0215\3\2\2\2S\u0218\3\2\2\2U\u021b\3\2\2\2W\u021e\3\2\2\2Y\u0223"+
		"\3\2\2\2[\u0227\3\2\2\2]\u022e\3\2\2\2_\u0231\3\2\2\2a\u024a\3\2\2\2c"+
		"\u024c\3\2\2\2e\u0256\3\2\2\2g\u025a\3\2\2\2i\u025f\3\2\2\2k\u0267\3\2"+
		"\2\2m\u026e\3\2\2\2o\u0274\3\2\2\2q\u027c\3\2\2\2s\u0288\3\2\2\2u\u028f"+
		"\3\2\2\2w\u0296\3\2\2\2y\u02a1\3\2\2\2{\u02ae\3\2\2\2}\u02c1\3\2\2\2\177"+
		"\u02c9\3\2\2\2\u0081\u02d0\3\2\2\2\u0083\u02d2\3\2\2\2\u0085\u02d9\3\2"+
		"\2\2\u0087\u02de\3\2\2\2\u0089\u02e4\3\2\2\2\u008b\u02e6\3\2\2\2\u008d"+
		"\u02e8\3\2\2\2\u008f\u02ea\3\2\2\2\u0091\u02ec\3\2\2\2\u0093\u02f9\3\2"+
		"\2\2\u0095\u0307\3\2\2\2\u0097\u030d\3\2\2\2\u0099\u0311\3\2\2\2\u009b"+
		"\u0315\3\2\2\2\u009d\u0319\3\2\2\2\u009f\u0321\3\2\2\2\u00a1\u0326\3\2"+
		"\2\2\u00a3\u032b\3\2\2\2\u00a5\u0332\3\2\2\2\u00a7\u0334\3\2\2\2\u00a9"+
		"\u0336\3\2\2\2\u00ab\u0338\3\2\2\2\u00ad\u033a\3\2\2\2\u00af\u033c\3\2"+
		"\2\2\u00b1\u033e\3\2\2\2\u00b3\u03b0\3\2\2\2\u00b5\u03b3\3\2\2\2\u00b7"+
		"\u03b7\3\2\2\2\u00b9\u03b9\3\2\2\2\u00bb\u03bc\3\2\2\2\u00bd\u03c0\3\2"+
		"\2\2\u00bf\u03c8\3\2\2\2\u00c1\u03ce\3\2\2\2\u00c3\u03d0\3\2\2\2\u00c5"+
		"\u03d2\3\2\2\2\u00c7\u03d4\3\2\2\2\u00c9\u03d6\3\2\2\2\u00cb\u03d8\3\2"+
		"\2\2\u00cd\u03da\3\2\2\2\u00cf\u03dc\3\2\2\2\u00d1\u03de\3\2\2\2\u00d3"+
		"\u03e0\3\2\2\2\u00d5\u03e2\3\2\2\2\u00d7\u03e4\3\2\2\2\u00d9\u03e6\3\2"+
		"\2\2\u00db\u03e8\3\2\2\2\u00dd\u03ea\3\2\2\2\u00df\u03ec\3\2\2\2\u00e1"+
		"\u03ee\3\2\2\2\u00e3\u03f0\3\2\2\2\u00e5\u03f2\3\2\2\2\u00e7\u03f4\3\2"+
		"\2\2\u00e9\u03f6\3\2\2\2\u00eb\u03f8\3\2\2\2\u00ed\u03fa\3\2\2\2\u00ef"+
		"\u03fc\3\2\2\2\u00f1\u03fe\3\2\2\2\u00f3\u0400\3\2\2\2\u00f5\u0402\3\2"+
		"\2\2\u00f7\u0404\3\2\2\2\u00f9\u0406\3\2\2\2\u00fb\u0408\3\2\2\2\u00fd"+
		"\u0100\5\u00c7d\2\u00fe\u0101\5\u00bb^\2\u00ff\u0101\5\u00bd_\2\u0100"+
		"\u00fe\3\2\2\2\u0100\u00ff\3\2\2\2\u0101\u0102\3\2\2\2\u0102\u0103\5\u00c7"+
		"d\2\u0103\4\3\2\2\2\u0104\u010e\5\u00c3b\2\u0105\u0109\5\u00c5c\2\u0106"+
		"\u010a\5\u00c3b\2\u0107\u010a\5\u00c5c\2\u0108\u010a\5\u00c1a\2\u0109"+
		"\u0106\3\2\2\2\u0109\u0107\3\2\2\2\u0109\u0108\3\2\2\2\u010a\u010d\3\2"+
		"\2\2\u010b\u010d\n\2\2\2\u010c\u0105\3\2\2\2\u010c\u010b\3\2\2\2\u010d"+
		"\u0110\3\2\2\2\u010e\u010f\3\2\2\2\u010e\u010c\3\2\2\2\u010f\u0111\3\2"+
		"\2\2\u0110\u010e\3\2\2\2\u0111\u0112\5\u00c3b\2\u0112\6\3\2\2\2\u0113"+
		"\u0114\5\u00e3r\2\u0114\u0115\5\u00f1y\2\u0115\u0116\5\u00dfp\2\u0116"+
		"\u0117\5\u00dfp\2\u0117\b\3\2\2\2\u0118\u0119\5\u00abV\2\u0119\u011a\5"+
		"\u00edw\2\u011a\u011b\5\u00efx\2\u011b\u011c\5\u00c9e\2\u011c\u011d\5"+
		"\u00ebv\2\u011d\u011f\5\u00efx\2\u011e\u0120\7-\2\2\u011f\u011e\3\2\2"+
		"\2\u011f\u0120\3\2\2\2\u0120\u0121\3\2\2\2\u0121\u0122\5\u00adW\2\u0122"+
		"\n\3\2\2\2\u0123\u0124\5\u00abV\2\u0124\u0125\5\u00d1i\2\u0125\u0126\5"+
		"\u00e3r\2\u0126\u0128\5\u00cfh\2\u0127\u0129\7-\2\2\u0128\u0127\3\2\2"+
		"\2\u0128\u0129\3\2\2\2\u0129\u012a\3\2\2\2\u012a\u012b\5\u00adW\2\u012b"+
		"\f\3\2\2\2\u012c\u012d\5\u00abV\2\u012d\u012e\5\u00edw\2\u012e\u012f\5"+
		"\u00efx\2\u012f\u0130\5\u00c9e\2\u0130\u0131\5\u00ebv\2\u0131\u0132\5"+
		"\u00efx\2\u0132\u0133\7/\2\2\u0133\u0134\5\u00adW\2\u0134\16\3\2\2\2\u0135"+
		"\u0136\5\u00abV\2\u0136\u0137\5\u00d1i\2\u0137\u0138\5\u00e3r\2\u0138"+
		"\u0139\5\u00cfh\2\u0139\u013a\7/\2\2\u013a\u013b\5\u00adW\2\u013b\20\3"+
		"\2\2\2\u013c\u013d\5\u00d5k\2\u013d\u013e\5\u00d1i\2\u013e\u013f\5\u00ef"+
		"x\2\u013f\22\3\2\2\2\u0140\u0141\5\u00edw\2\u0141\u0142\5\u00d1i\2\u0142"+
		"\u0143\5\u00dfp\2\u0143\u0144\5\u00d1i\2\u0144\u0145\5\u00cdg\2\u0145"+
		"\u0146\5\u00efx\2\u0146\24\3\2\2\2\u0147\u0148\5\u00d9m\2\u0148\u0149"+
		"\5\u00e3r\2\u0149\u014a\5\u00edw\2\u014a\u014b\5\u00d1i\2\u014b\u014c"+
		"\5\u00ebv\2\u014c\u014d\5\u00efx\2\u014d\26\3\2\2\2\u014e\u014f\5\u00cf"+
		"h\2\u014f\u0150\5\u00d1i\2\u0150\u0151\5\u00dfp\2\u0151\u0152\5\u00d1"+
		"i\2\u0152\u0153\5\u00efx\2\u0153\u0154\5\u00d1i\2\u0154\30\3\2\2\2\u0155"+
		"\u0156\5\u00e5s\2\u0156\u0157\5\u00e7t\2\u0157\u0158\5\u00d1i\2\u0158"+
		"\u0159\5\u00e3r\2\u0159\32\3\2\2\2\u015a\u015b\5\u00dfp\2\u015b\u015c"+
		"\5\u00e5s\2\u015c\u015d\5\u00c9e\2\u015d\u015e\5\u00cfh\2\u015e\34\3\2"+
		"\2\2\u015f\u0160\5\u00f1y\2\u0160\u0161\5\u00e3r\2\u0161\u0162\5\u00df"+
		"p\2\u0162\u0163\5\u00e5s\2\u0163\u0164\5\u00c9e\2\u0164\u0165\5\u00cf"+
		"h\2\u0165\36\3\2\2\2\u0166\u0167\5\u00c9e\2\u0167\u0168\5\u00dfp\2\u0168"+
		"\u0169\5\u00d9m\2\u0169\u016a\5\u00f3z\2\u016a\u016b\5\u00d1i\2\u016b"+
		" \3\2\2\2\u016c\u016d\5\u00c9e\2\u016d\u016e\5\u00cfh\2\u016e\u016f\5"+
		"\u00cfh\2\u016f\"\3\2\2\2\u0170\u0171\5\u00cfh\2\u0171\u0172\5\u00ebv"+
		"\2\u0172\u0173\5\u00e5s\2\u0173\u0174\5\u00e7t\2\u0174$\3\2\2\2\u0175"+
		"\u0176\5\u00e1q\2\u0176\u0177\5\u00e5s\2\u0177\u0178\5\u00cfh\2\u0178"+
		"\u0179\5\u00d9m\2\u0179\u017a\5\u00d3j\2\u017a\u017b\5\u00f9}\2\u017b"+
		"&\3\2\2\2\u017c\u017d\5\u00d5k\2\u017d\u017e\5\u00ebv\2\u017e\u017f\5"+
		"\u00c9e\2\u017f\u0180\5\u00e3r\2\u0180\u0181\5\u00efx\2\u0181(\3\2\2\2"+
		"\u0182\u0183\5\u00ebv\2\u0183\u0184\5\u00d1i\2\u0184\u0185\5\u00f3z\2"+
		"\u0185\u0186\5\u00e5s\2\u0186\u0187\5\u00ddo\2\u0187\u0188\5\u00d1i\2"+
		"\u0188*\3\2\2\2\u0189\u018a\5\u00c9e\2\u018a\u018b\5\u00edw\2\u018b\u018c"+
		"\5\u00edw\2\u018c\u018d\5\u00d9m\2\u018d\u018e\5\u00d5k\2\u018e\u018f"+
		"\5\u00e3r\2\u018f,\3\2\2\2\u0190\u0191\5\u00ebv\2\u0191\u0192\5\u00d1"+
		"i\2\u0192\u0193\5\u00e1q\2\u0193\u0194\5\u00e5s\2\u0194\u0195\5\u00f3"+
		"z\2\u0195\u0196\5\u00d1i\2\u0196.\3\2\2\2\u0197\u0198\5\u00c9e\2\u0198"+
		"\u0199\5\u00f1y\2\u0199\u019a\5\u00efx\2\u019a\u019b\5\u00e5s\2\u019b"+
		"\u019c\5\u00dfp\2\u019c\u019d\5\u00e5s\2\u019d\u019e\5\u00c9e\2\u019e"+
		"\u019f\5\u00cfh\2\u019f\60\3\2\2\2\u01a0\u01a1\5\u00d3j\2\u01a1\u01a2"+
		"\5\u00e5s\2\u01a2\u01a3\5\u00ebv\2\u01a3\u01a4\5\u00cdg\2\u01a4\u01a5"+
		"\5\u00d1i\2\u01a5\62\3\2\2\2\u01a6\u01a7\5\u00e7t\2\u01a7\u01a8\5\u00c9"+
		"e\2\u01a8\u01a9\5\u00edw\2\u01a9\u01aa\5\u00edw\2\u01aa\u01ab\5\u00f5"+
		"{\2\u01ab\u01ac\5\u00e5s\2\u01ac\u01ad\5\u00ebv\2\u01ad\u01ae\5\u00cf"+
		"h\2\u01ae\64\3\2\2\2\u01af\u01b0\5\u00cbf\2\u01b0\u01b1\5\u00f1y\2\u01b1"+
		"\u01b2\5\u00dfp\2\u01b2\u01b3\5\u00ddo\2\u01b3\u01b4\5\u00dfp\2\u01b4"+
		"\u01b5\5\u00e5s\2\u01b5\u01b6\5\u00c9e\2\u01b6\u01b7\5\u00cfh\2\u01b7"+
		"\66\3\2\2\2\u01b8\u01b9\5\u00efx\2\u01b9\u01ba\5\u00d9m\2\u01ba\u01bb"+
		"\5\u00e1q\2\u01bb\u01bc\5\u00d1i\2\u01bc\u01bd\5\u00edw\2\u01bd\u01be"+
		"\5\u00d1i\2\u01be\u01bf\5\u00ebv\2\u01bf\u01c0\5\u00d9m\2\u01c0\u01c1"+
		"\5\u00d1i\2\u01c1\u01c2\5\u00edw\2\u01c28\3\2\2\2\u01c3\u01c4\5\u00eb"+
		"v\2\u01c4\u01c5\5\u00d1i\2\u01c5\u01c6\5\u00cdg\2\u01c6\u01c7\5\u00e5"+
		"s\2\u01c7\u01c8\5\u00ebv\2\u01c8\u01c9\5\u00cfh\2\u01c9\u01ca\5\u00ed"+
		"w\2\u01ca:\3\2\2\2\u01cb\u01cc\5\u00e1q\2\u01cc\u01cd\5\u00e5s\2\u01cd"+
		"\u01ce\5\u00cfh\2\u01ce\u01cf\5\u00d1i\2\u01cf\u01d0\5\u00dfp\2\u01d0"+
		"\u01d1\5\u00edw\2\u01d1<\3\2\2\2\u01d2\u01d3\5\u00e1q\2\u01d3\u01d4\5"+
		"\u00e5s\2\u01d4\u01d5\5\u00cfh\2\u01d5\u01d6\5\u00d1i\2\u01d6\u01d7\5"+
		"\u00dfp\2\u01d7>\3\2\2\2\u01d8\u01d9\5\u00f3z\2\u01d9\u01da\5\u00d1i\2"+
		"\u01da\u01db\5\u00ebv\2\u01db\u01dc\5\u00edw\2\u01dc\u01dd\5\u00d9m\2"+
		"\u01dd\u01de\5\u00e5s\2\u01de\u01df\5\u00e3r\2\u01df@\3\2\2\2\u01e0\u01e1"+
		"\5\u00e7t\2\u01e1\u01e2\5\u00d1i\2\u01e2\u01e3\5\u00ebv\2\u01e3\u01e4"+
		"\5\u00e1q\2\u01e4\u01e5\5\u00d9m\2\u01e5\u01e6\5\u00edw\2\u01e6\u01e7"+
		"\5\u00edw\2\u01e7\u01e8\5\u00d9m\2\u01e8\u01e9\5\u00e5s\2\u01e9\u01ea"+
		"\5\u00e3r\2\u01ea\u01eb\5\u00edw\2\u01ebB\3\2\2\2\u01ec\u01ed\5\u00eb"+
		"v\2\u01ed\u01ee\5\u00e5s\2\u01ee\u01ef\5\u00dfp\2\u01ef\u01f0\5\u00d1"+
		"i\2\u01f0\u01f1\5\u00edw\2\u01f1D\3\2\2\2\u01f2\u01f3\5\u00f1y\2\u01f3"+
		"\u01f4\5\u00edw\2\u01f4\u01f5\5\u00d1i\2\u01f5\u01f6\5\u00ebv\2\u01f6"+
		"\u01f7\5\u00edw\2\u01f7F\3\2\2\2\u01f8\u01f9\5\u00e7t\2\u01f9\u01fa\5"+
		"\u00d1i\2\u01fa\u01fb\5\u00ebv\2\u01fb\u01fc\5\u00e1q\2\u01fc\u01fd\5"+
		"\u00d9m\2\u01fd\u01fe\5\u00edw\2\u01fe\u01ff\5\u00edw\2\u01ff\u0200\5"+
		"\u00d9m\2\u0200\u0201\5\u00e5s\2\u0201\u0202\5\u00e3r\2\u0202H\3\2\2\2"+
		"\u0203\u0204\5\u00ebv\2\u0204\u0205\5\u00e5s\2\u0205\u0206\5\u00dfp\2"+
		"\u0206\u0207\5\u00d1i\2\u0207J\3\2\2\2\u0208\u0209\5\u00f1y\2\u0209\u020a"+
		"\5\u00edw\2\u020a\u020b\5\u00d1i\2\u020b\u020c\5\u00ebv\2\u020cL\3\2\2"+
		"\2\u020d\u020e\5\u00d3j\2\u020e\u020f\5\u00ebv\2\u020f\u0210\5\u00e5s"+
		"\2\u0210\u0211\5\u00e1q\2\u0211N\3\2\2\2\u0212\u0213\5\u00e5s\2\u0213"+
		"\u0214\5\u00d3j\2\u0214P\3\2\2\2\u0215\u0216\5\u00e5s\2\u0216\u0217\5"+
		"\u00e3r\2\u0217R\3\2\2\2\u0218\u0219\5\u00efx\2\u0219\u021a\5\u00e5s\2"+
		"\u021aT\3\2\2\2\u021b\u021c\5\u00d9m\2\u021c\u021d\5\u00e3r\2\u021dV\3"+
		"\2\2\2\u021e\u021f\5\u00d9m\2\u021f\u0220\5\u00e3r\2\u0220\u0221\5\u00ef"+
		"x\2\u0221\u0222\5\u00e5s\2\u0222X\3\2\2\2\u0223\u0224\5\u00edw\2\u0224"+
		"\u0225\5\u00d1i\2\u0225\u0226\5\u00efx\2\u0226Z\3\2\2\2\u0227\u0228\5"+
		"\u00f3z\2\u0228\u0229\5\u00c9e\2\u0229\u022a\5\u00dfp\2\u022a\u022b\5"+
		"\u00f1y\2\u022b\u022c\5\u00d1i\2\u022c\u022d\5\u00edw\2\u022d\\\3\2\2"+
		"\2\u022e\u022f\5\u00c9e\2\u022f\u0230\5\u00edw\2\u0230^\3\2\2\2\u0231"+
		"\u0232\5\u00d5k\2\u0232\u0233\5\u00ebv\2\u0233\u0234\5\u00e5s\2\u0234"+
		"\u0235\5\u00f1y\2\u0235\u0236\5\u00e7t\2\u0236\u0237\7\"\2\2\u0237\u0238"+
		"\5\u00cbf\2\u0238\u0239\5\u00f9}\2\u0239`\3\2\2\2\u023a\u023b\5\u00d3"+
		"j\2\u023b\u023c\5\u00d9m\2\u023c\u023d\5\u00dfp\2\u023d\u023e\5\u00ef"+
		"x\2\u023e\u023f\5\u00d1i\2\u023f\u0240\5\u00ebv\2\u0240\u0241\7\"\2\2"+
		"\u0241\u0242\5\u00cbf\2\u0242\u0243\5\u00f9}\2\u0243\u024b\3\2\2\2\u0244"+
		"\u0245\5\u00f5{\2\u0245\u0246\5\u00d7l\2\u0246\u0247\5\u00d1i\2\u0247"+
		"\u0248\5\u00ebv\2\u0248\u0249\5\u00d1i\2\u0249\u024b\3\2\2\2\u024a\u023a"+
		"\3\2\2\2\u024a\u0244\3\2\2\2\u024bb\3\2\2\2\u024c\u024d\5\u00efx\2\u024d"+
		"\u024e\5\u00ebv\2\u024e\u024f\5\u00c9e\2\u024f\u0250\5\u00e3r\2\u0250"+
		"\u0251\5\u00edw\2\u0251\u0252\5\u00e7t\2\u0252\u0253\5\u00e5s\2\u0253"+
		"\u0254\5\u00edw\2\u0254\u0255\5\u00d1i\2\u0255d\3\2\2\2\u0256\u0257\5"+
		"\u00d9m\2\u0257\u0258\5\u00cfh\2\u0258\u0259\5\u00edw\2\u0259f\3\2\2\2"+
		"\u025a\u025b\5\u00f5{\2\u025b\u025c\5\u00d9m\2\u025c\u025d\5\u00efx\2"+
		"\u025d\u025e\5\u00d7l\2\u025eh\3\2\2\2\u025f\u0260\5\u00d1i\2\u0260\u0261"+
		"\5\u00e9u\2\u0261\u0262\5\u00f1y\2\u0262\u0263\5\u00c9e\2\u0263\u0264"+
		"\5\u00dfp\2\u0264\u0265\5\u00efx\2\u0265\u0266\5\u00e5s\2\u0266j\3\2\2"+
		"\2\u0267\u0268\5\u00cbf\2\u0268\u0269\5\u00d1i\2\u0269\u026a\5\u00d3j"+
		"\2\u026a\u026b\5\u00e5s\2\u026b\u026c\5\u00ebv\2\u026c\u026d\5\u00d1i"+
		"\2\u026dl\3\2\2\2\u026e\u026f\5\u00c9e\2\u026f\u0270\5\u00d3j\2\u0270"+
		"\u0271\5\u00efx\2\u0271\u0272\5\u00d1i\2\u0272\u0273\5\u00ebv\2\u0273"+
		"n\3\2\2\2\u0274\u0275\5\u00e1q\2\u0275\u0276\5\u00d1i\2\u0276\u0277\5"+
		"\u00d1i\2\u0277\u0278\5\u00efx\2\u0278\u0279\5\u00d9m\2\u0279\u027a\5"+
		"\u00e3r\2\u027a\u027b\5\u00d5k\2\u027bp\3\2\2\2\u027c\u027d\5\u00e5s\2"+
		"\u027d\u027e\5\u00f3z\2\u027e\u027f\5\u00d1i\2\u027f\u0280\5\u00ebv\2"+
		"\u0280\u0281\5\u00dfp\2\u0281\u0282\5\u00c9e\2\u0282\u0283\5\u00e7t\2"+
		"\u0283\u0284\5\u00e7t\2\u0284\u0285\5\u00d9m\2\u0285\u0286\5\u00e3r\2"+
		"\u0286\u0287\5\u00d5k\2\u0287r\3\2\2\2\u0288\u0289\5\u00cfh\2\u0289\u028a"+
		"\5\u00f1y\2\u028a\u028b\5\u00ebv\2\u028b\u028c\5\u00d9m\2\u028c\u028d"+
		"\5\u00e3r\2\u028d\u028e\5\u00d5k\2\u028et\3\2\2\2\u028f\u0290\5\u00f5"+
		"{\2\u0290\u0291\5\u00d9m\2\u0291\u0292\5\u00efx\2\u0292\u0293\5\u00d7"+
		"l\2\u0293\u0294\5\u00d9m\2\u0294\u0295\5\u00e3r\2\u0295v\3\2\2\2\u0296"+
		"\u0297\5\u00cdg\2\u0297\u0298\5\u00e5s\2\u0298\u0299\5\u00e3r\2\u0299"+
		"\u029a\5\u00efx\2\u029a\u029b\5\u00c9e\2\u029b\u029c\5\u00d9m\2\u029c"+
		"\u029d\5\u00e3r\2\u029d\u029e\5\u00d9m\2\u029e\u029f\5\u00e3r\2\u029f"+
		"\u02a0\5\u00d5k\2\u02a0x\3\2\2\2\u02a1\u02a2\5\u00edw\2\u02a2\u02a3\5"+
		"\u00efx\2\u02a3\u02a4\5\u00c9e\2\u02a4\u02a5\5\u00ebv\2\u02a5\u02a6\5"+
		"\u00efx\2\u02a6\u02a7\5\u00d9m\2\u02a7\u02a8\5\u00e3r\2\u02a8\u02a9\5"+
		"\u00d5k\2\u02a9\u02aa\5\u00f5{\2\u02aa\u02ab\5\u00d9m\2\u02ab\u02ac\5"+
		"\u00efx\2\u02ac\u02ad\5\u00d7l\2\u02adz\3\2\2\2\u02ae\u02af\5\u00d3j\2"+
		"\u02af\u02b0\5\u00d9m\2\u02b0\u02b1\5\u00e3r\2\u02b1\u02b2\5\u00d9m\2"+
		"\u02b2\u02b3\5\u00edw\2\u02b3\u02b4\5\u00d7l\2\u02b4\u02b5\5\u00d9m\2"+
		"\u02b5\u02b6\5\u00e3r\2\u02b6\u02b7\5\u00d5k\2\u02b7\u02b8\5\u00f5{\2"+
		"\u02b8\u02b9\5\u00d9m\2\u02b9\u02ba\5\u00efx\2\u02ba\u02bb\5\u00d7l\2"+
		"\u02bb|\3\2\2\2\u02bc\u02bd\5\u00e5s\2\u02bd\u02be\5\u00ebv\2\u02be\u02c2"+
		"\3\2\2\2\u02bf\u02c0\7~\2\2\u02c0\u02c2\7~\2\2\u02c1\u02bc\3\2\2\2\u02c1"+
		"\u02bf\3\2\2\2\u02c2~\3\2\2\2\u02c3\u02c4\5\u00c9e\2\u02c4\u02c5\5\u00e3"+
		"r\2\u02c5\u02c6\5\u00cfh\2\u02c6\u02ca\3\2\2\2\u02c7\u02c8\7(\2\2\u02c8"+
		"\u02ca\7(\2\2\u02c9\u02c3\3\2\2\2\u02c9\u02c7\3\2\2\2\u02ca\u0080\3\2"+
		"\2\2\u02cb\u02cc\5\u00e3r\2\u02cc\u02cd\5\u00e5s\2\u02cd\u02ce\5\u00ef"+
		"x\2\u02ce\u02d1\3\2\2\2\u02cf\u02d1\7#\2\2\u02d0\u02cb\3\2\2\2\u02d0\u02cf"+
		"\3\2\2\2\u02d1\u0082\3\2\2\2\u02d2\u02d3\5\u00d9m\2\u02d3\u02d4\5\u00d5"+
		"k\2\u02d4\u02d5\5\u00e3r\2\u02d5\u02d6\5\u00e5s\2\u02d6\u02d7\5\u00eb"+
		"v\2\u02d7\u02d8\5\u00d1i\2\u02d8\u0084\3\2\2\2\u02d9\u02da\5\u00efx\2"+
		"\u02da\u02db\5\u00ebv\2\u02db\u02dc\5\u00f1y\2\u02dc\u02dd\5\u00d1i\2"+
		"\u02dd\u0086\3\2\2\2\u02de\u02df\5\u00d3j\2\u02df\u02e0\5\u00c9e\2\u02e0"+
		"\u02e1\5\u00dfp\2\u02e1\u02e2\5\u00edw\2\u02e2\u02e3\5\u00d1i\2\u02e3"+
		"\u0088\3\2\2\2\u02e4\u02e5\7,\2\2\u02e5\u008a\3\2\2\2\u02e6\u02e7\7\61"+
		"\2\2\u02e7\u008c\3\2\2\2\u02e8\u02e9\7-\2\2\u02e9\u008e\3\2\2\2\u02ea"+
		"\u02eb\7/\2\2\u02eb\u0090\3\2\2\2\u02ec\u02ed\5\u00cdg\2\u02ed\u02ee\5"+
		"\u00e5s\2\u02ee\u02ef\5\u00f1y\2\u02ef\u02f0\5\u00e3r\2\u02f0\u02f1\5"+
		"\u00efx\2\u02f1\u02f2\5\u00edw\2\u02f2\u02f3\5\u00efx\2\u02f3\u02f4\5"+
		"\u00c9e\2\u02f4\u02f5\5\u00ebv\2\u02f5\u02f6\5\u00efx\2\u02f6\u02f7\5"+
		"\u00d1i\2\u02f7\u02f8\5\u00cfh\2\u02f8\u0092\3\2\2\2\u02f9\u02fa\5\u00cd"+
		"g\2\u02fa\u02fb\5\u00e5s\2\u02fb\u02fc\5\u00f1y\2\u02fc\u02fd\5\u00e3"+
		"r\2\u02fd\u02fe\5\u00efx\2\u02fe\u02ff\5\u00d3j\2\u02ff\u0300\5\u00d9"+
		"m\2\u0300\u0301\5\u00e3r\2\u0301\u0302\5\u00d9m\2\u0302\u0303\5\u00ed"+
		"w\2\u0303\u0304\5\u00d7l\2\u0304\u0305\5\u00d1i\2\u0305\u0306\5\u00cf"+
		"h\2\u0306\u0094\3\2\2\2\u0307\u0308\5\u00cdg\2\u0308\u0309\5\u00e5s\2"+
		"\u0309\u030a\5\u00f1y\2\u030a\u030b\5\u00e3r\2\u030b\u030c\5\u00efx\2"+
		"\u030c\u0096\3\2\2\2\u030d\u030e\5\u00edw\2\u030e\u030f\5\u00f1y\2\u030f"+
		"\u0310\5\u00e1q\2\u0310\u0098\3\2\2\2\u0311\u0312\5\u00e1q\2\u0312\u0313"+
		"\5\u00d9m\2\u0313\u0314\5\u00e3r\2\u0314\u009a\3\2\2\2\u0315\u0316\5\u00e1"+
		"q\2\u0316\u0317\5\u00c9e\2\u0317\u0318\5\u00f7|\2\u0318\u009c\3\2\2\2"+
		"\u0319\u031a\5\u00c9e\2\u031a\u031b\5\u00f3z\2\u031b\u031c\5\u00d1i\2"+
		"\u031c\u031d\5\u00ebv\2\u031d\u031e\5\u00c9e\2\u031e\u031f\5\u00d5k\2"+
		"\u031f\u0320\5\u00d1i\2\u0320\u009e\3\2\2\2\u0321\u0322\5\u00e1q\2\u0322"+
		"\u0323\5\u00e5s\2\u0323\u0324\5\u00cfh\2\u0324\u0325\5\u00d1i\2\u0325"+
		"\u00a0\3\2\2\2\u0326\u0327\5\u00e1q\2\u0327\u0328\5\u00d1i\2\u0328\u0329"+
		"\5\u00c9e\2\u0329\u032a\5\u00e3r\2\u032a\u00a2\3\2\2\2\u032b\u032c\5\u00e1"+
		"q\2\u032c\u032d\5\u00d1i\2\u032d\u032e\5\u00cfh\2\u032e\u032f\5\u00d9"+
		"m\2\u032f\u0330\5\u00c9e\2\u0330\u0331\5\u00e3r\2\u0331\u00a4\3\2\2\2"+
		"\u0332\u0333\7?\2\2\u0333\u00a6\3\2\2\2\u0334\u0335\7*\2\2\u0335\u00a8"+
		"\3\2\2\2\u0336\u0337\7+\2\2\u0337\u00aa\3\2\2\2\u0338\u0339\7]\2\2\u0339"+
		"\u00ac\3\2\2\2\u033a\u033b\7_\2\2\u033b\u00ae\3\2\2\2\u033c\u033d\7}\2"+
		"\2\u033d\u00b0\3\2\2\2\u033e\u033f\7\177\2\2\u033f\u00b2\3\2\2\2\u0340"+
		"\u0341\t\3\2\2\u0341\u0342\t\3\2\2\u0342\u0343\7\60\2\2\u0343\u0344\t"+
		"\3\2\2\u0344\u0345\t\3\2\2\u0345\u0346\7\60\2\2\u0346\u0347\t\3\2\2\u0347"+
		"\u0348\t\3\2\2\u0348\u0349\t\3\2\2\u0349\u034a\t\3\2\2\u034a\u034b\7\""+
		"\2\2\u034b\u034c\t\3\2\2\u034c\u034d\t\3\2\2\u034d\u034e\7<\2\2\u034e"+
		"\u034f\t\3\2\2\u034f\u0350\t\3\2\2\u0350\u0351\7<\2\2\u0351\u0352\t\3"+
		"\2\2\u0352\u03b1\t\3\2\2\u0353\u0354\t\3\2\2\u0354\u0355\t\3\2\2\u0355"+
		"\u0356\7\60\2\2\u0356\u0357\t\3\2\2\u0357\u0358\t\3\2\2\u0358\u0359\7"+
		"\60\2\2\u0359\u035a\t\3\2\2\u035a\u035b\t\3\2\2\u035b\u035c\t\3\2\2\u035c"+
		"\u03b1\t\3\2\2\u035d\u035e\t\3\2\2\u035e\u035f\t\3\2\2\u035f\u0360\t\3"+
		"\2\2\u0360\u0361\t\3\2\2\u0361\u0362\7/\2\2\u0362\u0363\t\3\2\2\u0363"+
		"\u0364\t\3\2\2\u0364\u0365\7/\2\2\u0365\u0366\t\3\2\2\u0366\u0367\t\3"+
		"\2\2\u0367\u0368\7\"\2\2\u0368\u0369\t\3\2\2\u0369\u036a\t\3\2\2\u036a"+
		"\u036b\7<\2\2\u036b\u036c\t\3\2\2\u036c\u036d\t\3\2\2\u036d\u036e\7<\2"+
		"\2\u036e\u036f\t\3\2\2\u036f\u03b1\t\3\2\2\u0370\u0371\t\3\2\2\u0371\u0372"+
		"\t\3\2\2\u0372\u0373\t\3\2\2\u0373\u0374\t\3\2\2\u0374\u0375\7/\2\2\u0375"+
		"\u0376\t\3\2\2\u0376\u0377\t\3\2\2\u0377\u0378\7/\2\2\u0378\u0379\t\3"+
		"\2\2\u0379\u03b1\t\3\2\2\u037a\u037b\t\3\2\2\u037b\u037c\t\3\2\2\u037c"+
		"\u037d\t\3\2\2\u037d\u037e\t\3\2\2\u037e\u037f\t\3\2\2\u037f\u0380\t\3"+
		"\2\2\u0380\u0381\t\3\2\2\u0381\u0382\t\3\2\2\u0382\u0383\7\"\2\2\u0383"+
		"\u0384\t\3\2\2\u0384\u0385\t\3\2\2\u0385\u0386\7<\2\2\u0386\u0387\t\3"+
		"\2\2\u0387\u0388\t\3\2\2\u0388\u0389\7<\2\2\u0389\u038a\t\3\2\2\u038a"+
		"\u03b1\t\3\2\2\u038b\u038c\t\3\2\2\u038c\u038d\t\3\2\2\u038d\u038e\t\3"+
		"\2\2\u038e\u038f\t\3\2\2\u038f\u0390\t\3\2\2\u0390\u0391\t\3\2\2\u0391"+
		"\u0392\t\3\2\2\u0392\u03b1\t\3\2\2\u0393\u0394\t\3\2\2\u0394\u0395\t\3"+
		"\2\2\u0395\u0396\t\3\2\2\u0396\u0397\t\3\2\2\u0397\u0398\7\60\2\2\u0398"+
		"\u0399\t\3\2\2\u0399\u039a\t\3\2\2\u039a\u039b\7\60\2\2\u039b\u039c\t"+
		"\3\2\2\u039c\u039d\t\3\2\2\u039d\u039e\7\"\2\2\u039e\u039f\t\3\2\2\u039f"+
		"\u03a0\t\3\2\2\u03a0\u03a1\7<\2\2\u03a1\u03a2\t\3\2\2\u03a2\u03a3\t\3"+
		"\2\2\u03a3\u03a4\7<\2\2\u03a4\u03a5\t\3\2\2\u03a5\u03b1\t\3\2\2\u03a6"+
		"\u03a7\t\3\2\2\u03a7\u03a8\t\3\2\2\u03a8\u03a9\t\3\2\2\u03a9\u03aa\t\3"+
		"\2\2\u03aa\u03ab\7\60\2\2\u03ab\u03ac\t\3\2\2\u03ac\u03ad\t\3\2\2\u03ad"+
		"\u03ae\7\60\2\2\u03ae\u03af\t\3\2\2\u03af\u03b1\t\3\2\2\u03b0\u0340\3"+
		"\2\2\2\u03b0\u0353\3\2\2\2\u03b0\u035d\3\2\2\2\u03b0\u0370\3\2\2\2\u03b0"+
		"\u037a\3\2\2\2\u03b0\u038b\3\2\2\2\u03b0\u0393\3\2\2\2\u03b0\u03a6\3\2"+
		"\2\2\u03b1\u00b4\3\2\2\2\u03b2\u03b4\t\3\2\2\u03b3\u03b2\3\2\2\2\u03b4"+
		"\u03b5\3\2\2\2\u03b5\u03b3\3\2\2\2\u03b5\u03b6\3\2\2\2\u03b6\u00b6\3\2"+
		"\2\2\u03b7\u03b8\7.\2\2\u03b8\u00b8\3\2\2\2\u03b9\u03ba\7\60\2\2\u03ba"+
		"\u00ba\3\2\2\2\u03bb\u03bd\t\4\2\2\u03bc\u03bb\3\2\2\2\u03bd\u03be\3\2"+
		"\2\2\u03be\u03bc\3\2\2\2\u03be\u03bf\3\2\2\2\u03bf\u00bc\3\2\2\2\u03c0"+
		"\u03c4\t\4\2\2\u03c1\u03c3\t\5\2\2\u03c2\u03c1\3\2\2\2\u03c3\u03c6\3\2"+
		"\2\2\u03c4\u03c2\3\2\2\2\u03c4\u03c5\3\2\2\2\u03c5\u00be\3\2\2\2\u03c6"+
		"\u03c4\3\2\2\2\u03c7\u03c9\t\6\2\2\u03c8\u03c7\3\2\2\2\u03c9\u03ca\3\2"+
		"\2\2\u03ca\u03c8\3\2\2\2\u03ca\u03cb\3\2\2\2\u03cb\u03cc\3\2\2\2\u03cc"+
		"\u03cd\b`\2\2\u03cd\u00c0\3\2\2\2\u03ce\u03cf\7,\2\2\u03cf\u00c2\3\2\2"+
		"\2\u03d0\u03d1\7)\2\2\u03d1\u00c4\3\2\2\2\u03d2\u03d3\7^\2\2\u03d3\u00c6"+
		"\3\2\2\2\u03d4\u03d5\7$\2\2\u03d5\u00c8\3\2\2\2\u03d6\u03d7\t\7\2\2\u03d7"+
		"\u00ca\3\2\2\2\u03d8\u03d9\t\b\2\2\u03d9\u00cc\3\2\2\2\u03da\u03db\t\t"+
		"\2\2\u03db\u00ce\3\2\2\2\u03dc\u03dd\t\n\2\2\u03dd\u00d0\3\2\2\2\u03de"+
		"\u03df\t\13\2\2\u03df\u00d2\3\2\2\2\u03e0\u03e1\t\f\2\2\u03e1\u00d4\3"+
		"\2\2\2\u03e2\u03e3\t\r\2\2\u03e3\u00d6\3\2\2\2\u03e4\u03e5\t\16\2\2\u03e5"+
		"\u00d8\3\2\2\2\u03e6\u03e7\t\17\2\2\u03e7\u00da\3\2\2\2\u03e8\u03e9\t"+
		"\20\2\2\u03e9\u00dc\3\2\2\2\u03ea\u03eb\t\21\2\2\u03eb\u00de\3\2\2\2\u03ec"+
		"\u03ed\t\22\2\2\u03ed\u00e0\3\2\2\2\u03ee\u03ef\t\23\2\2\u03ef\u00e2\3"+
		"\2\2\2\u03f0\u03f1\t\24\2\2\u03f1\u00e4\3\2\2\2\u03f2\u03f3\t\25\2\2\u03f3"+
		"\u00e6\3\2\2\2\u03f4\u03f5\t\26\2\2\u03f5\u00e8\3\2\2\2\u03f6\u03f7\t"+
		"\27\2\2\u03f7\u00ea\3\2\2\2\u03f8\u03f9\t\30\2\2\u03f9\u00ec\3\2\2\2\u03fa"+
		"\u03fb\t\31\2\2\u03fb\u00ee\3\2\2\2\u03fc\u03fd\t\32\2\2\u03fd\u00f0\3"+
		"\2\2\2\u03fe\u03ff\t\33\2\2\u03ff\u00f2\3\2\2\2\u0400\u0401\t\34\2\2\u0401"+
		"\u00f4\3\2\2\2\u0402\u0403\t\35\2\2\u0403\u00f6\3\2\2\2\u0404\u0405\t"+
		"\36\2\2\u0405\u00f8\3\2\2\2\u0406\u0407\t\37\2\2\u0407\u00fa\3\2\2\2\u0408"+
		"\u0409\t \2\2\u0409\u00fc\3\2\2\2\22\2\u0100\u0109\u010c\u010e\u011f\u0128"+
		"\u024a\u02c1\u02c9\u02d0\u03b0\u03b5\u03be\u03c4\u03ca";
	public static final ATN _ATN =
		ATNSimulator.deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}