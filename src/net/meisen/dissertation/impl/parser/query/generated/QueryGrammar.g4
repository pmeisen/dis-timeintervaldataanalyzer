/*
 * This is the grammar defined to understand a properitary query
 * used to retrieve data from the TidaSystem. The code generation
 * is not triggered automatically and therefore the generated code
 * might not be "synced" to this file content.
 */
grammar QueryGrammar;

/*
 * This is my first grammar, therefore I added a section with some 
 * hints I thought might help.
 *  - parser rules start with lowercase letters, lexer rules with uppercase
 *  - ...
 */

options {
language = Java;
}

@parser::header {
package net.meisen.dissertation.impl.parser.query.generated;
}

@lexer::header {
package net.meisen.dissertation.impl.parser.query.generated;
}

/*
 * Define the different expressions/parts of the statement
 */
exprSelect   : STMT_SELECT selectorSelectType (OP_OF exprMeasure)? OP_FROM selectorModelId (OP_IN exprInterval)? (OP_FILTERBY exprComp)? (OP_GROUPBY exprGroup)? EOF;
exprMeasure  : compMeasure (SEPARATOR compMeasure)*;
exprInterval : selectorOpenInterval (selectorDateInterval | selectorIntInterval) selectorCloseInterval;
exprComp     : compDescriptorEqual | BRACKET_ROUND_OPENED exprComp BRACKET_ROUND_CLOSED | LOGICAL_NOT exprComp | exprComp (LOGICAL_OR | LOGICAL_AND) exprComp;
exprGroup    : exprAggregate (LOGICAL_IGNORE compGroupIgnore)?;
exprAggregate: selectorDescriptorId (SEPARATOR selectorDescriptorId)*;

/*
 * Define the different redudant definitions within the parts of the statement
 */
compMeasure              : compMeasureAtom | compMeasureAtom selectorSecondMathOperator compMeasure;
compMeasureAtom          : compAggrFunction | compMeasureAtom selectorFirstMathOperator compMeasureAtom | BRACKET_ROUND_OPENED compMeasure BRACKET_ROUND_CLOSED;
compDescriptorEqual      : selectorDescriptorId CMP_EQUAL selectorDescValue;
compDescValueTupel       : BRACKET_ROUND_OPENED selectorDescValue (SEPARATOR selectorDescValue)* BRACKET_ROUND_CLOSED;
compGroupIgnore          : BRACKET_CURLY_OPENED compDescValueTupel (SEPARATOR compDescValueTupel)* BRACKET_CURLY_CLOSED;
compAggrFunction         : selectorAggrFunctionName BRACKET_ROUND_OPENED compDescriptorFormula BRACKET_ROUND_CLOSED;
compDescriptorFormula    : compDescriptorFormulaAtom | compDescriptorFormulaAtom selectorSecondMathOperator compDescriptorFormula;
compDescriptorFormulaAtom: selectorDescriptorId | compDescriptorFormulaAtom selectorFirstMathOperator compDescriptorFormulaAtom | BRACKET_ROUND_OPENED compDescriptorFormula BRACKET_ROUND_CLOSED;

/*
 * Define special selectors which make up a semantic based on specific tokens, 
 * a selector is understood - in our case - as an atom, which cannot be a token,
 * because it's semantic is context based
 */
selectorModelId           : MARKED_ID | SIMPLE_ID | ENHANCED_ID;
selectorDescriptorId      : MARKED_ID | SIMPLE_ID | ENHANCED_ID;
selectorSelectType        : TYPE_TIMESERIES | TYPE_RECORDS;
selectorDateInterval      : DATE SEPARATOR DATE;
selectorIntInterval       : INT SEPARATOR INT;
selectorOpenInterval      : BRACKET_ROUND_OPENED | BRACKET_SQUARE_OPENED;
selectorCloseInterval     : BRACKET_ROUND_CLOSED | BRACKET_SQUARE_CLOSED;
selectorDescValue         : (DESC_VALUE | NULL_VALUE);
selectorAggrFunctionName  : (AGGR_COUNT | AGGR_SUM | AGGR_MIN | AGGR_MAX | AGGR_AVERAGE | AGGR_MEAN | AGGR_MODE | AGGR_MEDIAN | SIMPLE_ID);

selectorFirstMathOperator : MATH_MULTIPLY | MATH_DIVISION;
selectorSecondMathOperator: MATH_PLUS | MATH_MINUS;

/*
 * Define the different tokens, order is important because of first match, 
 * i.e.
 *  - specially marked tokens first, so that the marking is recognized
 *  - reserved words second
 *  - general tokens lately
 */
// "..." everything marked by quotes should be handled as identifier
MARKED_ID : SYM_IDMARKER (SIMPLE_ID | ENHANCED_ID) SYM_IDMARKER;

// '...' everything marked by single quotes should be handled as value of a descriptor
DESC_VALUE: SYM_DESC_VALUE ((SYM_QUOTE (SYM_DESC_VALUE | SYM_QUOTE | SYM_ALL_MASK))|~('\''|'\\'))*? SYM_DESC_VALUE;
// NULL used to identify a null descriptor-value
NULL_VALUE: N U L L;

// reserved word to define a SELECT statement
STMT_SELECT   : S E L E C T;

// reserved words to define the types of data to select
TYPE_TIMESERIES: T I M E S E R I E S;
TYPE_RECORDS   : R E C O R D S;

// reserved words to define special positions in the statement
OP_FROM     : F R O M;
OP_OF       : O F;
OP_IN       : I N;
OP_GROUPBY  : G R O U P ' ' B Y;
OP_FILTERBY : F I L T E R ' ' B Y;

// reserved words used for logic expressions 
LOGICAL_OR      : O R | '||';
LOGICAL_AND     : A N D | '&&';
LOGICAL_NOT     : N O T | '!';
LOGICAL_IGNORE  : I G N O R E;

// reserved words used for calculations
MATH_MULTIPLY   : '*';
MATH_DIVISION   : '/';
MATH_PLUS       : '+';
MATH_MINUS      : '-';

// reserved words used for aggregation functions
AGGR_COUNT     : C O U N T;
AGGR_SUM       : S U M;
AGGR_MIN       : M I N;
AGGR_MAX       : M A X;
AGGR_AVERAGE   : A V E R A G E;
AGGR_MODE      : M O D E;
AGGR_MEAN      : M E A N;
AGGR_MEDIAN    : M E D I A N;

// reserved symbols used for comparison
CMP_EQUAL       : '=';

// reserved symbols used as different types of brakets
BRACKET_ROUND_OPENED  : '(';
BRACKET_ROUND_CLOSED  : ')';
BRACKET_SQUARE_OPENED : '[';
BRACKET_SQUARE_CLOSED : ']';
BRACKET_CURLY_OPENED  : '{';
BRACKET_CURLY_CLOSED  : '}';

// reserved symbol used for separation
SEPARATOR   : ',';

// define some special data types
DATE      : [0-9][0-9]'.'[0-9][0-9]'.'[0-9][0-9][0-9][0-9]' '[0-9][0-9]':'[0-9][0-9]':'[0-9][0-9] |
            [0-9][0-9]'.'[0-9][0-9]'.'[0-9][0-9][0-9][0-9] |
            [0-9][0-9][0-9][0-9]'-'[0-9][0-9]'-'[0-9][0-9]' '[0-9][0-9]':'[0-9][0-9]':'[0-9][0-9] |
            [0-9][0-9][0-9][0-9]'-'[0-9][0-9]'-'[0-9][0-9] |
            [0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]' '[0-9][0-9]':'[0-9][0-9]':'[0-9][0-9] |
            [0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9] |
            [0-9][0-9][0-9][0-9]'.'[0-9][0-9]'.'[0-9][0-9]' '[0-9][0-9]':'[0-9][0-9]':'[0-9][0-9] |
            [0-9][0-9][0-9][0-9]'.'[0-9][0-9]'.'[0-9][0-9];
INT       : [0-9]+;

// define the strings allowed to occure 
SIMPLE_ID   : [A-Za-z]+;
ENHANCED_ID : [A-Za-z][A-Za-z0-9_\-]*;

// ignore all not specially mentioned wildspaces
WHITESPACE: [ \t\r\n]+ -> skip;

/*
 * Define some special tokens used for wildchars, and special markups.
 */
fragment SYM_ALL_MASK      : '*';
fragment SYM_DESC_VALUE    : '\'';
fragment SYM_QUOTE         : '\\';
fragment SYM_IDMARKER      : '"';

/*
 * There is no case insensitive matching, therefore we define
 * one token for each character which can be case insensitive.
 */
fragment A: 'A' | 'a';
fragment B: 'B' | 'b';
fragment C: 'C' | 'c';
fragment D: 'D' | 'd';
fragment E: 'E' | 'e';
fragment F: 'F' | 'f';
fragment G: 'G' | 'g';
fragment H: 'H' | 'h';
fragment I: 'I' | 'i';
fragment J: 'J' | 'j';
fragment K: 'K' | 'k';
fragment L: 'L' | 'l';
fragment M: 'M' | 'm';
fragment N: 'N' | 'n';
fragment O: 'O' | 'o';
fragment P: 'P' | 'p';
fragment Q: 'Q' | 'q';
fragment R: 'R' | 'r';
fragment S: 'S' | 's';
fragment T: 'T' | 't';
fragment U: 'U' | 'u';
fragment V: 'V' | 'v';
fragment W: 'W' | 'w';
fragment X: 'X' | 'x';
fragment Y: 'Y' | 'y';
fragment Z: 'Z' | 'z';