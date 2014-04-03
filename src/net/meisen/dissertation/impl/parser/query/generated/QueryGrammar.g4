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

// select statement to select timeSeries or records for a timeWindow (exprInterval)
exprSelect   : STMT_SELECT selectorSelectType OP_FROM selectorModelId (OP_IN exprInterval)? (OP_FILTERBY exprLogic)? (OP_GROUPBY exprAggregate)? EOF;
exprAggregate: IDENTIFIER (SEPARATOR IDENTIFIER)*;
exprLogic    : exprComp+;
exprComp     : compDescriptorEqual | BRACKET_ROUND_OPENED exprComp BRACKET_ROUND_CLOSED | LOGICAL_NOT exprComp | exprComp (LOGICAL_OR | LOGICAL_AND) exprComp;
exprInterval : selectorOpenInterval (selectorDateInterval | selectorIntInterval) selectorCloseInterval;

// define different comparators for metaData
compDescriptorEqual     : IDENTIFIER CMP_EQUAL DESC_VALUE;

selectorModelId         : IDENTIFIER;
selectorSelectType      : TYPE_TIMESERIES | TYPE_RECORDS;
selectorDateInterval    : DATE SEPARATOR DATE;
selectorIntInterval     : INT SEPARATOR INT;
selectorOpenInterval    : BRACKET_ROUND_OPENED | BRACKET_SQUARE_OPENED;
selectorCloseInterval   : BRACKET_ROUND_CLOSED | BRACKET_SQUARE_CLOSED;

STMT_SELECT   : S E L E C T;

TYPE_TIMESERIES: T I M E S E R I E S;
TYPE_RECORDS   : R E C O R D S;

OP_FROM     : F R O M;
OP_IN       : I N;
OP_GROUPBY  : G R O U P ' ' B Y;
OP_FILTERBY : F I L T E R ' ' B Y;

LOGICAL_OR  : O R | '||';
LOGICAL_AND : A N D | '&&';
LOGICAL_NOT : N O T | '!';

CMP_EQUAL       : '=';

BRACKET_ROUND_OPENED  : '(';
BRACKET_ROUND_CLOSED  : ')';
BRACKET_SQUARE_OPENED : '[';
BRACKET_SQUARE_CLOSED : ']';

SEPARATOR   : ',';

IDENTIFIER: [A-Za-z][A-Za-z0-9_\-]*;
DATE      : [0-9][0-9]'.'[0-9][0-9]'.'[0-9][0-9][0-9][0-9]' '[0-9][0-9]':'[0-9][0-9]':'[0-9][0-9] |
            [0-9][0-9]'.'[0-9][0-9]'.'[0-9][0-9][0-9][0-9] |
            [0-9][0-9][0-9][0-9]'-'[0-9][0-9]'-'[0-9][0-9]' '[0-9][0-9]':'[0-9][0-9]':'[0-9][0-9] |
            [0-9][0-9][0-9][0-9]'-'[0-9][0-9]'-'[0-9][0-9] |
            [0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]' '[0-9][0-9]':'[0-9][0-9]':'[0-9][0-9] |
            [0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9] |
            [0-9][0-9][0-9][0-9]'.'[0-9][0-9]'.'[0-9][0-9]' '[0-9][0-9]':'[0-9][0-9]':'[0-9][0-9] |
            [0-9][0-9][0-9][0-9]'.'[0-9][0-9]'.'[0-9][0-9];
INT       : [0-9]+;
DESC_VALUE: SYM_DESC_VALUE ((SYM_QUOTE (SYM_DESC_VALUE | SYM_QUOTE))|.)*? SYM_DESC_VALUE;

WHITESPACE: [ \t\r\n]+ -> skip;

fragment SYM_DESC_VALUE : '\'';
fragment SYM_QUOTE      : '\\';

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