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
package net.meisen.dissertation.parser.query.generated;
}

@lexer::header {
package net.meisen.dissertation.parser.query.generated;
}

/*
 * SELECT TIMELINES IN ([START, END]) FILTER BY A='B' OR "C"='D' GROUP BY DESC_ID, "desc name"
 */
select    : STMT_SELECT (TYPE_TIMELINES | TYPE_RECORDS) OP_IN interval (OP_FILTERBY logic)? (OP_GROUPBY selector_descriptor (SEPARATOR selector_descriptor)*)?;
logic     : expression ((LOGICAL_OR | LOGICAL_AND) logic)* | BRACKET_ROUND_OPENED logic BRACKET_ROUND_CLOSED;
expression: selector_descriptor CMP_EQUAL DESC_VALUE | selector_interval (CMP_EQUAL | CMP_SMALLER | CMP_LARGER | CMP_LARGEREQUAL | CMP_SMALLEREQUAL) (DATE | INT);
interval  : (BRACKET_ROUND_OPENED | BRACKET_SQUARE_OPENED) (INT SEPARATOR INT | DATE SEPARATOR DATE) (BRACKET_ROUND_CLOSED | BRACKET_SQUARE_CLOSED);

selector_interval    : INTERVAL_START | INTERVAL_END;
selector_descriptor  : DESC_NAME | IDENTIFIER;

STMT_SELECT   : S E L E C T;

TYPE_TIMELINES: T I M E L I N E S;
TYPE_RECORDS  : R E C O R D S;

OP_IN       : I N;
OP_GROUPBY  : G R O U P ' ' B Y;
OP_FILTERBY : F I L T E R ' ' B Y;

LOGICAL_OR  : O R | '||';
LOGICAL_AND : A N D | '&&';

INTERVAL_START : S T A R T;
INTERVAL_END   : E N D;

CMP_LARGEREQUAL : '>=';
CMP_SMALLEREQUAL: '<=';
CMP_EQUAL       : '=';
CMP_SMALLER     : '<';
CMP_LARGER      : '>';

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
DESC_NAME : SYM_DESC_NAME ((SYM_QUOTE (SYM_DESC_NAME | SYM_QUOTE))|.)*? SYM_DESC_NAME;

WHITESPACE: [ \t\r\n]+ -> skip;

fragment SYM_DESC_NAME  : '"';
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