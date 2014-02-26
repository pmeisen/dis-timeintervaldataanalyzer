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
 * SELECT TIMELINES IN ([START, END]) GROUP BY DESC_ID, "desc name"
 */
select    : STMT_SELECT TYPE OPERATOR interval;

interval  : OPENED_BRACE (INT SEPARATOR INT | DATE SEPARATOR DATE) CLOSED_BRACE;

STMT_SELECT: 'SELECT';

TYPE     : TYPE_TIMELINES 
         | TYPE_RECORDS
         ;
fragment TYPE_TIMELINES: 'TIMELINES';
fragment TYPE_RECORDS  : 'RECORDS';

OPERATOR : OP_IN;
fragment OP_IN: 'IN';

OPENED_BRACE: '(' | '[';
CLOSED_BRACE: ')' | ']';
SEPARATOR   : ',';

IDENTIFIER: [A-Za-z][A-Za-z0-9_\-]*;
DATE      : DIGIT DIGIT '.' DIGIT DIGIT '.' DIGIT DIGIT DIGIT DIGIT ' ' (DIGIT DIGIT (':' DIGIT DIGIT (':' DIGIT DIGIT)?)?)?;
INT       : DIGIT+;
STRING    : SYM_STRING (ESC|.)*? SYM_STRING;

WHITESPACE: [ \t\r\n]+ -> skip;

fragment SYM_STRING  : '"';
fragment SYM_QUOTE   : '\\';
fragment DIGIT: [0-9];
fragment ESC  : SYM_QUOTE (SYM_STRING | SYM_QUOTE);