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

root   : (exprInsert | exprDelete | exprSelect | exprLoad | exprUnload | exprAlive | exprGet | exprAdd | exprDrop | exprModify | exprGrant | exprRevoke | exprAssign | exprRemove) EOF;

/*
 * Define the expressions to add users or roles.
 */
exprAdd             : STMT_ADD (TYPE_USER VALUE exprWithPassword ((exprWithRoles exprWithPermissions? | exprWithPermissions exprWithRoles?))? | (TYPE_ROLE VALUE exprWithPermissions?));
exprWithPassword    : OP_WITH PROP_PASSWORD VALUE;
exprWithPermissions : OP_WITH TYPE_PERMISSIONS selectorValueList;
exprWithRoles       : OP_WITH TYPE_ROLES selectorValueList;

/*
 * Define the expressions to add users or roles.
 */
exprDrop            : STMT_DROP (((TYPE_USER | TYPE_ROLE) VALUE) | TYPE_MODEL selectorModelId);

/*
 * Define the expressions to modify a user's password.
 */
exprModify          : STMT_MODIFY ((TYPE_USER VALUE exprSetPassword) | (TYPE_MODEL selectorModelId exprSetBulkLoad));
exprSetPassword     : OP_SET PROP_PASSWORD CMP_EQUAL VALUE;
exprSetBulkLoad     : OP_SET PROP_BULKLOAD CMP_EQUAL selectorBoolean;

/*
 * Define the expressions to grant permissions to a user or a role.
 */
exprGrant           : STMT_GRANT TYPE_PERMISSIONS selectorValueList OP_TO (TYPE_USER | TYPE_ROLE) VALUE;

/*
 * Define the expressions to revoke permissions from a user or a role.
 */
exprRevoke          : STMT_REVOKE TYPE_PERMISSIONS selectorValueList OP_FROM (TYPE_USER | TYPE_ROLE) VALUE;

/*
 * Define the expressions to assign a role to a user.
 */
exprAssign                : STMT_ASSIGN (exprAssignSingleRole | exprAssignMultipleRoles) OP_TO TYPE_USER VALUE;
exprAssignSingleRole      : TYPE_ROLE VALUE;
exprAssignMultipleRoles   : TYPE_ROLES selectorValueList;

/*
 * Define the expressions to remove a role from a user.
 */
exprRemove                : STMT_REMOVE (exprRemoveSingleRole | exprRemoveMultipleRoles) OP_FROM TYPE_USER VALUE;
exprRemoveSingleRole      : TYPE_ROLE VALUE;
exprRemoveMultipleRoles   : TYPE_ROLES selectorValueList;

/*
 * Define the different expressions/parts of the get statement
 */
exprGet       : STMT_GET (TYPE_MODELS | TYPE_VERSION | TYPE_USERS | TYPE_ROLES | TYPE_PERMISSIONS);

/*
 * Define the different expressions/parts of the alive statement
 */
exprAlive     : STMT_ALIVE;

/*
 * Define the different expressions/parts of the load statement
 */
exprLoad            : (STMT_LOAD | STMT_OPEN) (selectorModelId | (OP_FROM VALUE)) (OP_SET exprLoadSetProperty (SEPARATOR exprLoadSetProperty)*)?;
exprLoadSetProperty : (PROP_AUTOLOAD CMP_EQUAL selectorBoolean) | (PROP_FORCE CMP_EQUAL selectorBoolean);

/*
 * Define the different expressions/parts of the unload statement
 */
exprUnload            : STMT_UNLOAD selectorModelId (OP_SET exprUnloadSetProperty (SEPARATOR exprUnloadSetProperty)*)?;
exprUnloadSetProperty : (PROP_AUTOLOAD CMP_EQUAL selectorBoolean);

/*
 * Define the different expressions/parts of the insert statement
 */
exprInsert    : STMT_INSERT OP_INTO selectorModelId exprStructure OP_VALUES exprValues (SEPARATOR exprValues)*;
exprStructure : BRACKET_ROUND_OPENED compStructureElement (SEPARATOR compStructureElement)* BRACKET_ROUND_CLOSED;
exprValues    : BRACKET_ROUND_OPENED compValueElement (SEPARATOR compValueElement)* BRACKET_ROUND_CLOSED;

/*
 * Define the different parts of the delete statement
 */
exprDelete    : STMT_DELETE selectorIntIdList OP_FROM selectorModelId;

/*
 * Define the different expressions/parts of the select statement
 */
exprSelect          : exprSelectRecords | exprSelectTimeSeries;
exprSelectRecords   : STMT_SELECT (TYPE_RECORDS | (AGGR_COUNT | OP_IDONLY) BRACKET_ROUND_OPENED TYPE_RECORDS BRACKET_ROUND_CLOSED) OP_FROM selectorModelId (selectorIntervalRelation exprInterval)? (OP_FILTERBY (exprComp | compId))? (OP_LIMIT selectorOffset (SEPARATOR selectorLimit)?)?;
exprSelectTimeSeries: STMT_SELECT (TYPE_TIMESERIES | OP_TRANSPOSE BRACKET_ROUND_OPENED TYPE_TIMESERIES BRACKET_ROUND_CLOSED) (OP_OF exprMeasure)? OP_FROM selectorModelId (OP_IN exprInterval)? (OP_FILTERBY exprComp)? (OP_GROUPBY exprGroup)?;
exprMeasure         : (compNamedLowMeasure (SEPARATOR compNamedLowMeasure)* | (compNamedDimMathMeasure (SEPARATOR compNamedDimMathMeasure)* OP_ON selectorMember));
exprInterval        : selectorOpenInterval (selectorDateInterval | selectorIntInterval) selectorCloseInterval;
exprComp            : compMemberEqual | compDescriptorEqual | BRACKET_ROUND_OPENED exprComp BRACKET_ROUND_CLOSED | LOGICAL_NOT exprComp | exprComp (LOGICAL_OR | LOGICAL_AND) exprComp;
exprGroup           : exprAggregate (compGroupInclude)? (compGroupExclude)?;
exprAggregate       : (selectorMember | selectorDescriptorId) (SEPARATOR (selectorMember | selectorDescriptorId))*;

/*
 * Define the different redudant definitions within the parts of the statement
 */
compNamedLowMeasure      : compLowMeasure (OP_ALIAS selectorAlias)?;
compNamedDimMathMeasure  : compDimMathMeasure (OP_ALIAS selectorAlias)?;
compMemberEqual          : selectorMember CMP_EQUAL selectorValue;
compDescriptorEqual      : selectorDescriptorId CMP_EQUAL selectorValue;
compDescValueTupel       : BRACKET_ROUND_OPENED selectorValue (SEPARATOR selectorValue)* BRACKET_ROUND_CLOSED;
compGroupInclude         : LOGICAL_INCLUDE compGroupFilter;
compGroupExclude         : LOGICAL_EXCLUDE compGroupFilter;
compGroupFilter          : BRACKET_CURLY_OPENED compDescValueTupel (SEPARATOR compDescValueTupel)* BRACKET_CURLY_CLOSED;
compStructureElement     : selectorIntervalDef | selectorDescriptorId;
compValueElement         : selectorNullValue | selectorDateValue | selectorIntValue | selectorValue;
compId                   : FIELD_ID CMP_EQUAL INT (SEPARATOR INT)*;

/*
 * Define the different measures
 */
compDimAggrFunction      : selectorDimAggrFunctionName BRACKET_ROUND_OPENED compDescriptorFormula BRACKET_ROUND_CLOSED;
compLowAggrFunction      : selectorLowAggrFunctionName BRACKET_ROUND_OPENED compDescriptorFormula BRACKET_ROUND_CLOSED;
compMathAggrFunction     : selectorMathAggrFunctionName BRACKET_ROUND_OPENED compLowMeasure BRACKET_ROUND_CLOSED;

compLowMeasure           : compLowMeasure selectorSecondMathOperator compLowMeasureAtom | compLowMeasureAtom;
compLowMeasureAtom       : compLowAggrFunction | compLowMeasureAtom selectorFirstMathOperator compLowMeasureAtom | BRACKET_ROUND_OPENED compLowMeasure BRACKET_ROUND_CLOSED;

compMathMeasure          : compMathMeasure selectorSecondMathOperator compMathMeasureAtom | compMathMeasureAtom;
compMathMeasureAtom      : compMathAggrFunction | compMathMeasureAtom selectorFirstMathOperator compMathMeasureAtom | BRACKET_ROUND_OPENED compMathMeasure BRACKET_ROUND_CLOSED;

compDimMeasure           : compDimMeasure selectorSecondMathOperator compDimMeasureAtom | compDimMeasureAtom;
compDimMeasureAtom       : compDimAggrFunction | compDimMeasureAtom selectorFirstMathOperator compDimMeasureAtom | BRACKET_ROUND_OPENED compDimMeasure BRACKET_ROUND_CLOSED;

compDimMathMeasure       : compDimMathMeasure selectorSecondMathOperator compDimMathMeasureAtom | compDimMathMeasureAtom;
compDimMathMeasureAtom   : compMathMeasure | compDimMeasure | compDimMathMeasureAtom selectorFirstMathOperator compDimMathMeasureAtom | BRACKET_ROUND_OPENED compDimMathMeasureAtom BRACKET_ROUND_CLOSED;

compDescriptorFormula    : compDescriptorFormula selectorSecondMathOperator compDescriptorFormulaAtom | compDescriptorFormulaAtom;
compDescriptorFormulaAtom: selectorDescriptorId | compDescriptorFormulaAtom selectorFirstMathOperator compDescriptorFormulaAtom | BRACKET_ROUND_OPENED compDescriptorFormula BRACKET_ROUND_CLOSED;

/*
 * Define special selectors which make up a semantic based on specific tokens, 
 * a selector is understood - in our case - as an atom, which cannot be a token,
 * because it's semantic is context based
 */
selectorMember              : (MARKED_ID | SIMPLE_ID | ENHANCED_ID) DIMSEPARATOR (MARKED_ID | SIMPLE_ID | ENHANCED_ID) DIMSEPARATOR (MARKED_ID | SIMPLE_ID | ENHANCED_ID);
selectorModelId             : MARKED_ID | SIMPLE_ID | ENHANCED_ID;
selectorDescriptorId        : MARKED_ID | SIMPLE_ID | ENHANCED_ID;
selectorAlias               : MARKED_ID | SIMPLE_ID | ENHANCED_ID;
selectorDateInterval        : DATE SEPARATOR DATE;
selectorOffset              : INT;
selectorLimit               : INT;
selectorIntInterval         : INT SEPARATOR INT;
selectorIntIdList           : INT (SEPARATOR INT)*;
selectorDateIntervalWithNull: (DATE | NULL_VALUE) SEPARATOR (DATE | NULL_VALUE);
selectorIntIntervalWithNull : (INT | NULL_VALUE) SEPARATOR (INT | NULL_VALUE);
selectorDateValue           : DATE;
selectorIntValue            : INT;
selectorNullValue           : NULL_VALUE;
selectorValue               : VALUE | NULL_VALUE;
selectorOpenInterval        : BRACKET_ROUND_OPENED | BRACKET_SQUARE_OPENED;
selectorCloseInterval       : BRACKET_ROUND_CLOSED | BRACKET_SQUARE_CLOSED;
selectorMathAggrFunctionName: (AGGR_COUNT | AGGR_SUM | AGGR_MIN | AGGR_MAX | AGGR_AVERAGE | AGGR_MEAN | AGGR_MODE | AGGR_MEDIAN | SIMPLE_ID);
selectorDimAggrFunctionName : (AGGR_COUNT | AGGR_SUM | AGGR_MIN | AGGR_MAX | AGGR_AVERAGE | AGGR_MEAN | AGGR_MODE | AGGR_MEDIAN | SIMPLE_ID);
selectorLowAggrFunctionName : (AGGR_COUNT | AGGR_SUM | AGGR_MIN | AGGR_MAX | AGGR_AVERAGE | AGGR_MEAN | AGGR_MODE | AGGR_MEDIAN | AGGR_COUNTSTARTED | AGGR_COUNTFINISHED | SIMPLE_ID);
selectorFirstMathOperator   : MATH_MULTIPLY | MATH_DIVISION;
selectorSecondMathOperator  : MATH_PLUS | MATH_MINUS;
selectorIntervalDef         : (POS_START_INCL | POS_START_EXCL) | (POS_END_INCL | POS_END_EXCL);
selectorBoolean             : LOGICAL_TRUE | LOGICAL_FALSE;
selectorIntervalRelation    : IR_EQUALTO | IR_BEFORE | IR_AFTER | IR_MEETING | IR_OVERLAPPING | IR_DURING | IR_CONTAINING | IR_STARTINGWITH | IR_FINISHINGWITH | IR_WITHIN;
selectorValueList           : VALUE (SEPARATOR VALUE)*;

/*
 * Define the different tokens, order is important because of first match, 
 * i.e.
 *  - specially marked tokens first, so that the marking is recognized
 *  - reserved words second
 *  - general tokens last
 */
// "..." everything marked by quotes should be handled as identifier
MARKED_ID : SYM_IDMARKER (SIMPLE_ID | ENHANCED_ID) SYM_IDMARKER;

// '...' everything marked by single quotes should be handled as value of a descriptor
VALUE: SYM_VALUE ((SYM_QUOTE (SYM_VALUE | SYM_QUOTE | SYM_ALL_MASK))|~('\''|'\\'))*? SYM_VALUE;
// NULL used to identify a null descriptor-value
NULL_VALUE: N U L L;

// reserverd words to specify the positions of start and end
POS_START_INCL : BRACKET_SQUARE_OPENED S T A R T '+'? BRACKET_SQUARE_CLOSED;
POS_END_INCL   : BRACKET_SQUARE_OPENED E N D '+'? BRACKET_SQUARE_CLOSED;
POS_START_EXCL : BRACKET_SQUARE_OPENED S T A R T '-' BRACKET_SQUARE_CLOSED;
POS_END_EXCL   : BRACKET_SQUARE_OPENED E N D '-' BRACKET_SQUARE_CLOSED;

// the word to select the identifier
FIELD_ID       : BRACKET_SQUARE_OPENED I D BRACKET_SQUARE_CLOSED;

// reserved words to define a SELECT statement
STMT_GET      : G E T;
STMT_SELECT   : S E L E C T;
STMT_INSERT   : I N S E R T;
STMT_DELETE   : D E L E T E;
STMT_OPEN     : O P E N;
STMT_LOAD     : L O A D;
STMT_UNLOAD   : U N L O A D;
STMT_ALIVE    : A L I V E;
STMT_ADD      : A D D;
STMT_DROP     : D R O P;
STMT_MODIFY   : M O D I F Y;
STMT_GRANT    : G R A N T;
STMT_REVOKE   : R E V O K E;
STMT_ASSIGN   : A S S I G N;
STMT_REMOVE   : R E M O V E;

// reserved words for properties
PROP_AUTOLOAD : A U T O L O A D;
PROP_FORCE    : F O R C E;
PROP_PASSWORD : P A S S W O R D;
PROP_BULKLOAD : B U L K L O A D;

// reserved words to define the types of modifications
TYPE_TIMESERIES  : T I M E S E R I E S;
TYPE_RECORDS     : R E C O R D S;
TYPE_MODELS      : M O D E L S;
TYPE_MODEL       : M O D E L;
TYPE_VERSION     : V E R S I O N;
TYPE_PERMISSIONS : P E R M I S S I O N S;
TYPE_ROLES       : R O L E S;
TYPE_USERS       : U S E R S;
TYPE_PERMISSION  : P E R M I S S I O N;
TYPE_ROLE        : R O L E;
TYPE_USER        : U S E R;

// reserved words to define special positions in the statement
OP_FROM     : F R O M;
OP_OF       : O F;
OP_ON       : O N;
OP_TO       : T O;
OP_IN       : I N;
OP_INTO     : I N T O;
OP_SET      : S E T;
OP_VALUES   : V A L U E S;
OP_ALIAS    : A S;
OP_GROUPBY  : G R O U P ' ' B Y;
OP_FILTERBY : F I L T E R ' ' B Y | W H E R E;
OP_LIMIT    : L I M I T;
OP_TRANSPOSE: T R A N S P O S E;
OP_IDONLY   : I D S;
OP_WITH     : W I T H;

// reserved words used to express relations among the time-window and an interval
IR_EQUALTO       : E Q U A L T O;
IR_BEFORE        : B E F O R E;
IR_AFTER         : A F T E R;
IR_MEETING       : M E E T I N G;
IR_OVERLAPPING   : O V E R L A P P I N G;
IR_DURING        : D U R I N G;
IR_WITHIN        : W I T H I N;
IR_CONTAINING    : C O N T A I N I N G;
IR_STARTINGWITH  : S T A R T I N G W I T H;
IR_FINISHINGWITH : F I N I S H I N G W I T H;

// reserved words used for logic expressions 
LOGICAL_OR      : O R | '||';
LOGICAL_AND     : A N D | '&&';
LOGICAL_NOT     : N O T | '!';
LOGICAL_INCLUDE : I N C L U D E;
LOGICAL_EXCLUDE : E X C L U D E;
LOGICAL_TRUE    : T R U E;
LOGICAL_FALSE   : F A L S E;

// reserved words used for calculations
MATH_MULTIPLY   : '*';
MATH_DIVISION   : '/';
MATH_PLUS       : '+';
MATH_MINUS      : '-';

// reserved words used for aggregation functions
AGGR_COUNTSTARTED : C O U N T S T A R T E D;
AGGR_COUNTFINISHED: C O U N T F I N I S H E D;
AGGR_COUNT        : C O U N T;
AGGR_SUM          : S U M;
AGGR_MIN          : M I N;
AGGR_MAX          : M A X;
AGGR_AVERAGE      : A V E R A G E;
AGGR_MODE         : M O D E;
AGGR_MEAN         : M E A N;
AGGR_MEDIAN       : M E D I A N;

// reserved symbols used for comparison
CMP_EQUAL       : '=';

// reserved symbols used as different types of brakets
BRACKET_ROUND_OPENED  : '(';
BRACKET_ROUND_CLOSED  : ')';
BRACKET_SQUARE_OPENED : '[';
BRACKET_SQUARE_CLOSED : ']';
BRACKET_CURLY_OPENED  : '{';
BRACKET_CURLY_CLOSED  : '}';

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

// reserved symbol used for separation
SEPARATOR   : ',';
DIMSEPARATOR: '.';

// define the strings allowed to occure 
SIMPLE_ID   : [A-Za-z]+;
ENHANCED_ID : [A-Za-z][A-Za-z0-9_\-]*;

// ignore all not specially mentioned wildspaces
WHITESPACE: [ \t\r\n]+ -> skip;

/*
 * Define some special tokens used for wildchars, and special markups.
 */
fragment SYM_ALL_MASK      : '*';
fragment SYM_VALUE         : '\'';
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