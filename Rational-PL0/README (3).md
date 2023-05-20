This is the grammar, I have used for scanning and parsing according to me :

Program ::= Block
Block ::= DeclarationSeq CommandSeq
DeclarationSeq ::= EPSILON | VarDecls | ProcDecls | VarDecls ProDecls
VarDecls ::= EPSILON | RatVarDecls IntVarDecls BoolVarDecls | DoubleVarDecls | SingleVarDecl
DoubleVarDecls ::= RatVarDecls IntVarDecls | IntVarDecls BoolVarDecls | RatVarDecls BoolVarDecls
SingleVarDecl ::= RatVarDecls | IntVarDecls | BoolVarDecls
RatVarDecls ::= RATIONAL Identifiers SEMICOLON
IntVarDecls ::= INTEGER Identifiers SEMICOLON
BoolVarDecls ::= BOOLEAN Identifiers SEMICOLON
Identifiers  ::= Identifier | Identifier COMMA Identifiers
ProcDecls ::= EPSILON | ProcDef SEMICOLON | ProcDef SEMICOLON ProcDecls
ProcDef  ::= procedure Identifier Block
CommandSeq ::= LPAREN Commands RPAREN
Commands ::= EPSILON | Command SEMICOLON | Command SEMICOLON Commands
Command ::= AssignmentCmd | CallCmd | ReadCmd | PrintCmd | ConditionalCmd | WhileCmd
AssignmentCmd ::= Identifier := Expression
CallCmd ::= call Identifier
ReadCmd ::= read( Identifier )
PrintCmd ::= print( Expression )
Expression ::= RatExpression | IntExpression | BoolExpression
ConditionalCmd ::= if BoolExpression then CommandSeq else CommandSeq fi
WhileCmd ::= while BoolExpression do CommandSeq od
Expression  ::= IntegerExpression | BoolExpression | RatExpression
IntegerExpression ::= Identifier | Integer | IntegerExpression "+" IntegerExpression | IntegerExpression "-" IntegerExpression |
                    IntegerExpression "/" IntegerExpression | IntegerExpression "*" IntegerExpression | IntegerExpression "%" IntegerExpression | "~" IntegerExpression
RatExpression ::=   Identifier | Rational | RatExpression ".+." RatExpression | RatExpression ".-." RatExpression |
                    RatExpression "./." RatExpression | RatExpression ".*." RatExpression | "~" RatExpression | "+" RatExpression
BoolExpression ::= "tt" | "ff" | "!" BoolExpression | BoolExpression "&&" BoolExpression | BoolExpression "||"  BoolExpression |
                    BoolExpression| IntegerExpression "=" IntegerExpression | IntegerExpression "<>" IntegerExpression | IntegerExpression ">=" IntegerExpression | IntegerExpression "<=" IntegerExpression |
                    IntegerExpression ">" IntegerExpression | IntegerExpression "<" IntegerExpression | RatExpression ">" RatExpression | RatExpression "=" RatExpression | RatExpression "<>" RatExpression | RatExpression ">=" RatExpression | RatExpression "<=" RatExpression
Rational :=  PosRat | "~" PosRat
PosRat :=  Integer | Frac | Deci
Frac := Integer "/" integer
Deci :=  "." Integer | Integer "." Integer
Identifier:= Letter | Identifier Letter | Identifier Dig
Letter ::= ["A"-"Z"] | ["a"-"z"]
Dig ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 

and this grammar extends the below grammar for rational expressions:

EXPR := ADD_TERM | EXPR SUB ADD_TERM
ADD_TERM := MULT_TERM | ADD_TERM ADD MULT_TERM
MULT_TERM := DIV_TERM | MULT_TERM MULT DIV_TERM
DIV_TERM := TOK | DIV_TERM DIV TOK
TOK := RAT | VAR | "(" EXPR ")"
VAR := IDENTIFIER
IDENTIFIER := LETTER | IDENTIFIER LETTER | IDENTIFIER DIG
ADD := "+"
MULT := "*"
DIV := "/"
SUB := "-"
LETTER := ["A"-"Z"] | ["a"-"z"]
DIG := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 
