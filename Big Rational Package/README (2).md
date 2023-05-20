The grammar for rational numbers which allows large integers treated as rationals, rationals expressed as numerator-denominator pairings of large integers, and also rationals numbers in the form of decimals.

The Grammar G1 is defined as follows:
G1 <N,T,P,S> = <{RAT, INT, FRAC, DECI, DIG, POS, POS_RAT, POS_INT}, {"1", "2", "3", "4", "5", "6", "7", "8", "9", "0", ".", "/", "~"} , P , RAT>

and the productions(P) are defined as follows:

RAT :=  POS_RAT | "~" POS_RAT

POS_RAT :=  INT | FRAC | DECI

FRAC := INT "/" POS_INT

DECI :=  "." INT | INT "." INT

POS_INT := POS | POS INT

INT := DIG | DIG INT

POS := "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"

DIG := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" 


Now, we have to design a grammar for rational number expressions over the grammar above for rational numbers using the binary operators“+”, “-”, “*”, and “/” in infix form with the usual rules of associativity and precedence. The expressions may contain variables of type rational or int and may be mixtures of rational numbers of the form p/q (where p and q are large integers) as well as rational numbers in decimal form (as defined above).

The grammar G2 is defined as follows:
G2 <N,T,P,S> = <{EXPR, ADD_TERM, MULT_TERM, DIV_TERM, TOK, RAT, VAR, IDENTIFIER, LETTER, DIG, ADD, MULT, SUB, DIV}, {"1", "2", "3", "4", "5", "6", "7", "8", "9", "0", ".", "/", "~", "-", "+", "*", ["A"-"Z"], ["a"-"z"], "(", ")"} , P1 , EXPR>

and the productions(P1) are defined as follows:

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


Here, we have followed the usual rules of precedence i.e. used the BODMAS rule and because of that "/" > "*" > "+" > "-" in preference order and hence are followed in our grammer. Also, our operators are left associative i.e. 1-2-3 = -4 because first - will be used firstly. Also, i have just used the brackets in the second grammer but of course it is not necessary because that's why we need associativity. Thus, if the brackets are removed just our T will be reduced to T' where T' = {"1", "2", "3", "4", "5", "6", "7", "8", "9", "0", ".", "/", "~", "-", "+", "*", ["A"-"Z"], ["a"-"z"] } andalso in our production set the TOK term will just contain RAT | VAR.


