use "rational.sml";
fun printinarray(array) = if (List.length(array) =0) then ""
    else hd(array) ^ "^" ^ printinarray(tl(array));

fun printtriplearr(array : (string *string * string) list) = if (List.length(array) = 0) then ""
    else #1 (hd(array)) ^ "^" ^ #2 (hd(array)) ^ "^" ^ #3 (hd(array)) ^ "|||" ^ printtriplearr(tl(array));
fun containsdot(st, ind) = if (ind = String.size(st)) then false
    else if (String.sub(st, ind) = #".") then true
    else containsdot(st, ind+1);

fun containsandornotttff(st, ind) = if (ind = String.size(st)) then false
    else if (String.sub(st, ind) = #"&" orelse String.sub(st, ind) = #"!" orelse String.sub(st, ind) = #"|" orelse String.sub(st, ind) = #"<" orelse String.sub(st, ind) = #">" orelse String.sub(st, ind) = #"="  orelse (ind>=0 andalso ind<= String.size(st)-2 andalso String.sub(st,ind) = #"t" andalso String.sub(st,ind+1) = #"t") orelse (ind>=0 andalso ind<= String.size(st)-2 andalso String.sub(st,ind) = #"f" andalso String.sub(st,ind+1) = #"f")) then true
    else containsandornotttff(st, ind+1);

fun isnumber(n) = if((ord(n) < 58  andalso ord(n) > 47  )) then true
                else false;

fun ischar(next) = if ((ord(next)>64 andalso ord(next) < 91) orelse (ord(next)>96 andalso ord(next) < 123)) then true
                    else false;

fun givebackvalue(typevar, array : (string * string * string) list, store_var) = if (List.length(array) = 0) then store_var
    else if ((#1 (hd(array))) = typevar andalso  (#2 (hd(array))) = store_var) then #3 (hd(array))
    else givebackvalue(typevar, tl(array) , store_var);

fun remove_var(typevar, givenstr, array, ind, store_var, boolean, final_ans) = 
    if (ind >= String.size(givenstr)) then final_ans^givebackvalue(typevar, array, store_var)
    else if (boolean = true andalso (ischar(String.sub(givenstr,ind)) orelse isnumber(String.sub(givenstr,ind)))) then remove_var(typevar, givenstr, array, ind+1, store_var ^ String.str(String.sub(givenstr, ind)), boolean , final_ans)
    else if (boolean = true) then remove_var(typevar, givenstr, array, ind+1, "", false, final_ans^ givebackvalue(typevar, array, store_var) ^ String.str(String.sub(givenstr, ind)))
    else if (boolean = false andalso ischar(String.sub(givenstr,ind))) then remove_var(typevar, givenstr, array, ind+1, store_var ^ String.str(String.sub(givenstr, ind)), true, final_ans )
    else remove_var(typevar, givenstr, array , ind+1, store_var, boolean, final_ans ^ String.str(String.sub(givenstr, ind)));

fun add_if_not_null(operands, mystr)= if (String.size(mystr)  > 0) then mystr::operands
    else operands;

fun givemeoperands(char, operands) = if (char = "+") then BigInt.tostring(BigInt.addi(BigInt.fromstring(hd(operands)), BigInt.fromstring(hd(tl(operands)))))::tl(tl(operands))
    else if (char = "-") then BigInt.tostring(BigInt.subt(BigInt.fromstring(hd(tl(operands))), BigInt.fromstring(hd((operands)))))::tl(tl(operands))
    else if (char = "*") then BigInt.tostring(BigInt.multiply_prev(BigInt.fromstring(hd(operands)), BigInt.fromstring(hd(tl(operands)))))::tl(tl(operands))
    else if (char = "/") then BigInt.tostring(BigInt.divi(BigInt.fromstring(hd(tl(operands))), BigInt.fromstring(hd((operands)))))::tl(tl(operands))
    else if (char = "~" ) then BigInt.tostring(BigInt.nega(BigInt.fromstring(hd(operands))))::tl(operands)
    else (
        BigInt.tostring(BigInt.modu(BigInt.fromstring(hd(tl(operands))), BigInt.fromstring(hd((operands)))))::tl(tl(operands))    );
fun givemerationaloperands(char, operands) = if (char = ".+.") then Rational.showDecimal(Rational.add(Rational.fromDecimal(hd(operands)), Rational.fromDecimal(hd(tl(operands)))))::tl(tl(operands))
    else if (char = ".-.") then Rational.showDecimal(Rational.subtract(Rational.fromDecimal(hd(tl(operands))), Rational.fromDecimal(hd((operands)))))::tl(tl(operands))
    else if (char = ".*.") then Rational.showDecimal(Rational.multiply(Rational.fromDecimal(hd(operands)), Rational.fromDecimal(hd(tl(operands)))))::tl(tl(operands))
    else if (char = "~") then Rational.showDecimal(Rational.neg(Rational.fromDecimal(hd(operands))))::tl(operands)
    else  Rational.showDecimal(Option.valOf(Rational.divide(Rational.fromDecimal(hd(tl(operands))), Rational.fromDecimal(hd((operands))))))::tl(tl(operands))

fun givemeintbooleanoperands(char, operands) = 
    if (char = "+") then BigInt.tostring(BigInt.addi(BigInt.fromstring(hd(operands)), BigInt.fromstring(hd(tl(operands)))))::tl(tl(operands))
    else if (char = "-") then BigInt.tostring(BigInt.subt(BigInt.fromstring(hd(tl(operands))), BigInt.fromstring(hd((operands)))))::tl(tl(operands))
    else if (char = "*") then BigInt.tostring(BigInt.multiply_prev(BigInt.fromstring(hd(operands)), BigInt.fromstring(hd(tl(operands)))))::tl(tl(operands))
    else if (char = "/") then BigInt.tostring(BigInt.divi(BigInt.fromstring(hd(tl(operands))), BigInt.fromstring(hd((operands)))))::tl(tl(operands))
    else if (char = "%") then BigInt.tostring(BigInt.modu(BigInt.fromstring(hd(tl(operands))), BigInt.fromstring(hd((operands)))))::tl(tl(operands))
    else if (char = "~" ) then BigInt.tostring(BigInt.nega(BigInt.fromstring(hd(operands))))::tl(operands)
    else if (char  = "&&") then (
        if (hd(operands) = "ff" orelse hd(tl(operands))= "ff") then "ff"::tl(tl(operands))
        else "tt"::tl(tl(operands))
    )
    else if (char = "||") then (
        if (hd(operands) = "tt" orelse hd(tl(operands))= "tt") then "tt"::tl(tl(operands))
        else "ff"::tl(tl(operands))

    )
    else if (char = "!") then (
        if (hd(operands) = "tt") then "ff"::tl(operands)
        else "tt"::tl(operands)
    )
    else if (char = "=") then (
        if (hd(operands) = hd(tl(operands))) then "tt"::tl(tl(operands))
        else "ff"::tl(tl(operands)) 
    )
    else if (char = "<>") then (
        if (hd(operands) <> hd(tl(operands))) then "tt"::tl(tl(operands))
        else "ff"::tl(tl(operands)) 
    )
    else if (char = "<=") then (
        if ((BigInt.compare(BigInt.fromstring(hd(tl(operands))),BigInt.fromstring(hd((operands)))) =LESS) orelse (BigInt.compare(BigInt.fromstring(hd(tl(operands))),BigInt.fromstring(hd((operands))))= EQUAL) ) then "tt"::tl(tl(operands))
        else "ff"::tl(tl(operands)) 
    )
    else if (char = ">=") then (
        if ((BigInt.compare(BigInt.fromstring(hd(tl(operands))),BigInt.fromstring(hd((operands)))) =GREATER) orelse (BigInt.compare(BigInt.fromstring(hd(tl(operands))),BigInt.fromstring(hd((operands))))= EQUAL) ) then "tt"::tl(tl(operands))
        else "ff"::tl(tl(operands)) 
    )
    else if (char = ">") then (
        if ((BigInt.compare(BigInt.fromstring(hd(tl(operands))),BigInt.fromstring(hd((operands)))) =GREATER)) then "tt"::tl(tl(operands))
        else "ff"::tl(tl(operands)) 
    )
    else(
        if ((BigInt.compare(BigInt.fromstring(hd(tl(operands))),BigInt.fromstring(hd((operands)))) =LESS)) then "tt"::tl(tl(operands))
        else "ff"::tl(tl(operands)) 

    );   

fun givemeratbooleanoperands(char, operands) = 
    if (char = ".+.") then Rational.showDecimal(Rational.add(Rational.fromDecimal(hd(operands)), Rational.fromDecimal(hd(tl(operands)))))::tl(tl(operands))
    else if (char = ".-.") then Rational.showDecimal(Rational.subtract(Rational.fromDecimal(hd(tl(operands))), Rational.fromDecimal(hd((operands)))))::tl(tl(operands))
    else if (char = ".*.") then Rational.showDecimal(Rational.multiply(Rational.fromDecimal(hd(operands)), Rational.fromDecimal(hd(tl(operands)))))::tl(tl(operands))
    else if (char  = "./.") then Rational.showDecimal(Option.valOf(Rational.divide(Rational.fromDecimal(hd(tl(operands))), Rational.fromDecimal(hd((operands))))))::tl(tl(operands))
    else if (char = "~" ) then Rational.showDecimal(Rational.neg(Rational.fromDecimal(hd(operands))))::tl(operands)
    else if (char  = "&&") then (
        if (hd(operands) = "ff" orelse hd(tl(operands))= "ff") then "ff"::tl(tl(operands))
        else "tt"::tl(tl(operands))
    )
    else if (char = "||") then (
        if (hd(operands) = "tt" orelse hd(tl(operands))= "tt") then "tt"::tl(tl(operands))
        else "ff"::tl(tl(operands))

    )
    else if (char = "!") then (
        if (hd(operands) = "tt") then "ff"::tl(operands)
        else "tt"::tl(operands)
    )
    else if (char = "=") then (
        if (hd(operands) = hd(tl(operands))) then "tt"::tl(tl(operands))
        else "ff"::tl(tl(operands)) 
    )
    else if (char = "<>") then (
        if (hd(operands) <> hd(tl(operands))) then "tt"::tl(tl(operands))
        else "ff"::tl(tl(operands)) 
    )
    else if (char = "<=") then (
        if ((Rational.less(Rational.fromDecimal(hd(tl(operands))),Rational.fromDecimal(hd((operands))))) orelse (Rational.equal(Rational.fromDecimal(hd(tl(operands))),Rational.fromDecimal(hd((operands))))) ) then "tt"::tl(tl(operands))
        else "ff"::tl(tl(operands)) 
    )
    else if (char = ">=") then (
        if ((Rational.less(Rational.fromDecimal(hd(tl(operands))),Rational.fromDecimal(hd((operands)))) =false) ) then "tt"::tl(tl(operands))
        else "ff"::tl(tl(operands)) 
    )
    else if (char = ">") then (
        if ((Rational.less(Rational.fromDecimal(hd(tl(operands))),Rational.fromDecimal(hd((operands)))) =false) andalso (Rational.equal(Rational.fromDecimal(hd(tl(operands))),Rational.fromDecimal(hd((operands)))) =false)) then "tt"::tl(tl(operands))
        else "ff"::tl(tl(operands)) 
    )
    else(
        if (((Rational.less(Rational.fromDecimal(hd(tl(operands))),Rational.fromDecimal(hd((operands)))) =true))) then "tt"::tl(tl(operands))
        else "ff"::tl(tl(operands)) 

    );   

fun handle_rational_closing(operator, operands) = if (hd(operator) = "(") then (tl(operator),operands)
    else handle_rational_closing(tl(operator) , givemerationaloperands(hd(operator) , operands));
fun handle_intboolean_closing(operator, operands) = if (hd(operator) = "(") then (tl(operator),operands)
    else handle_intboolean_closing(tl(operator) , givemeintbooleanoperands(hd(operator) , operands));

fun handle_ratboolean_closing(operator, operands) = if (hd(operator) = "(") then (tl(operator) , operands)
    else handle_ratboolean_closing(tl(operator), givemeratbooleanoperands(hd(operator) , operands));


fun priority_rational(operator, operands, mychar) =
     if (mychar = #"(") then (operator, operands)
     else if (mychar = #"+") then (
        if (List.length(operator) > 0 andalso  ( hd(operator) = ".+." orelse hd(operator) = "./." orelse hd(operator) = ".*." orelse hd(operator) = "~" orelse hd(operator) = ".-.")) then (
            priority_rational(tl(operator), givemerationaloperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else if (mychar = #"-") then (
        if (List.length(operator) > 0 andalso (  hd(operator) = ".-." orelse hd(operator) = ".+." orelse hd(operator) = "./." orelse hd(operator) = ".*." orelse hd(operator) = "~")) then (
            priority_rational(tl(operator), givemerationaloperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else if (mychar = #"/") then (
        if (List.length(operator) >0 andalso ( hd(operator) = "./." orelse hd(operator)= ".*." orelse hd(operator) = "~")) then 
            (
                priority_rational(tl(operator),  givemerationaloperands(hd(operator), operands), mychar)
            )
        else (
        (operator, operands))
    )
    else if (mychar = #"~") then (
        (operator, operands)
    )
    else  (
        if (List.length(operator) > 0 andalso ( hd(operator) = "./." orelse hd(operator)= "~" orelse hd(operator)= ".*.")) then (
            priority_rational(tl(operator), givemerationaloperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    );
fun priority_intboolean(operator, operands, mychar) =  if (mychar = #"(") then (operator,operands)
    else if (mychar = #">" orelse mychar = #"<" orelse mychar = #"=") then (
        if (List.length(operator)<> 0 andalso (hd(operator)<> "("))then(
            priority_intboolean(tl(operator) , givemeintbooleanoperands(hd(operator), operands) , mychar)
        )
        else (
            (operator, operands)
        )
    )
    else if (mychar = #"|") then (
        if (List.length(operator) > 0 andalso ( hd(operator) = "&&" orelse hd(operator) = "!" orelse hd(operator) = "||" )) then (
            priority_intboolean(tl(operator), givemeintbooleanoperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else if (mychar = #"&") then (
        if (List.length(operator) > 0 andalso ( hd(operator) = "!" orelse hd(operator) = "&&")) then (
            priority_intboolean(tl(operator), givemeintbooleanoperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else if (mychar = #"+") then (
        if (List.length(operator) > 0 andalso ( hd(operator) = "/" orelse hd(operator) = "*" orelse hd(operator) = "~" orelse hd(operator) = "-" orelse hd(operator)= "+" orelse hd(operator)= "%")) then (
            priority_intboolean(tl(operator), givemeintbooleanoperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else if (mychar = #"-") then (
        if (List.length(operator) > 0 andalso ( hd(operator) = "-" orelse hd(operator) = "+" orelse hd(operator) = "/" orelse hd(operator) = "*" orelse hd(operator) = "~" orelse hd(operator)= "%")) then (
            priority_intboolean(tl(operator), givemeintbooleanoperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else if (mychar = #"/") then (
        if(List.length(operator)> 0 andalso (hd(operator) = "~" orelse hd(operator)= "*" orelse hd(operator) = "/" orelse hd(operator) = "%"))
        then (
            priority_intboolean(tl(operator), givemeintbooleanoperands(hd(operator), operands) , mychar)
        )
        else(
                (operator, operands))

    )
    else if (mychar  = #"~") then (
        (operator, operands)
    )
    else if (mychar = #"%") then (
         if (List.length(operator) > 0 andalso ( hd(operator) = "/" orelse hd(operator) = "~" orelse hd(operator) = "*" orelse hd(operator)= "%")) then (
            priority_intboolean(tl(operator), givemeintbooleanoperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )       
    )
    else if (mychar = #"*") then(
        if (List.length(operator) > 0 andalso ( hd(operator) = "/" orelse hd(operator) = "~" orelse hd(operator) = "*" orelse hd(operator)= "%")) then (
            priority_intboolean(tl(operator), givemeintbooleanoperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else  (
        (operator, operands)
    );
fun priority_ratboolean(operator, operands, mychar) =  if (mychar = #"(") then (operator,operands)
    else if (mychar = #">" orelse mychar = #"<" orelse mychar = #"=") then (
        if (List.length(operator)> 0 andalso(hd(operator)<> "("))then(
            priority_ratboolean(tl(operator) , givemeratbooleanoperands(hd(operator), operands) , mychar)
        )
        else (
            (operator, operands)
        )
    )
    else if (mychar = #"|") then (
        if (List.length(operator) > 0 andalso ( hd(operator) = "&&" orelse hd(operator) = "!" orelse hd(operator) = "||") ) then (
            priority_ratboolean(tl(operator), givemeratbooleanoperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else if (mychar = #"&") then (
        if (List.length(operator) > 0 andalso ( hd(operator) = "!" orelse hd(operator) = "&&")) then (
            priority_ratboolean(tl(operator), givemeratbooleanoperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
     else if (mychar = #"+") then (
        if (List.length(operator) > 0 andalso  ( hd(operator) = "./." orelse hd(operator) = ".*." orelse hd(operator) = "~" orelse hd(operator) = ".-." orelse hd(operator) = ".+.")) then (
            priority_ratboolean(tl(operator), givemeratbooleanoperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else if (mychar = #"-") then (
        if (List.length(operator) > 0 andalso ( hd(operator) = ".+." orelse hd(operator) = "./." orelse hd(operator) = ".*." orelse hd(operator) = "~" orelse hd(operator)= ".-.")) then (
            priority_ratboolean(tl(operator), givemeratbooleanoperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else if (mychar = #"/") then (
        if (List.length(operator) >0 andalso (hd(operator)= ".*." orelse hd(operator) = "~" orelse hd(operator) = "./.")) then 
            (
                priority_ratboolean(tl(operator),  givemeratbooleanoperands(hd(operator), operands), mychar)
            )
        else (
        (operator, operands))
    )
    else if (mychar = #"~") then (
        (operator, operands)
    )
    else if (mychar  = #"*") then (
        if (List.length(operator) > 0 andalso ( hd(operator) = "./." orelse hd(operator)= "~" orelse hd(operator) = ".*.")) then (
            priority_ratboolean(tl(operator), givemeratbooleanoperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else  (
        (operator, operands)
    );
fun handle_closing(operator, operands) = if (hd(operator) = "(") then (tl(operator),operands)
    else handle_closing(tl(operator) , givemeoperands(hd(operator) , operands));

fun priority(operator, operands, mychar) = if (mychar = #"(") then (operator,operands)
    else if (mychar = #"+") then (
        if (List.length(operator) > 0 andalso (hd(operator) = "+" orelse hd(operator) = "-" orelse hd(operator) = "/" orelse hd(operator) = "*" orelse hd(operator) = "~" orelse hd(operator)= "%")) then (
            priority(tl(operator), givemeoperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else if (mychar = #"-") then (
        if (List.length(operator) > 0 andalso ( hd(operator) = "-" orelse hd(operator) = "+" orelse hd(operator) = "/" orelse hd(operator) = "*" orelse hd(operator) = "~" orelse hd(operator)= "%")) then (
            priority(tl(operator), givemeoperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else if (mychar = #"/") then (
        if(List.length(operator)> 0 andalso (hd(operator) = "~" orelse hd(operator)= "*" orelse hd(operator) = "/" orelse hd(operator)= "%"))
        then (
            priority(tl(operator), givemeoperands(hd(operator), operands) , mychar)
        )
        else(
            (operator, operands))

    )
    else if (mychar  = #"~") then (
        (operator, operands)
    )
    else if (mychar = #"%") then (
        if(List.length(operator)> 0 andalso (hd(operator) = "~" orelse hd(operator)= "*" orelse hd(operator) = "/" orelse hd(operator)= "%"))
        then (
            priority(tl(operator), givemeoperands(hd(operator), operands) , mychar)
        )
        else(
            (operator, operands))
    )
    else  (
        if (List.length(operator) > 0 andalso ( hd(operator) = "/" orelse hd(operator) = "~" orelse hd(operator) = "*" orelse hd(operator)= "%")) then (
            priority(tl(operator), givemeoperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    );

fun solveintegerexp(expr, operators, operands ,ind, mystr) = 
    if (ind = String.size(expr)) then (
        if (String.size(mystr) > 0 ) then solveintegerexp(expr, operators, mystr::operands , ind,  "")
        else if (List.length(operators) =0 andalso List.length(operands)= 1) then operands
        else solveintegerexp(expr, tl(operators), givemeoperands(hd(operators), operands), ind, mystr)
    )
    else if (String.sub(expr , ind) = #" ") then solveintegerexp(expr, operators, add_if_not_null(operands, mystr), ind+1, "")
    else if ( String.sub(expr,ind) = #"+" orelse String.sub(expr,ind) = #"-" orelse String.sub(expr,ind) = #"*" orelse String.sub(expr,ind) = #"/" orelse String.sub(expr,ind) = #"%" orelse String.sub(expr, ind) = #"(") then 
            solveintegerexp(expr,String.substring(expr,ind,1)::(#1 (priority(operators, add_if_not_null(operands, mystr), String.sub(expr,ind) ))), (#2 (priority(operators, add_if_not_null(operands, mystr), String.sub(expr,ind) ))), ind+1, "" )
    else if (String.sub(expr,ind) = #"~") then solveintegerexp(expr, "~"::operators, operands, ind+1, "")
    else if (String.sub(expr, ind) = #")") then (
        (* print("yaha aaya last mei"); *)
        solveintegerexp(expr, #1 (handle_closing(operators, add_if_not_null(operands, mystr))) , #2 (handle_closing(operators, add_if_not_null(operands,mystr)))  ,ind+1, "")
    )else (
        (* print(printinarray(operators)); *)
        (* print("finalelse m" ^ Int.toString(ind)); *)
        solveintegerexp(expr, operators, operands, ind+1, mystr ^ String.str(String.sub(expr, ind)))
        );

fun solveintbooleanexp(expr, operators, operands, ind , mystr) = 
    if (ind = String.size(expr)) then (
        if (String.size(mystr) > 0 ) then solveintbooleanexp(expr, operators, mystr::operands , ind,  "")
        else if (List.length(operators) =0 andalso List.length(operands)= 1) then operands
        else solveintbooleanexp(expr, tl(operators), givemeintbooleanoperands(hd(operators), operands), ind, mystr)
    )
    else if (String.sub(expr , ind) = #" ") then solveintbooleanexp(expr, operators, add_if_not_null(operands, mystr), ind+1, "")
    (* else if ((String.sub(expr,ind) = #"." andalso (String.sub(expr,ind+1) = #"+" orelse String.sub(expr,ind+1) = #"-" orelse String.sub(expr,ind+1) = #"*" orelse String.sub(expr,ind+1) = #"/"))) then 
            solverationalexp(expr,String.substring(expr,ind , 3)::(#1 (priority_rational(operators, add_if_not_null(operands, mystr), String.sub(expr,ind+1) ))), (#2 (priority_rational(operators, add_if_not_null(operands, mystr), String.sub(expr,ind+1)))), ind+3, "" ) *)
    else if (((String.sub(expr,ind) = #"+" orelse String.sub(expr,ind) = #"-" orelse String.sub(expr,ind) = #"*" orelse String.sub(expr,ind) = #"/" orelse String.sub(expr,ind) = #"%")  )) then 
            solveintbooleanexp(expr,String.substring(expr,ind,1)::(#1 (priority_intboolean(operators, add_if_not_null(operands, mystr), String.sub(expr,ind) ))), (#2 (priority_intboolean(operators, add_if_not_null(operands, mystr), String.sub(expr,ind)) )), ind+1, "" )
    else if ( (ind <= String.size(expr)-2) andalso((String.substring(expr, ind,2) = "&&") orelse (String.substring(expr, ind,2) = "||") orelse (String.substring(expr, ind,2) = "<>") orelse
              (String.substring(expr, ind,2) = ">=") orelse (String.substring(expr, ind,2) = "<="))) then(
                solveintbooleanexp(expr, String.substring(expr,ind , 2)::(#1 (priority_intboolean(operators, add_if_not_null(operands, mystr), String.sub(expr,ind)))) , (#2 (priority_intboolean(operators, add_if_not_null(operands, mystr), String.sub(expr,ind) ))), ind+2, "")
    )
    else if (String.sub(expr, ind) = #"!" orelse (String.substring(expr, ind,1) = "=") orelse (String.substring(expr, ind,1) = ">") orelse (String.substring(expr, ind,1) = "<")) then(
         solveintbooleanexp(expr, String.substring(expr,ind , 1)::(#1 (priority_intboolean(operators, add_if_not_null(operands, mystr), String.sub(expr,ind)))) , (#2 (priority_intboolean(operators, add_if_not_null(operands, mystr), String.sub(expr,ind) ))), ind+1, "")
    )
    else if (String.sub(expr,ind)= #"(") then solveintbooleanexp(expr, "("::operators, add_if_not_null(operands, mystr) , ind+1, "" )
    else if (String.sub(expr,ind) = #"~") then solveintbooleanexp(expr, "~"::operators, operands, ind+1, "")
    else if (String.sub(expr, ind) = #")") then solveintbooleanexp(expr, #1 (handle_intboolean_closing(operators, add_if_not_null(operands, mystr))) , #2( handle_intboolean_closing(operators, add_if_not_null(operands, mystr)))  ,ind+1, "")
    else solveintbooleanexp(expr, operators, operands, ind+1, mystr ^ String.str(String.sub(expr, ind)));

fun solveratbooleanexp(expr, operators, operands, ind , mystr) = 
    if (ind = String.size(expr)) then (
        if (String.size(mystr) > 0 ) then solveratbooleanexp(expr, operators, mystr::operands , ind,  "")
        else if (List.length(operators) =0 andalso List.length(operands)= 1) then operands
        else solveratbooleanexp(expr, tl(operators), givemeratbooleanoperands(hd(operators), operands), ind, mystr)
    )
    else if (String.sub(expr , ind) = #" ") then solveratbooleanexp(expr, operators, add_if_not_null(operands, mystr), ind+1, "")
    else if (ind  <=String.size(expr)-3 andalso (String.sub(expr,ind) = #"." andalso (String.sub(expr,ind+1) = #"+" orelse String.sub(expr,ind+1) = #"-" orelse String.sub(expr,ind+1) = #"*" orelse String.sub(expr,ind+1) = #"/"))) then 
            solveratbooleanexp(expr,String.substring(expr,ind , 3)::(#1 (priority_ratboolean(operators, add_if_not_null(operands, mystr), String.sub(expr,ind+1) ))), (#2 (priority_ratboolean(operators, add_if_not_null(operands, mystr), String.sub(expr,ind+1)))), ind+3, "" )
    (* else if (((String.sub(expr,ind) = #"+" orelse String.sub(expr,ind) = #"-" orelse String.sub(expr,ind) = #"*" orelse String.sub(expr,ind) = #"/" orelse String.sub(expr,ind) = #"%")  )) then 
            solveintbooleanexp(expr,String.substring(expr,ind,1)::(#1 (priority_intboolean(operators, add_if_not_null(operands, mystr), String.sub(expr,ind) ))), (#2 (priority_intboolean(operators, add_if_not_null(operands, mystr), String.sub(expr,ind)) )), ind+1, "" ) *)
    else if (ind<= String.size(expr)-2 andalso ( (String.substring(expr, ind,2) = "&&") orelse (String.substring(expr, ind,2) = "||") orelse (String.substring(expr, ind,2) = "<>") orelse
              (String.substring(expr, ind,2) = ">=") orelse (String.substring(expr, ind,2) = "<="))) then(
                solveratbooleanexp(expr, String.substring(expr,ind , 2)::(#1 (priority_ratboolean(operators, add_if_not_null(operands, mystr), String.sub(expr,ind)))) , (#2 (priority_ratboolean(operators, add_if_not_null(operands, mystr), String.sub(expr,ind) ))), ind+2, "")
    )
    else if (String.sub(expr, ind) = #"!" orelse (String.substring(expr, ind,1) = "=") orelse (String.substring(expr, ind,1) = ">") orelse (String.substring(expr, ind,1) = "<")) then(
         solveratbooleanexp(expr, String.substring(expr,ind , 1)::(#1 (priority_ratboolean(operators, add_if_not_null(operands, mystr), String.sub(expr,ind)))) , (#2 (priority_ratboolean(operators, add_if_not_null(operands, mystr), String.sub(expr,ind) ))), ind+1, "")
    )
    else if (String.sub(expr,ind)= #"(" andalso ((ind= 0) orelse (isnumber(String.sub(expr, ind-1))= false andalso String.sub(expr, ind-1) <> #"."))) then solveratbooleanexp(expr, "("::operators, add_if_not_null(operands, mystr) , ind+1, "" )
    else if ( String.sub(expr, ind)= #"~") then solveratbooleanexp(expr, String.substring(expr, ind, 1)::operators, operands, ind+1, "")
    else if (String.sub(expr, ind) = #")" andalso (String.sub(expr, ind-1) = #")" orelse String.sub(expr, ind-1) = #" " )) then solveratbooleanexp(expr, #1 (handle_ratboolean_closing(operators, add_if_not_null(operands, mystr))) , #2( handle_ratboolean_closing(operators, add_if_not_null(operands, mystr)))  ,ind+1, "")
    else solveratbooleanexp(expr, operators, operands, ind+1, mystr ^ String.str(String.sub(expr, ind)));

fun solverationalexp(expr, operators, operands, ind , mystr) = 
    if (ind = String.size(expr)) then (
        if (String.size(mystr) > 0 ) then solverationalexp(expr, operators, mystr::operands , ind,  "")
        else if (List.length(operators) =0 andalso List.length(operands)= 1) then operands
        else solverationalexp(expr, tl(operators), givemerationaloperands(hd(operators), operands), ind, mystr)
    )
    else if (String.sub(expr , ind) = #" ") then solverationalexp(expr, operators, add_if_not_null(operands, mystr), ind+1, "")
    else if ((String.sub(expr,ind) = #"." andalso ind <= String.size(expr)-2 andalso (String.sub(expr,ind+1) = #"+" orelse String.sub(expr,ind+1) = #"-" orelse String.sub(expr,ind+1) = #"*" orelse String.sub(expr,ind+1) = #"/"))) then 
            solverationalexp(expr,String.substring(expr,ind , 3)::(#1 (priority_rational(operators, add_if_not_null(operands, mystr), String.sub(expr,ind+1) ))), (#2 (priority_rational(operators, add_if_not_null(operands, mystr), String.sub(expr,ind+1) ))), ind+3, "" )
    else if (String.sub(expr,ind)= #"(" andalso ((ind= 0) orelse (isnumber(String.sub(expr, ind-1))= false andalso String.sub(expr, ind-1) <> #"."))) then solverationalexp(expr, "("::operators, add_if_not_null(operands, mystr) , ind+1, "" )
    else if ( String.sub(expr, ind)= #"~") then solverationalexp(expr, String.substring(expr, ind, 1)::operators, operands, ind+1, "")
    else if (String.sub(expr, ind) = #")"andalso (String.sub(expr, ind-1) = #")" orelse String.sub(expr, ind-1) = #" " )) then solverationalexp(expr, #1 (handle_rational_closing(operators, add_if_not_null(operands, mystr))) , #2( handle_rational_closing(operators, add_if_not_null(operands, mystr)))  ,ind+1, "")
    else solverationalexp(expr, operators, operands, ind+1, mystr ^ String.str(String.sub(expr, ind)));


fun handle_rat_boolean_expression (st, array) = 
    let
    val expr = remove_var("boolean",st, array, 0, "", false, "")
    val new_expr = remove_var("rational" , expr, array, 0, "", false, "")
    val jam = solveratbooleanexp(new_expr, [], [], 0,"")
    val giveproperstring = hd(jam)
    in
        giveproperstring
end;
fun handle_int_boolean_expression (st, array) = 
    let
    val expr = remove_var("boolean",st, array, 0, "", false, "")
    val new_expr = remove_var("integer" , expr, array, 0 , "", false, "")
    val jam = solveintbooleanexp(new_expr, [], [], 0,"")
    val givepropersting = hd(jam)
    in
        givepropersting
end;
fun handle_rational_expression(st, array)  = 
    let 
    val expr = remove_var("rational" , st, array, 0 , "" , false, "")
    (* val mystr = expr; *)
    val jam = solverationalexp(expr, [], [] ,0, "")
    val giveproperstring = (hd(jam))
    in 
        (* print(mystr);
        "1" *)
        giveproperstring
end;
fun handle_integer_expression(st, array) =    
 let 
    val expr = remove_var("integer" , st, array, 0 , "" , false, "")
    val jam = solveintegerexp(expr, [], [] ,0, "")
    val giveproperstring =(hd(jam))
    in 
        giveproperstring
end;
fun handle_expression(st, array) = if (containsandornotttff(st, 0)) then(
    if (containsdot(st, 0)) then handle_rat_boolean_expression(st, array)
    else handle_int_boolean_expression(st, array)
    ) 
    else if (containsdot(st, 0)) then handle_rational_expression(st, array)
    else handle_integer_expression(st, array);
