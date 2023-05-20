use "rational.sml";

fun givemerationaloperands(char, operands) = if (char = "+") then Rational.showDecimal(Rational.add(Rational.fromDecimal(hd(operands)), Rational.fromDecimal(hd(tl(operands)))))::tl(tl(operands))
    else if (char = "-") then Rational.showDecimal(Rational.subtract(Rational.fromDecimal(hd(tl(operands))), Rational.fromDecimal(hd((operands)))))::tl(tl(operands))
    else if (char = "*") then Rational.showDecimal(Rational.multiply(Rational.fromDecimal(hd(operands)), Rational.fromDecimal(hd(tl(operands)))))::tl(tl(operands))
    else if (char = "~") then Rational.showDecimal(Rational.neg(Rational.fromDecimal(hd(operands))))::tl(operands)
    else  Rational.showDecimal(Option.valOf(Rational.divide(Rational.fromDecimal(hd(tl(operands))), Rational.fromDecimal(hd((operands))))))::tl(tl(operands));

fun handle_rational_closing(operator, operands) = if (hd(operator) = "(") then (tl(operator),operands)
    else handle_rational_closing(tl(operator) , givemerationaloperands(hd(operator) , operands));

fun isnumber(n) = if((ord(n) < 58  andalso ord(n) > 47  )) then true
                else false;

fun add_if_not_null(operands, mystr)= if (String.size(mystr)  > 0) then mystr::operands
    else operands;

fun priority_rational(operator, operands, mychar) =
     if (mychar = #"(") then (operator, operands)
     else if (mychar = #"+") then (
        if (List.length(operator) > 0 andalso  ( hd(operator)= "+" orelse hd(operator) = "/" orelse hd(operator) = "*" orelse hd(operator) = "~" orelse hd(operator) = "-")) then (
            priority_rational(tl(operator), givemerationaloperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else if (mychar = #"-") then (
        if (List.length(operator) > 0 andalso ( hd(operator) = "-" orelse hd(operator) = "+" orelse hd(operator) = "/" orelse hd(operator) = "*" orelse hd(operator) = "~")) then (
            priority_rational(tl(operator), givemerationaloperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
    else if (mychar = #"/") then (
        if (List.length(operator) >0 andalso (hd(operator) = "/" orelse hd(operator)= "*" orelse hd(operator) = "~")) then 
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
        if (List.length(operator) > 0 andalso ( hd(operator) = "*" orelse hd(operator) = "/" orelse hd(operator)= "~")) then (
            priority_rational(tl(operator), givemerationaloperands(hd(operator), operands), mychar)
        )
        else (
            (operator,operands)
        )
    )
;

fun solverationalexp(expr, operators, operands, ind , mystr) = 
    if (ind = String.size(expr)) then (
        if (String.size(mystr) > 0 ) then solverationalexp(expr, operators, mystr::operands , ind,  "")
        else if (List.length(operators) =0 andalso List.length(operands)= 1) then operands
        else solverationalexp(expr, tl(operators), givemerationaloperands(hd(operators), operands), ind, mystr)
    )
    else if (String.sub(expr , ind) = #" ") then solverationalexp(expr, operators, add_if_not_null(operands, mystr), ind+1, "")
    else if (((String.sub(expr,ind) = #"+" orelse String.sub(expr,ind) = #"-" orelse String.sub(expr,ind) = #"*" orelse String.sub(expr,ind) = #"/" orelse String.sub(expr,ind) = #"%")  )) then    
            solverationalexp(expr,String.substring(expr,ind,1)::(#1 (priority_rational(operators, add_if_not_null(operands, mystr), String.sub(expr,ind) ))), (#2 (priority_rational(operators, add_if_not_null(operands, mystr), String.sub(expr,ind)) )), ind+1, "" )
    else if (String.sub(expr,ind)= #"(" andalso ((ind= 0) orelse (isnumber(String.sub(expr, ind-1))= false andalso String.sub(expr, ind-1) <> #"."))) then solverationalexp(expr, "("::operators, add_if_not_null(operands, mystr) , ind+1, "" )
    else if ( String.sub(expr, ind)= #"~") then solverationalexp(expr, String.substring(expr, ind, 1)::operators, operands, ind+1, "")
    else if (String.sub(expr, ind) = #")"andalso (String.sub(expr, ind-1) = #")" orelse String.sub(expr, ind-1) = #" " )) then solverationalexp(expr, #1 (handle_rational_closing(operators, add_if_not_null(operands, mystr))) , #2( handle_rational_closing(operators, add_if_not_null(operands, mystr)))  ,ind+1, "")
    else solverationalexp(expr, operators, operands, ind+1, mystr ^ String.str(String.sub(expr, ind)));


fun calc(st)  = 
    let 
    val jam = solverationalexp(st, [], [] ,0, "")
    val giveproperstring = (hd(jam))
    in 
        giveproperstring
end;