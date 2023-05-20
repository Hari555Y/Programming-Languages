use "expressions.sml";

fun get_char_file (myinstream) = 
    let 
	fun loop (list, myinstream) = 
	    case TextIO.StreamIO.input1 myinstream of
		SOME (c, myinstream') => loop (c::list, myinstream')
	      | NONE =>  (list)
    in  rev(loop ([], myinstream)) 
end;
fun printinarray(array) = if (List.length(array) =0) then ""
    else hd(array) ^ "^" ^ printinarray(tl(array));

fun printtriplearr(array : (string *string * string) list) = if (List.length(array) = 0) then ""
    else #1 (hd(array)) ^ "^" ^ #2 (hd(array)) ^ "^" ^ #3 (hd(array)) ^ "|||" ^ printtriplearr(tl(array));

fun remove_spaces_front (st , ind) =
        if (String.sub(st, ind) = #" ") then remove_spaces_front(st, ind+1)
        else String.extract(st, ind, NONE);

fun remove_spaces_and_semi_front(st, ind) =         
    if (String.sub(st, ind) = #" " orelse String.sub(st, ind) = #";") then remove_spaces_and_semi_front(st, ind+1)
        else String.extract(st, ind, NONE);
fun remove_spaces_behind(st, ind) =
        if (String.sub(st, ind) = #" ") then remove_spaces_behind(st, ind-1)
        else String.substring(st, 0, ind+1);
fun remove_spaces_and_semi_behind(st, ind) =         
    if (String.sub(st, ind) = #" " orelse String.sub(st, ind) = #";") then remove_spaces_and_semi_behind(st, ind-1)
        else String.substring(st, 0, ind+1);
fun givestringafterremoval(st) = 
    let 
    val startdone = remove_spaces_front(st, 0)
    val enddone = remove_spaces_behind(startdone, String.size(startdone)-1)
    in 
        enddone
    end;

fun sandsemiremoval (st) =
    let 
    val startdone = remove_spaces_and_semi_front(st, 0)
    val enddone = remove_spaces_and_semi_behind(startdone, String.size(startdone)-1)
    in 
        enddone
    end;

fun after_removal(stream_commands) =
        List.map givestringafterremoval stream_commands;

fun handle_integers(array, large_str , small_str , ind) = 
    if (String.sub(large_str, ind) = #";") then (
        ("integer",small_str, "")::array
    )
    else if (String.sub(large_str, ind) = #" ") then (
        handle_integers(array, large_str, small_str, ind+1)
    )
    else if (String.sub(large_str, ind) = #",") then (
        handle_integers(("integer", small_str, "")::array , large_str, "", ind+1)
    )
    else(
        handle_integers(array , large_str, small_str ^ str(String.sub(large_str, ind)), ind+1)
    );

fun handle_boolean(array, large_str , small_str , ind) = 
    if (String.sub(large_str, ind) = #";") then (
        ("boolean",small_str, "")::array
    )
    else if (String.sub(large_str, ind) = #" ") then (
        handle_boolean(array, large_str, small_str, ind+1)
    )
    else if (String.sub(large_str, ind) = #",") then (
        handle_boolean(("boolean", small_str, "")::array , large_str, "", ind+1)
    )
    else(
        handle_boolean(array , large_str, small_str ^ str(String.sub(large_str, ind)), ind+1)
    );

fun handle_rationals(array, large_str , small_str , ind) = 
     if (String.sub(large_str, ind) = #";") then (
        ("rational", small_str, "")::array
    )
    else if (String.sub(large_str, ind) = #" ") then (
        handle_rationals(array, large_str, small_str, ind+1)
    )
    else if (String.sub(large_str, ind) = #",") then (
        handle_rationals(("rational", small_str, "")::array , large_str, "", ind+1)
    )
    else(
        handle_rationals(array , large_str, small_str ^ str(String.sub(large_str, ind)), ind+1)
    );

fun set_in_array(my_str, array, matched_var) =     
    if (List.length(array) = 0) then raise Empty
    else if((#2 (List.nth(array,0)))  = matched_var) then (#1 (List.nth(array,0)), #2 (List.nth(array,0)),  my_str)  :: tl(array)
    else hd(array) :: set_in_array(my_str, tl(array), matched_var);

fun giveidentifier(str, ind, final_str) = if (ind >= String.size(str) orelse String.sub(str, ind) = #";" orelse String.sub(str, ind) = #")") then(
    final_str
    )
    else if (isnumber(String.sub(str, ind)) orelse ischar(String.sub(str, ind))) then giveidentifier(str,ind,final_str ^ String.str(String.sub(str, ind)))
    else giveidentifier(str, ind, final_str);


fun handle_call(array : (string * string * string) list, my_str, ind) =
    if (ind = List.length(array)) then raise Empty
    else if((#2 (List.nth(array,ind)))  = giveidentifier(my_str, 0,"")) then (#3 (List.nth(array,ind))) 
    else handle_call(array, my_str, ind+1);

fun handle_read(my_str, array) = 
    let 
    val cmd_input =  valOf(TextIO.inputLine TextIO.stdIn)
    val array  = set_in_array(cmd_input, array, giveidentifier(my_str,0,""))
    in 
        ""
end;
fun handle_print(expression , array)=
    let 
    val x = handle_expression(expression , array)
    in
        x
end;
fun givevariable(stri, ind, vari) = if (String.sub(stri , ind) = #":") then givestringafterremoval(vari)
    else if (String.sub(stri , ind) = #" ") then givevariable(stri, ind+1, vari)
    else givevariable(stri, ind+1, vari ^ String.str(String.sub(stri, ind)));

fun givevalue(stri, ind , vari,  isequal) = if (ind >= String.size(stri) ) then givestringafterremoval(vari)
    else if (ind < String.size(stri) andalso ind>0 andalso String.sub(stri, ind) = #"=" andalso String.sub(stri, ind-1) = #":") then givevalue(stri, ind+1, vari, true)
    else if (isequal = false) then givevalue(stri, ind+1, vari, isequal)
    else (
        givevalue(stri, ind+1, vari^String.str(String.sub(stri, ind)), isequal)
    );

fun handle_assignment(stri, arr) = 
    let 
    val variable  = givevariable(stri,0, "")
    val value = handle_expression(givevalue(stri, 0 , "" , false), arr)
    val arr = set_in_array(value, arr, variable)
    in
        ""
end;
fun givemebxp(str, array, final_ans , ind) = if (ind <= String.size(str)-4 andalso String.sub(str,ind) = #"t" andalso String.substring(str, ind, 4) = "then") then final_ans
    else givemebxp(str, array,  final_ans ^ String.str(String.sub(str, ind)) , ind+1);

fun givemebxpwhile(str, array, final_ans, ind) = if (ind<= String.size(str)-2 andalso String.sub(str,ind) = #"d" andalso String.substring(str, ind, 2) = "do") then final_ans
    else givemebxpwhile(str, array,  final_ans ^ String.str(String.sub(str, ind)) , ind+1);

fun givemecmdexp(str, array, final_ans , ind, mybool) = 
    if (mybool = false andalso ind <= String.size(str)-4 andalso String.sub(str,ind) = #"t" andalso String.substring(str, ind, 4) = "then") then givemecmdexp(str , array, final_ans, ind+4, true)
    else if (mybool = false) then givemecmdexp(str, array, final_ans, ind+1, mybool)
    else if (mybool = true andalso ind <= String.size(str)-4 andalso String.sub(str,ind) = #"e" andalso String.substring(str, ind, 4) = "else") then final_ans
    else givemecmdexp(str, array,  final_ans ^ String.str(String.sub(str, ind)) , ind+1, mybool);

fun givemecmdexp2(str, array, final_ans, ind, mybool) = 
    if (mybool = false andalso ind <= String.size(str)-4 andalso String.sub(str,ind) = #"e" andalso String.substring(str, ind, 4) = "else") then givemecmdexp2(str , array, final_ans, ind+4, true)
    else if (mybool = false) then givemecmdexp2(str, array, final_ans, ind+1, mybool)
    else if (mybool = true andalso ind <= String.size(str)-2 andalso String.sub(str,ind) = #"f" andalso String.substring(str, ind, 2) = "fi") then final_ans
    else givemecmdexp2(str, array,  final_ans ^ String.str(String.sub(str, ind)) , ind+1, mybool);

fun givemecmdexpwhile(str, array, final_ans, ind, mybool) = 
    if (mybool = false andalso ind<= String.size(str)-2 andalso String.sub(str,ind) = #"d" andalso String.substring(str, ind, 2) = "do") then givemecmdexpwhile(str , array, final_ans, ind+2, true)
    else if (mybool = false) then givemecmdexpwhile(str, array, final_ans, ind+1, mybool)
    else if (mybool = true andalso ind<= String.size(str)-2 andalso String.sub(str,ind) = #"o" andalso String.substring(str, ind, 2) = "od") then final_ans
    else givemecmdexpwhile(str, array,  final_ans ^ String.str(String.sub(str, ind)) , ind+1, mybool);

fun handle_file(my_str, prev, next, iscomment, isprocedure, isbalanced, str, dec_arr , cmd_arr) = 
        if (String.sub(my_str,0) = #"(" andalso next = #"*") then handle_file(String.extract(my_str, 1 ,NONE) , String.sub(my_str,0), String.sub(my_str,2) , true,isprocedure,isbalanced, str, dec_arr, cmd_arr)
        else if (iscomment =  true andalso String.sub(my_str,0) = #")" andalso prev = #"*") then handle_file(String.extract(my_str, 1 ,NONE) , String.sub(my_str,0), String.sub(my_str,2) , false,isprocedure, isbalanced, str, dec_arr,cmd_arr)
        else if (iscomment = true) then handle_file(String.extract(my_str, 1 ,NONE), String.sub(my_str,0), String.sub(my_str,2) , iscomment,isprocedure,isbalanced, str, dec_arr , cmd_arr)
        else if (String.sub(my_str,0) = #"#") then (dec_arr, cmd_arr)
        else if (String.sub(my_str,0) = #"\n") then handle_file(String.extract(my_str, 1 ,NONE), #"\n" , String.sub(my_str,2) , iscomment,isprocedure,isbalanced, str, dec_arr, cmd_arr)
        else if (String.sub(my_str,0) = #";" andalso prev = #"}" andalso isprocedure = 1 andalso isbalanced =0) then
        (* andalso isprocedure =1 andalso isbalanced = 0  *)
        (
            (* print(Int.toString(String.size(str))); *)
        handle_file(String.extract(my_str, 1 ,NONE), #";",String.sub(my_str,2), iscomment, isprocedure-1, isbalanced, "" , dec_arr@[(str^";")], cmd_arr) 
        )
        else if (String.sub(my_str,0) = #"}"andalso isbalanced =1 andalso isprocedure = 0) then handle_file(String.extract(my_str, 1 ,NONE) , String.sub(my_str,0), String.sub(my_str,2) , iscomment,isprocedure, isbalanced-1, "", dec_arr, cmd_arr@[str ^ "}"] )
        else if (String.sub(my_str,0) = #";"andalso isprocedure = 0 andalso isbalanced = 0) then (
            (* print("mei yaha aagya bc"); *)
            handle_file(String.extract(my_str, 1 ,NONE) , String.sub(my_str,0), String.sub(my_str,2) , iscomment,isprocedure, isbalanced, "", dec_arr@[(str ^ ";")], cmd_arr )
        )
        else 
            (if (String.sub(my_str, 0) = #"{" andalso iscomment = false) then handle_file( String.extract(my_str, 1 ,NONE) , String.sub(my_str,0) , String.sub(my_str,2) , iscomment, isprocedure, isbalanced+1, str ^ String.str(String.sub(my_str,0)), dec_arr, cmd_arr)
             else if (String.sub(my_str,0) = #"p" andalso String.size(my_str) >= 9 andalso (String.substring(my_str, 0 , 9)) = "procedure" andalso iscomment = false andalso isbalanced = 0) then  handle_file( String.extract(my_str, 9 ,NONE) , #"e" , String.sub(my_str,10) , iscomment, isprocedure + 1, isbalanced, str ^ "procedure", dec_arr, cmd_arr)
             else if  (String.sub(my_str,0) = #";" andalso prev = #"}" andalso isbalanced = 0) then handle_file(String.extract(my_str, 1 ,NONE), #";",String.sub(my_str,2), iscomment, isprocedure-1, isbalanced, str^";" , dec_arr, cmd_arr) 
             else if (String.sub(my_str,0) = #"}") then handle_file(String.extract(my_str, 1 ,NONE) , String.sub(my_str,0), String.sub(my_str,2) , iscomment,isprocedure, isbalanced-1, str ^ "}", dec_arr, cmd_arr)
            else handle_file( String.extract(my_str, 1 ,NONE) , String.sub(my_str,0) , String.sub(my_str,2) , iscomment,isprocedure, isbalanced, str ^ String.str(String.sub(my_str,0)), dec_arr , cmd_arr)); 

fun rationalpl1 (my_str, prev_arr) = 
    let
    val (declaration_seq , command_seq) = handle_file(my_str^"##" , #"\n" ,String.sub(my_str, 1) , false,0,0, "", [], []) 
    (* val dec_printer = printinarray(command_seq) *)
     val all_dec_commands = after_removal(declaration_seq)
    val all_cmd_commands = after_removal(command_seq)
    (* val dec_printer = printinarray(all_dec_commands) *)

    fun handle_function(array, large_str , small_str , ind, prev) = 
        if (String.sub(large_str, ind) = #" " andalso prev <> #" ") then (
            ("procedure", small_str, rationalpl1(String.extract(large_str, ind ,NONE ), array))::array
        )
        else if (String.sub(large_str, ind) = #" ") then (
            handle_function(array, large_str, small_str, ind+1 , #" ")
        )
        else(
            handle_function(array , large_str, small_str ^ str(String.sub(large_str, ind)), ind+1, String.sub(large_str, ind))
    ); 
    fun handle_dec_once(arr, str_cmd) = if (String.size(str_cmd) >=8 andalso String.substring(str_cmd, 0,8) = "integer " ) then (
            handle_integers(arr, String.extract(str_cmd, 8, NONE), "", 0)
        )
        else if (String.size(str_cmd) >=9 andalso String.substring(str_cmd,0, 9) = "rational " ) then (
            handle_rationals(arr, String.extract(str_cmd, 9 , NONE), "", 0)
        )
        else if (String.size(str_cmd) >=8  andalso String.substring(str_cmd,0, 8) = "boolean " ) then (
            handle_boolean(arr, String.extract(str_cmd, 8 , NONE), "", 0)
        )
        else(
            handle_function(arr, String.extract(str_cmd, 10, NONE), "",0 , #" ")
    );
    fun handle_dec_seq(arr, dec_array) = if (List.length(dec_array)=0) then arr
        else handle_dec_seq(handle_dec_once(arr, List.nth(dec_array ,0)), tl(dec_array));

   fun handle_comm_seq(arr, mystr) =
        let
        fun implementif(bexp, cmdexp1, cmdexp2, array) = if(handle_expression(bexp, array) = "tt" ) then handle_comm_seq(array, givestringafterremoval(cmdexp1))
            else handle_comm_seq(array , givestringafterremoval(cmdexp2));
        fun handle_if(str, array) = 
            let
            val bexp = givemebxp(str,array, "", 0)
            val cmdexp1 = givemecmdexp(str, array , "" , 0, false)
            val cmdexp2 = givemecmdexp2(str, array, "" ,0 , false)
            in
                implementif(bexp, cmdexp1, cmdexp2, array)

        end;
        fun implementwhile(bexp, cmdexp, array) = if (handle_expression(bexp, array) = "tt") then handle_comm_seq(array, givestringafterremoval(cmdexp))
            else "";
        fun handle_while(str, array)= 
            let 
            val bexp = givemebxpwhile(str, array,"",0)
            val cmdexp = givemecmdexpwhile(str, array, "" , 0, false)
            in 
                implementwhile(bexp, cmdexp, array)

        end;
        fun handle_single_cmd(arr, stri) = if (String.size(stri) >=5 andalso String.substring(stri,0, 5) = "call " ) then(
            handle_call(arr, sandsemiremoval(String.extract(stri , 5 , NONE)) , 0))
            else if (String.size(stri) >=6 andalso String.substring(stri,0, 5) = "print" ) then(
                handle_print( sandsemiremoval(String.extract(stri , 5, NONE)), arr))
            else if (String.size(stri) >=5 andalso String.substring(stri,0, 4) = "read" ) then(
                handle_read( sandsemiremoval(String.extract(stri , 4 , NONE)) , arr))
            else if (String.size(stri) >=3 andalso String.substring(stri,0, 3) = "if " ) then(
                handle_if( String.extract(stri, 3 , NONE), arr))
            else if (String.size(stri) >=6 andalso String.substring(stri,0, 6) = "while " ) then(
                handle_while( String.extract(stri , 6 , NONE), arr))
            else(
                handle_assignment(stri , arr)
        );

        fun handle_commands(arr, commands_arr) = if (List.length(commands_arr)= 0) then ""
            else handle_single_cmd(arr, List.nth(commands_arr, 0)) ^ handle_commands(arr , tl(commands_arr));

        fun separate_cmd_seq(my_str, prev, next, iscomment, brackets, stro,cmd_arr) = 
            if (String.sub(my_str,0) = #"(" andalso next = #"*") then separate_cmd_seq(String.extract(my_str, 1 ,NONE) , String.sub(my_str,0), String.sub(my_str,2) , true,brackets, stro, cmd_arr)
            else if (iscomment =  true andalso String.sub(my_str,0) = #")" andalso prev = #"*") then separate_cmd_seq(String.extract(my_str, 1 ,NONE) , String.sub(my_str,0), String.sub(my_str,2) , false, brackets, stro, cmd_arr)
            else if (iscomment = true) then separate_cmd_seq(String.extract(my_str, 1 ,NONE), String.sub(my_str,0), String.sub(my_str,2) , iscomment,brackets, stro, cmd_arr)
            else if (String.sub(my_str,0) = #"#") then cmd_arr
            else if (String.sub(my_str,0) = #"\n") then separate_cmd_seq(String.extract(my_str, 1 ,NONE), #"\n" , String.sub(my_str,2) , iscomment,brackets, stro, cmd_arr)
            else if (String.sub(my_str,0) = #"}") then separate_cmd_seq(String.extract(my_str, 1 ,NONE) , String.sub(my_str,0), String.sub(my_str,2) , iscomment,brackets-1,stro ^ "}", cmd_arr )
            else if (String.sub(my_str,0) = #";"andalso iscomment = false andalso brackets = 0) then separate_cmd_seq(String.extract(my_str, 1 ,NONE) , String.sub(my_str,0), String.sub(my_str,2) , iscomment,brackets, "", cmd_arr@[stro ^ ";"] )
            else 
                (if (String.sub(my_str, 0) = #"{"  andalso iscomment = false) then separate_cmd_seq( String.extract(my_str, 1 ,NONE) , String.sub(my_str,0) , String.sub(my_str,2) , iscomment, brackets+1, stro ^ String.str(String.sub(my_str,0)), cmd_arr)
                else separate_cmd_seq( String.extract(my_str, 1 ,NONE) , String.sub(my_str,0) , String.sub(my_str,2) , iscomment,brackets, stro ^ String.str(String.sub(my_str,0)), cmd_arr)
        ); 

        val remove_braces=  String.substring(mystr, 1, String.size(mystr)-2) ^"##"
        val diff_cmd_sequences = separate_cmd_seq(remove_braces, #"\n", String.sub(remove_braces, 1), false, 0 , "" , [])
        val commands_arr = after_removal(diff_cmd_sequences)
        val final_output = handle_commands(arr, commands_arr)
        in
            (* print(printinarray(commands_arr));
            "1" *)
            final_output
        end;
    val myinitial_arr = handle_dec_once(prev_arr,List.nth(all_dec_commands,0))
    (* val sudsee = List.nth(all_dec_commands,0); *)
     val myfinal_arr = handle_dec_seq(myinitial_arr, tl(all_dec_commands) )

     val my_output = handle_comm_seq(myfinal_arr, List.nth(all_cmd_commands, 0))
       (* val taker = (myfinal_arr); *)
     (* val tito = printtriplearr(my_output); *)
    in
        (* print(printinarray(declaration_seq)); *)
        (* print(printtriplearr(taker));
        "1" *)
        (* dec_printer *)
        my_output     
        (* "1" *)
end;

fun rationalpl0(filename) =
    let
    val myinstream = TextIO.getInstream(TextIO.openIn filename)
    val my_str  = String.implode(get_char_file(myinstream))
    in 
        (* print(my_str); *)
        rationalpl1(my_str, [])
end;