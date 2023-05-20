	(* this is  a function to convert an integer a to string my_str *)
fun inttostring(a) =
	let 
	val my_str : string = Int.toString(a)
	in 
		my_str
	end;

	(* function to convert character c to a string str *)
fun chartostring(c) =
	let 
		val str : string = Char.toString(c)
	in 
		str
	end;

(* this function gives us the char list of the whole file we are reading *)
fun get_char_file (myinstream) = 
    let 
	fun loop (list, myinstream) = 
	    case TextIO.StreamIO.input1 myinstream of
		SOME (c, myinstream') => loop (c::list, myinstream')
	      | NONE =>  (list)
    in  rev(loop ([], myinstream))
end;

(* this is the main function handle_csv in case of csv files and this is also important in case of table in .mdt file *)
fun handle_csv(list ,addstream, prev) = if length(list) =2 then  TextIO.output(addstream , str(hd(list))^"</td></tr>\n</TABLE></CENTER>")
        else if (hd(list) = #"\n" andalso prev  = #"<") then(   
          (* in case of starting new lines *)
          handle_csv(tl(list), addstream, hd(list))
        )
        else if (hd(list) = #"\n" andalso (hd(tl(list)) = #">" orelse hd(tl(tl(list)))= #">")) then (
          handle_csv(tl(list), addstream, hd(list))
        )
        else if(hd(list) = #"\n") then ( 
                            TextIO.output(addstream , "</td></tr>\n<tr><td>");
                            handle_csv(tl(list), addstream,  hd(list))
        )
        else if( hd(list) = #"|" ) then(
            (* in case of separator *)
                            TextIO.output(addstream , "</td><td>");
                            handle_csv(tl(list) , addstream, hd(list))
        )
        else if (hd(list) = #" ") then (
                handle_csv(tl(list), addstream,  hd(list))
        )
        else( 
          (* in case of matrix elements *)
              TextIO.output(addstream , str(hd(list)));
                            handle_csv(tl(list), addstream, hd(list))
        );

(* check if some other character  _ exist *)
fun isthereanother (charfile, prev) = if (hd(charfile) = #"\n" orelse hd(charfile) = #" ") then false
                                      else if (hd(charfile) = #"_" andalso prev <> #"\\" andalso (hd(tl(charfile)) = #"\n" orelse hd(tl(charfile)) = #" ") ) then true
                                      else isthereanother(tl(charfile) , hd(charfile));
(* check if another character of ** is there *)
fun isthereanotherbold(charfile, prev) = if (hd(charfile) = #"\n") then false
                                      else if (hd(charfile) = #"*" andalso prev <> #"\\" andalso hd(tl(charfile)) = #"*") then true
                                      else isthereanotherbold(tl(charfile) , hd(charfile));

(* check if another italic char is there for ending *)
fun isthereanotheritalic(charfile, prev) = if (hd(charfile) = #"\n") then false
                                      else if (hd(charfile) = #"*" andalso prev <> #"\\") then true
                                      else isthereanotheritalic(tl(charfile) , hd(charfile));

(* check if underline is there or not  *)
fun isunderline (prev, charfile) =  if ((prev = #" " orelse prev  = #"\n") andalso hd(charfile) = #"_" andalso isthereanother(charfile, prev)) then true
                                  else 
                                    false;

(* check if there is a between underline in there *)
fun isinbetweenunderline(prev, charfile, next, underlinestarted) = if (prev <> #" " andalso next <> #" " andalso prev <> #"\n" andalso next <> #"\n" andalso prev <> #"\\" andalso hd(charfile) = #"_" andalso underlinestarted  = true) then true
                                  else 
                                    false;

(* check if it is the final underline *)
fun isfinalunderline(charfile, next, prev, underlinestarted) = if ((next = #"\n" orelse next = #" " ) andalso prev <> #"\\" andalso underlinestarted = true andalso hd(charfile) = #"_" ) then true
                                      else false;

(* check if the next line or inside can be header or not *)
fun isheader(prev , charfile) =   if (prev <> #"\\" andalso hd(charfile) = #"#") then true
                                  else 
                                    false;

(* check if the line contains bold char *)
fun isbold(charfile , prev, next) = if (hd(charfile) = #"*" andalso next = #"*"  andalso isthereanotherbold(tl(tl(charfile)), hd(tl(charfile)))) then true 
                                      else false;

(* check if the next line is italic or not *)
fun isitalic(charfile , prev, next) = if (hd(charfile) = #"*" andalso next <> #"*" andalso  isthereanotheritalic((tl(charfile)), hd((charfile)))) then true 

                                      else false;

(* checking the header level of the next line *)
fun header_level(charfile, count) = if hd(charfile) = #"#" then header_level(tl(charfile), count+1)
                                    else count;

(* check if the next line is a horizontal line *)
fun nextlineishorizon(charfile, prev, next) = if (hd(charfile) = #"\n") then true
                                              else if (hd(charfile) <> #" " andalso hd(charfile) <> #"-") then false
                                              else nextlineishorizon(tl(charfile), hd(charfile), hd(tl(tl(charfile))));

(* check if the line contains only 3 or more dashes *)
fun containsthreedashes(charfile, prev, next) = if (hd(charfile) = #"\n") then false
                                                else if (hd(charfile) = #"-" andalso prev  = #"-" andalso next  = #"-") then true
                                                else containsthreedashes(tl(charfile) , hd(charfile) , hd(tl(tl(charfile))));

(* checking the index of some char in the file*)
fun checkthischar(charfile, char, count) = if hd(charfile) = char then count
                                          else checkthischar(tl(charfile) , char, count+1); 

(* checking the index of some file while also seeing escape and line end char *)
fun givemethissymbolindex(charfile, char, count, prev) = if hd(charfile) = #"\n" then  ~1
                                            else if hd(charfile) = char andalso prev <> #"\\" then count
                                          else givemethissymbolindex(tl(charfile) , char, count+1, hd(charfile));

(* find the char index of the two double numbers together *)
fun finddoublecharindex(charfile, char, prev, count) = if hd(charfile) = #"\n" andalso prev = #"\n" then  ~1
                                           else if hd(charfile) = char andalso prev = char then count
                                          else finddoublecharindex(tl(charfile) , char, hd(charfile), count +1); 
(* check if next number is an integer *)
fun isnumber(n) = if((ord(n) < 58  andalso ord(n) > 47  )) then true
                else false

(* check if the char is char a-zA-Z *)
fun ischar(next) = if ((ord(next)>64 andalso ord(next) < 91) orelse (ord(next)>96 andalso ord(next) < 123)) then true
                    else false

(* check if the next line is a ordered list line i.e. number. regex *)
fun list_line(charfile, prev, next, count) = if hd(charfile) = #"\n" then ~1
                                          else if hd(charfile) = #" " andalso (prev = #"\n" orelse prev = #" ")  then list_line(tl(charfile), hd(charfile), hd(tl(tl(charfile))), count +1)
                                          else if isnumber(hd(charfile)) = false then count
                                          else
                                            list_line(tl(charfile), hd(charfile), hd(tl(tl(charfile))), count +1)

(* check if there is some other charcter which is basically escaped *)
fun nextinthese(char) = if (char = #"<" orelse char = #">" orelse char = #"\"" orelse char = #"&" orelse char = #"'" orelse char = #"_"
                          orelse char  = #"[" orelse char = #"]" orelse char = #")" orelse char = #"(") then (true)
                        else false   

(* the main function to handle the mdt file *)
fun handle_mdt(addstream, charfile, prev, next, stack , underlinestarted, blackquotestarted, ulstarted, olstarted) =
                let
                  (* variables of depth and present values defined using the above defined functions *)
                    val hlevel = header_level(charfile, 0)          
                    val str_there = chartostring(hd(charfile))
                    val nextlinestart = checkthischar(charfile, #"\n",  0)
                    val nextclosing = givemethissymbolindex(charfile, #">" , 0 , prev)
                    val fortable = finddoublecharindex(charfile, #">" , prev, 0)
                    val nextclosing1 = givemethissymbolindex(charfile, #"]" , 0 , prev)
                    val nextclosing2 = givemethissymbolindex(charfile, #"(" , 0 , prev)
                    val nextclosing3 = givemethissymbolindex(charfile, #")" , 0 , prev)
                    val nextdot  = givemethissymbolindex(charfile, #"." , 0 , prev)
                in
                (* if length < 2 i.e. we are in the added "\n" chars *)
                if length(charfile) =2 then (
                  TextIO.output(addstream , concat(stack))
                )
                (* handling of special chars after escaping *)
                else if (hd(charfile)= #"\\" andalso nextinthese(next)) then (
                          if (next  = #"<") then (
                            TextIO.output(addstream, "&lt;");
                            handle_mdt(addstream, tl(tl(charfile)) , next, hd(tl(tl(tl(charfile)))), stack, underlinestarted, blackquotestarted, ulstarted, olstarted)
                          )
                          else if (next = #">") then(
                            TextIO.output(addstream, "&gt;");
                            handle_mdt(addstream, tl(tl(charfile)) , next, hd(tl(tl(tl(charfile)))), stack, underlinestarted, blackquotestarted, ulstarted, olstarted)

                          )
                          else if (next = #"\"") then (
                            TextIO.output(addstream, "&quot;");
                            handle_mdt(addstream, tl(tl(charfile)) , next, hd(tl(tl(tl(charfile)))), stack, underlinestarted, blackquotestarted, ulstarted, olstarted)
                          )
                          else if (next = #"&") then (
                            TextIO.output(addstream, "&amp;");
                            handle_mdt(addstream, tl(tl(charfile)) , next, hd(tl(tl(tl(charfile)))), stack, underlinestarted, blackquotestarted, ulstarted, olstarted)
                          )
                          else if (next = #"'") then(
                            TextIO.output(addstream, "&apos;");
                            handle_mdt(addstream, tl(tl(charfile)) , next, hd(tl(tl(tl(charfile)))), stack, underlinestarted, blackquotestarted, ulstarted, olstarted)
                          )
                          else if (next = #"_") then(
                            TextIO.output(addstream, "_");
                            handle_mdt(addstream, tl(tl(charfile)) , next, hd(tl(tl(tl(charfile)))), stack, underlinestarted, blackquotestarted, ulstarted, olstarted)
                          )
                          else(
                            TextIO.output(addstream, str(next));
                            handle_mdt(addstream, tl(tl(charfile)) , next, hd(tl(tl(tl(charfile)))), stack, underlinestarted, blackquotestarted, ulstarted, olstarted)

                          )

                )
                (* the case of two \n elements i.e. list elements closing and also popping from stack *)
                else if (prev = #"\n" andalso hd(charfile) = #"\n") then (
                  if (length(stack)>0 andalso hd(stack) = "</p>" andalso ischar(next)) then(
                    TextIO.output(addstream, "</p><p>");
                    handle_mdt(addstream , tl(charfile ), hd(charfile), hd(tl(tl(charfile))), tl(stack), underlinestarted, blackquotestarted, ulstarted, olstarted)
                  )
                  else if (length(stack)>0 andalso hd(stack) = "</p>") then(
                    TextIO.output(addstream, "</p>");
                    handle_mdt(addstream , tl(charfile ), hd(charfile), hd(tl(tl(charfile))), tl(stack), underlinestarted, blackquotestarted, ulstarted, olstarted)
                  )
                  else(
                    TextIO.output(addstream , "<p>");
                      handle_mdt(addstream , tl(charfile ), hd(charfile), hd(tl(tl(charfile))), ["</p>"]@stack, underlinestarted, blackquotestarted, ulstarted, olstarted)

                  )
                )

                (* the case of opening of ordered lists *)
                else if (prev = #"\n" andalso nextdot <> ~1 andalso list_line(charfile, prev, next, 0)  = nextdot andalso List.nth(charfile, nextdot+1) = #" " andalso olstarted = false) then (
                        TextIO.output(addstream, "<ol><li>");
                        handle_mdt(addstream, List.drop(charfile, nextdot+1) , List.nth(charfile, nextdot), List.nth(charfile, nextdot+2), ["</li>" , "</ol>"]@stack, underlinestarted, blackquotestarted , ulstarted, true)
                )

                (* the case of the starting of unordered lines *)
                else if (hd(charfile) = #"-" andalso (prev  = #"\n" orelse (olstarted = true andalso prev = #" ")) andalso next = #" " andalso ulstarted = false) then (
                        TextIO.output(addstream , "<ul><li>");
                        handle_mdt(addstream, tl(charfile) , hd(charfile), hd(tl(tl(charfile))), ["</li>" , "</ul>"]@stack, underlinestarted, blackquotestarted , true, olstarted)
                )

                (* the case of ordered lines *)
                else if (prev = #"\n" andalso nextdot <> ~1 andalso list_line(charfile, prev, next, 0) = nextdot andalso List.nth(charfile, nextdot+1) = #" " andalso olstarted = true) then (
                        if (ulstarted = true andalso length(stack) >0 andalso hd(stack) <> "</ul>") then(
                          TextIO.output(addstream, hd(stack));
                          handle_mdt(addstream, charfile, prev, next, (tl(stack)) , underlinestarted, blackquotestarted, ulstarted, olstarted)
                        )
                        else(
                          TextIO.output(addstream, hd(stack));
                          (if (length(stack) >1 andalso hd(tl(stack)) = "</li>") then (
                            TextIO.output(addstream, "</li>");
                          TextIO.output(addstream , "<li>");
                          handle_mdt(addstream, List.drop(charfile, nextdot+1) ,  List.nth(charfile, nextdot),List.nth(charfile, nextdot+2), ["</li>"]@tl(tl(stack)), underlinestarted, blackquotestarted , false, true)
                          )
                          else (
                            TextIO.output(addstream , "<li>");
                            handle_mdt(addstream, List.drop(charfile, nextdot+1) , List.nth(charfile, nextdot), List.nth(charfile, nextdot+2), ["</li>"]@tl(stack), underlinestarted, blackquotestarted , false, true)                          
                          )  
                        )     
                        )           

                )
                (* the case of elements of unordered list *)
                else if (hd(charfile) = #"-" andalso (prev  = #"\n" orelse (olstarted = true andalso prev = #" " )) andalso next  =  #" " andalso ulstarted = true) then (
                        if (length(stack) >0 andalso hd(stack) = "</li>") then (
                          TextIO.output(addstream, "</li>");
                        TextIO.output(addstream , "<li>");
                        handle_mdt(addstream, tl(charfile) , hd(charfile), hd(tl(tl(charfile))), ["</li>"]@stack, underlinestarted, blackquotestarted , true, olstarted)
                        )
                        else (
                          TextIO.output(addstream , "<li>");
                          handle_mdt(addstream, tl(charfile) , hd(charfile), hd(tl(tl(charfile))), ["</li>"]@stack, underlinestarted, blackquotestarted , true, olstarted)                          
                        )
                )
                (* the case of link in the []() format *)
                else if (hd(charfile) = #"[" andalso prev <> #"\\" andalso nextclosing1 <> ~1 andalso nextclosing2 <> ~1 andalso nextclosing3 <> ~1 andalso nextclosing2 =  nextclosing1+1) then (
                        TextIO.output(addstream , "<a href =" ^ "\"" ^String.substring(String.implode(List.take(charfile, nextclosing3 +1)), nextclosing2 +1 , nextclosing3-nextclosing2-1)  ^ "\""^ ">" ^String.implode(List.take(tl(charfile) , nextclosing1-1)) ^ "</a>" );
                        handle_mdt(addstream , List.drop(charfile, nextclosing3+1) , #">" , List.nth(charfile , nextclosing3+2) , stack, underlinestarted, blackquotestarted, ulstarted, olstarted)
                )
                (* the case of table *)
                else if (hd(charfile) = #"<" andalso next  = #"<" andalso prev <> #"\\" andalso fortable <> ~1) then (
                          TextIO.output(addstream  ,"<CENTER><TABLE border=\"1\">\n<tr><td>");
                          handle_csv((List.take (tl(tl(tl(charfile))), fortable-3)) , addstream, prev);
                          handle_mdt(addstream , List.drop(charfile, fortable+1), #">" , List.nth(charfile, fortable+2), stack, underlinestarted, blackquotestarted, ulstarted, olstarted)
                )
                (* the case of closing of blockquotes *)
                else if (hd(charfile) = #"\n" andalso length(stack) > 0 andalso hd(stack) = "</blockquote>") then 
                          ( TextIO.output(addstream , hd(stack));
                            handle_mdt(addstream , (charfile) ,prev , next , tl(stack), underlinestarted, true, ulstarted, olstarted)
                          )
                          (* the blockquote case handled here all the stack elements popped *)
                else if (prev = #"\n" andalso hd(charfile) = #">") then (                  
                        if (length(stack) > 0 andalso hd(stack) = "</li>") then(
                          TextIO.output(addstream, hd(stack));
                          handle_mdt(addstream, charfile, prev, next, tl(stack), underlinestarted, blackquotestarted, ulstarted, olstarted)
                        )
                        else if (length(stack) > 0 andalso hd(stack) = "</ul>") then(
                          TextIO.output(addstream, hd(stack));
                          handle_mdt(addstream, charfile, prev, next, tl(stack), underlinestarted, blackquotestarted, false, olstarted)
                        )
                        else if (length(stack) > 0 andalso hd(stack) = "</ol>") then(
                          TextIO.output(addstream, hd(stack));
                          handle_mdt(addstream, charfile, prev, next, tl(stack), underlinestarted, blackquotestarted, ulstarted, false)
                        )
                        else(
                        TextIO.output(addstream , "<blockquote>");
                        handle_mdt(addstream , tl(charfile), hd(charfile) , hd(tl(tl(charfile))) , ["</blockquote>"]@stack, underlinestarted, true, ulstarted, olstarted))
                )
                else if ((prev = #" " orelse prev = #">") andalso hd(charfile) = #">" andalso blackquotestarted = true) then (
                        TextIO.output(addstream , "<blockquote>");
                        handle_mdt(addstream , tl(charfile), hd(charfile) , hd(tl(tl(charfile))) , ["</blockquote>"]@stack, underlinestarted, true, ulstarted, olstarted)
                )
                (* the case of blackquote nesting *)
                else if (blackquotestarted =  true andalso hd(charfile) = #" " andalso next  = #">") then(
                      handle_mdt(addstream , tl(charfile), hd(charfile) , hd(tl(tl(charfile))) ,stack, underlinestarted, true, ulstarted, olstarted)
                ) 

                else if (hd(charfile) = #"<" andalso prev <> #"\\" andalso length(charfile) > nextclosing andalso List.nth(charfile ,1 ) = #"h" andalso List.nth(charfile ,2 ) = #"t" andalso List.nth(charfile ,3 ) = #"t" andalso List.nth(charfile ,4 ) = #"p" andalso givemethissymbolindex(charfile, #">" , 0, prev) <> ~1)  then (
                      TextIO.output(addstream , "<a href =" ^ "\"" ^String.implode(List.take(tl(charfile) , nextclosing-1)) ^ "\""^ ">" ^String.implode(List.take(tl(charfile) , nextclosing-1)) ^ "</a>" );
                      handle_mdt(addstream , List.drop(charfile, nextclosing+1) , #">" , List.nth(charfile , nextclosing+2) , stack, underlinestarted, blackquotestarted, ulstarted, olstarted)

                )
                else if ((hd(charfile) = #" " orelse hd(charfile) = #"-") andalso prev  = #"\n" andalso nextlineishorizon(charfile, prev, hd((tl(charfile)))) andalso containsthreedashes((charfile), prev, hd((tl(charfile))))) then (
                      TextIO.output(addstream , concat(stack));
                      TextIO.output(addstream, "<hr>");
                      handle_mdt(addstream , List.drop(charfile, nextlinestart) , List.nth(charfile, nextlinestart-1), List.nth(charfile, nextlinestart+1) , [] , underlinestarted, blackquotestarted, ulstarted, olstarted)
                )
                (* the case of bolding of elements closing *)
                else if (hd(charfile) = #"*" andalso next = #"*" andalso prev <> #"\\" andalso length(stack) >0 andalso hd(stack) = "</strong>") then 
                (
                  TextIO.output(addstream, hd(stack));
                      handle_mdt(addstream , tl(tl(charfile)) , #"*" , hd(tl(tl(tl(charfile)))) , tl(stack), underlinestarted, blackquotestarted, ulstarted, olstarted)
                )
                (* the case of bold chars start *)
                else if (isbold(charfile, prev, next)) then(
                      TextIO.output(addstream, "<strong>");
                      handle_mdt(addstream , tl(tl(charfile)) , #"*" , hd(tl(tl(tl(charfile)))) , ["</strong>"]@(stack), underlinestarted, blackquotestarted, ulstarted, olstarted)
                )
                (* the case of italic closing *)
                else if (hd(charfile) = #"*" andalso prev <> #"\\" andalso length(stack) >0 andalso hd(stack) = "</i>") then 
                (
                  TextIO.output(addstream, hd(stack));
                      handle_mdt(addstream , (tl(charfile)) , #"*" , hd((tl(tl(charfile)))) , tl(stack), underlinestarted, blackquotestarted, ulstarted, olstarted)
                )
                (* the case of italic starting *)
                else if (isitalic(charfile, prev, next)) then (
                      TextIO.output(addstream, "<i>");
                      handle_mdt(addstream , (tl(charfile)) , #"*" , hd((tl(tl(charfile)))) , ["</i>"]@(stack), underlinestarted, blackquotestarted, ulstarted, olstarted)                  
                )
                (* starting of underline *)
                else if (isunderline(prev,  charfile)) then  (
                  TextIO.output(addstream, "<u>");
                  handle_mdt(addstream , tl(charfile) , hd(charfile) , hd(tl(tl(charfile))) , ["</u>"]@(stack), true, blackquotestarted, ulstarted, olstarted)

                )
                (* the case of betweenscore case *)
                else if (isinbetweenunderline(prev,  charfile , next , underlinestarted)) then  (
                  TextIO.output(addstream, " ");
                  handle_mdt(addstream , tl(charfile) , hd(charfile) , hd(tl(tl(charfile))) , (stack), true,  blackquotestarted, ulstarted, olstarted)
                )
                (* the final underscore case *)
                else if (isfinalunderline(charfile, next, prev , underlinestarted) andalso length(stack) > 0 andalso String.substring(hd(stack) ,  0, 3) = "</u") then (
                  TextIO.output(addstream , hd(stack));
                  handle_mdt(addstream , tl(charfile) , hd(charfile) , hd(tl(tl(charfile))) , tl(stack), false, blackquotestarted, ulstarted, olstarted)
                )
                else if (hd(charfile) = #"\n" andalso length(stack) > 0 andalso String.substring(hd(stack) ,  0, 3) = "</h") then 
                          ( TextIO.output(addstream , hd(stack));
                            handle_mdt(addstream , (charfile) , prev , hd(tl((charfile))) , tl(stack), underlinestarted, blackquotestarted, ulstarted, olstarted)
                          )
                          (* the case of header *)
                else if (isheader(prev, charfile)) then(
                    if (length(stack)> 0 andalso hd(stack) <> "</blockquote>") then (
                      TextIO.output(addstream, hd(stack));
                      handle_mdt(addstream, charfile, prev, next, tl(stack) , underlinestarted, blackquotestarted, ulstarted, olstarted)
                    )
                    else (
                    TextIO.output(addstream , "<h"^ inttostring(hlevel) ^ ">");
                    handle_mdt(addstream , List.drop(charfile, hlevel), List.nth(charfile, hlevel-1),  List.nth(charfile, hlevel+1) , ["</h"^ inttostring(hlevel) ^ ">"]@stack, underlinestarted, blackquotestarted, false, false)
                    )
                )
                else( 
                  (* remaining cases when "\n" and other chars are there *)
                    if (hd(charfile) = #"\n" andalso prev  = #"\n") then (
                        handle_mdt(addstream , tl(charfile ), hd(charfile), hd(tl(tl(charfile))), stack, underlinestarted, false, ulstarted, olstarted))

                    
                  else if (hd(charfile)  = #"\n") then(
                        TextIO.output(addstream, " ");
                        handle_mdt(addstream , tl(charfile), hd(charfile), hd(tl(tl(charfile))), stack, underlinestarted, false, ulstarted, olstarted))
                      else (
                           (TextIO.output(addstream , str(hd(charfile)));
                        handle_mdt(addstream , tl(charfile), hd(charfile), hd(tl(tl(charfile))), stack, underlinestarted, blackquotestarted, ulstarted, olstarted))
                          
                        )
                )
                
                end;

fun mdt2html(filename)=
  let
    (* variables for changing the file names and opening the in and out files for appending, adding *)
    val filesize = String.size(filename)
    val isfilecsv = String.substring(filename , filesize-3 , 3)
    val outfilename = String.substring(filename , 0 , filesize-3) ^ "html"
    val instream = TextIO.openIn filename
    val outstream = TextIO.openOut outfilename
    val addstream = TextIO.openAppend outfilename
    val myinstream = TextIO.getInstream(TextIO.openIn filename)
  in
    (* forming the file just for writing and handling the csv files case *)
    (TextIO.output(outstream, ""); 
    if (isfilecsv  = "csv") then (TextIO.output(addstream  ,"<CENTER><TABLE border=\"1\">\n<tr><td>");
                            handle_csv(get_char_file(myinstream) , addstream , #"<")
                )
    else 
           (
            (* closing all the streams and files indice after handling the mdt file *)
            handle_mdt(addstream , get_char_file(myinstream)@[#"\n" , #"\n"], #"\n" , hd(tl(get_char_file(myinstream))) ,  [] , false , false, false, false);
    (TextIO.StreamIO.closeIn; 
    TextIO.closeIn(instream);
    TextIO.closeOut(outstream);
    TextIO.closeOut(addstream))))
  end;





