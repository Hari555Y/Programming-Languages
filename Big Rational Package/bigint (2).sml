signature BIGINT = 
sig 
    type bigint
    exception Berror
    val zero : bigint
    val one : bigint
    val nega : bigint -> bigint
    val abs : bigint -> bigint
    val fromint : int -> bigint 
    val toint  : bigint -> int
    val samesign : bigint * bigint -> bool
    val tostring : bigint -> string
    val fromstring : string -> bigint
    val compare : bigint * bigint -> order
    val addi : bigint * bigint -> bigint
    val subt : bigint * bigint -> bigint 
    val multiply_prev : bigint * bigint -> bigint
    val givemestring : int list * string -> string
    (* val giverem : int list * int list * int * int * int * int list  -> int list; *)
    val givemeremainder : int list * int list -> int list
    val givemequotient : int list * int list -> int list
    val divi : bigint * bigint -> bigint
    val modu : bigint * bigint -> bigint  
end;


structure BigInt : BIGINT =
struct 

    type bigint = int * (int list)
    exception Berror;

    val zero = (0, [0])

    val one =   (1, [1])

    fun nega (tup : int * (int list) ) =
     (* if (#1 tup <> 0 andalso length(#2 tup) = 0) then raise Berror *)
     if (#1 tup = 0 andalso (length(#2 tup) <> 1 orelse hd(#2 tup) <> 0)) then raise Berror 
    else (~1*(#1 tup) , #2 tup)
    ;

    fun abs (tup : int * (int list)) = 
     (* if (#1 tup <>0 andalso length(#2 tup) = 0) then raise Berror *)
     if (#1 tup = 0 andalso (length(#2 tup) <> 1 orelse hd(#2 tup) <> 0)) then raise Berror 
    else if (#1 tup = 0) then (0, [0])
                    else (1, #2 tup)
    ;

    fun findmultcarry(a, b , c) = 
      let
      (* converting a and b to integer and then finding carry by dividing the result by 10  *)
      val x : int = valOf(Int.fromString(a)) 
      val y : int = valOf(Int.fromString(b))
      val m : int = ((x*y + c) div 10)
      in 
      (* returing the new integer m *)
        m
    end;

    fun findmultchar(a, b, c) = 
      let
      (* converting a and b to integer and then finding unit place by dividing the result by 10  and taking modulo *)
      val x : int = valOf(Int.fromString(a)) 
      val y :int =  valOf(Int.fromString(b))
      val m :int = ((x*y + c) mod 10)
      val masstring : string =  Int.toString(m);
      in 
      (* returing the new string massstring*)
        masstring
    end;

    (* function to convert character c to a string str *)
    fun chartostring(c) =
      let 
        val str : string = Char.toString(c)
      in 
        str
    end;

    fun givemelist(i , ls) = if (i = 0) then ls
                else givemelist(i div 10 , ls @ [i mod 10])
    ;

    fun fromint (i) = 
        if i =0  then (0, [0])
        else if i >0 then (1, rev (givemelist(i, [])))
        else (~1, rev (givemelist(~i, [])))
    ;

    fun givemeint (ls, i)  = if (length(ls) = 0) then i
                else givemeint (tl(ls) , 10* i  + hd(ls))
    ;

    fun toint (tup : int * (int list)) =
     (* if (#1 tup <> 0 andalso length(#2 tup) = 0) then raise Berror *)
             if (#1 tup = 0 andalso (length(#2 tup) <> 1 orelse hd(#2 tup) <> 0)) then raise Berror 
            else if (#1 tup = 0) then 0
            else if (#1 tup = 1) then givemeint(#2 tup , 0)
            else ~1 * givemeint(#2 tup, 0)
    ;

    fun samesign (tup1 : int * (int list), tup2 : int * (int list)) = if (#1 (tup1) * #1 (tup2) >=0) then true
        else false
    ;

    fun givemestring(ls, str) =  if (length(ls)=0) then str
                else givemestring (tl(ls) , str ^ Int.toString(hd(ls)))
    ;

    (* this is  a function to convert an integer a to string my_str *)
    fun inttomystring(a) =
      let 
      val my_str : string = Int.toString(a)
      in 
        my_str
    end;

    fun strip(a,[]) = []
    |             strip(a, b::y) = if a=chartostring(b)  then strip(a,y)
                    else b::y;
    
    fun striptolast(a, arr) = if (length(arr)=0) then [0]
    else if (length(arr)=1) then arr
    else if a=inttomystring(hd(arr))  then striptolast(a,tl(arr))
    else arr;


    fun tostring (tup : int * (int list)) = 
    (* if (#1 tup <> 0 andalso length(#2 tup) = 0) then raise Berror *)
              if (length(#2 tup) = 0) then ""
             else if (#1 tup = 0 andalso (length(#2 tup) <> 1 orelse hd(#2 tup) <> 0)) then raise Berror
            else if (#1 tup  = 0) then "0"
          else if (#1 tup = 1) then givemestring( striptolast ("0" ,#2 tup) , "")
          else "~" ^ givemestring(striptolast ("0" ,#2 tup), "")
    ;


    fun chartoint(c) =
      let
        val my_str : string = str (c)
        val my_int : int = valOf(Int.fromString(my_str))
      in
        my_int
      end
    ;
    
	(* this is a function which takes a string c and return it's ith character into integer form*)
fun chartointinstring(c , i) =
	let
		(*  my_str is the string which is basically the character at the ith position*)
	  val my_str : string = chartostring(String.sub(c, i))
	  	(* converting my_str into an integer my_int *)
	  val my_int : int = valOf(Int.fromString(my_str))
	in
	  my_int
	end



    fun givebackarray(st, arr) = if (size(st) =0) then arr
    else givebackarray(String.extract(st, 1, NONE) , arr@[chartoint(String.sub(st, 0))] )
    ;

    fun containonepositive(st , i) = if (i= size(st)) then false
    else if (String.sub(st, i)<> #"~" andalso  String.sub(st, i)<> #"0") then true
    else containonepositive(st, i+1);

    fun fromstring(st) = if (size(st)> 0 andalso str (String.sub (st, 0)) = "~") then (~1, striptolast("0", givebackarray(String.extract(st,1, NONE), [])))
      else if (containonepositive(st, 0)) then(1, striptolast("0", givebackarray(st, [])))
      else (0, [0])

    ;

    fun checkpositive (ls1, ls2) = if (length(ls1) = 0 andalso length(ls2) = 0) then EQUAL
                          else if (length(ls1) > length(ls2)) then GREATER  
                          else if (length(ls1) < length(ls2)) then LESS
                          else if (hd(ls1)  < hd(ls2)) then LESS
                          else if (hd(ls1) > hd(ls2)) then GREATER
                          else checkpositive (tl(ls1), tl(ls2));

    fun checknegative(ls1, ls2) = if (length(ls1) = 0 andalso length(ls2) = 0) then EQUAL
                          else if (length(ls1) > length(ls2)) then LESS  
                          else if (length(ls1) < length(ls2)) then GREATER
                          else if (hd(ls1)  < hd(ls2)) then GREATER
                          else if (hd(ls1) > hd(ls2)) then LESS
                          else checknegative (tl(ls1), tl(ls2));

    fun compare (tup1 : int * (int list), tup2 : int * (int list)) = if (#1 tup1 =0 andalso #1 tup2 =0) then EQUAL
          else if (#1 tup1 = 1 andalso #1 tup2  <=0) then GREATER
          else if (#1 tup1 <=0 andalso #1 tup2 = 1) then LESS
          else if (#1 tup1 = 1 andalso #1 tup2 = 1) then checkpositive (striptolast("0", #2 tup1), striptolast("0", #2 tup2))
          else checknegative (striptolast("0", #2 tup1), striptolast("0", #2 tup2))
    ;

    fun eletoarr(i) =
    let
    val imy : int = i
    val myarr : int list = imy::[]

    in
      myarr
    end;

    (* this is a function to convert string s into an integer my_int *)
  fun stringtoint(s) =
    let 
    val  my_int : int  = valOf(Int.fromString(s))
    in
      my_int
    end;

    fun sub(a, b , i,  m , n , carry)  =  if (i>=m) then ""
										(* if index is greater than greater of the two lengths then we will just return empty string *)
										(* if index is greater than the smaller string and less than larger then we will just see the case whether the carry thing is negative or not 
										   if it is negative then we will just add "9" and then move forward the carry to the next index. *)
									else if (i>=n) then
											if ((carry + stringtoint(chartostring(String.sub(a, m-i-1)))) = ~1) then sub(a,b,i+1, m,n,~1) ^ "9"
										(* else we will just move 0 as the carry to the next index and add the string part of the carry added to that particular index of the larger index *)
											else sub(a,b,i+1, m,n,0) ^ inttomystring(chartointinstring(a, m-i-1) + carry)
									else 
										(* else we will just divide it into two cases which include when carry +  larger string index greater than smaller index then we will just move forward with 0 carry
										otherwise move the -1 carry forward to next index and append the remaining thing to the next recursion call *)
										if (carry + chartointinstring(a, m-i-1) < chartointinstring(b, n-i-1)) then sub(a,b,i+1,m,n , ~1) ^ inttomystring((chartointinstring(a, m-i-1) +carry +10 -(chartointinstring(b, n-i-1))) mod 10)
										else sub(a, b , i+1, m , n , 0) ^ inttomystring(chartointinstring(a, m-i-1) + carry - (chartointinstring(b,n-i-1)));


    fun givecorrectorder(tup1 : int * (int list), tup2 : int * (int list) ,a,b,m,n) =
      if compare ((1 , #2 tup1), (1, #2 tup2)) = GREATER then
        sub(a, b , 0 , m , n ,0)
      else 
        sub(b, a, 0, n, m, 0)
    ;

    fun finalpossub(tup1 : int * (int list), tup2 : int * (int list)) =
      let 
      (* this gives us the normal subtraction of the two strings *)
      val a = givemestring(#2 tup1 , "")
      val b =  givemestring(#2 tup2 , "")
      val m = size(a)
      val n = size(b)
      val str1 = givecorrectorder(tup1, tup2 ,a,b,m,n)
      (* convert the string to array using explode *)
      val  p = explode str1
      (* this will be used to compare in the strip function defined above *)
      val mt  : string = "0"
      (* this will give the list after removing the zeroes using strip function *)
      val new_p  : char list = strip( mt, p )
      val new_str : string  = implode new_p
      val return_int_list  = givebackarray(new_str , [])
      in
        (* this will return us the string formed from new_p list *)
        return_int_list
      end;

    fun add(a, b , i,  m , n , carry)  =  if (i>=m andalso carry =0) then ""
                  else if (i >=m) then "1"
									else if (i>=n) then 
                      add(a, b , i+1, m , n , (chartointinstring(a, m-i-1) + carry) div 10) ^ inttomystring((chartointinstring(a, m-i-1) +carry) mod 10)								                 
                  else 
                  add(a, b , i+1, m , n , (chartointinstring(a, m-i-1) + carry + chartointinstring(b, n-i-1)) div 10) ^ inttomystring((chartointinstring(a, m-i-1) +carry + chartointinstring(b, n-i-1)) mod 10);

    fun givecorrectorder2(tup1 : int * (int list), tup2 : int * (int list) ,a,b,m,n) =
      if compare ((1 , #2 tup1), (1, #2 tup2)) = GREATER then
        add(a, b , 0 , m , n ,0)
      else 
        add(b, a, 0, n, m, 0)
    ;

    fun finalpossadd(tup1 : int * (int list), tup2 : int * (int list)) =
      let 
      (* this gives us the normal subtraction of the two strings *)
      val a = givemestring(#2 tup1 , "")
      val b =  givemestring(#2 tup2 , "")
      val m = size(a)
      val n = size(b)
      val str1 =  givecorrectorder2(tup1, tup2 ,a,b,m,n)
      (* convert the string to array using explode *)
      val  p = explode str1
      (* this will be used to compare in the strip function defined above *)
      val mt  : string = "0"
      (* this will give the list after removing the zeroes using strip function *)
      val new_p  : char list = strip( mt, p )
      val new_str : string  = implode new_p
      val return_int_list  = givebackarray(new_str, [])
      in
        (* this will return us the string formed from new_p list *)
        return_int_list
      end;

    fun addi(tup1 : int * (int list) , tup2 : int * (int list)) = 
    (* if (#1 tup1 <> 0 andalso length(#2 tup1) = 0) then raise Berror *)
            if (length(#2 tup1) = 0) then tup2
             else if (#1 tup1 = 0 andalso (length(#2 tup1) <> 1 orelse hd(#2 tup1) <> 0)) then raise Berror
            (* else if (#1 tup2 <> 0 andalso length(#2 tup2) = 0) then raise Berror *)
            else if (length(#2 tup2) =0) then tup1
            else if (#1 tup2 = 0 andalso (length(#2 tup2) <> 1 orelse hd(#2 tup2) <> 0)) then raise Berror
            else if (#1 tup1 = 0) then (#1 tup2 ,striptolast("0", #2 tup2))
            else if (#1 tup2 = 0)  then (#1 tup1 ,striptolast("0", #2 tup1))
          else if (#1 tup1 >=0 andalso #1 tup2 >=0) then (1,finalpossadd(tup1, tup2))
        else if (#1 tup1 <0 andalso #1 tup2 <0) then  (~1,finalpossadd(tup1, tup2))
        else if (#1 tup1 >=0 andalso #1 tup2 <0) then (
            if (checkpositive(striptolast("0" ,#2 tup1), striptolast("0", #2 tup2)) = GREATER) then (1, finalpossub(tup1, tup2))
            else if (checkpositive(striptolast("0" , #2 tup1), striptolast("0", #2 tup2)) = LESS) then (~1 , finalpossub(tup1, tup2))
            else (0, [0]))
        else (
            if (checkpositive(striptolast("0", #2 tup1), striptolast("0", #2 tup2)) = GREATER) then (~1, finalpossub(tup1, tup2))
            else if (checkpositive(striptolast("0", #2 tup1), striptolast("0" , #2 tup2)) = LESS) then (1 , finalpossub(tup1, tup2))
            else (0, [0]))
    ;

    fun subt(tup1 : int * (int list), tup2 : int * (int list)) = 
    (* if (#1 tup1 <> 0 andalso length(#2 tup1) = 0) then raise Berror *)
              if (length(#2 tup2) = 0) then tup1
              else if (length(#2 tup1) =0) then nega(tup2)
             else if (#1 tup1 = 0 andalso (length(#2 tup1) <> 1 orelse hd(#2 tup1) <> 0)) then raise Berror
            (* else if (#1 tup2 <> 0 andalso length(#2 tup2) = 0) then raise Berror *)
            else if (#1 tup2 = 0 andalso (length(#2 tup2) <> 1 orelse hd(#2 tup2) <> 0)) then raise Berror
            else if (#1 tup1 = 0) then nega( (#1 tup2, striptolast("0", #2 tup2)))
            else if (#1 tup2 = 0)  then (#1 tup1 ,striptolast("0", #2 tup1))
            else if (#1 tup1 >=0 andalso #1 tup2 <0) then (1,finalpossadd(tup1, tup2))
            else if (#1 tup1 <0 andalso #1 tup2 >=0) then (~1,finalpossadd(tup1, tup2))
            else if (#1 tup1 >=0 andalso #1 tup2 >=0) then(
            if (checkpositive(striptolast("0", #2 tup1), striptolast("0", #2 tup2)) = GREATER) then (1, finalpossub(tup1, tup2))
            else if (checkpositive(striptolast("0", #2 tup1),striptolast("0" ,#2 tup2)) = LESS) then (~1 , finalpossub(tup1, tup2))
            else (0, [0]))
    else(
            if (checkpositive(striptolast("0", #2 tup1), striptolast("0", #2 tup2)) = GREATER) then (~1, finalpossub(tup1, tup2))
            else if (checkpositive(striptolast("0", #2 tup1),striptolast("0", #2 tup2)) = LESS) then (1 , finalpossub(tup1, tup2))
            else (0, [0]))
    ;

    fun givei(i, st) = if (i=0) then st
        else givei(i-1, st ^"0");

    fun mult_one(a, b , m , i, carry) = if (i=m) then if carry =0 then "" else inttomystring(carry)
								(* if index is equal to m then we will return either the carry as string if not 0 otherwise return empty string *)
								(* recursvely call the mult function with index as i+1 and the carry by findmultcarry function and append the 
								new string/char as string obtained by the function findmultchar *)
								else mult_one(a, b, m , i+1, findmultcarry( chartostring(String.sub(a, m-i-1)) , b , carry)) ^ findmultchar(chartostring(String.sub(a,m-i-1)),b, carry);

    fun mult_any (a, b , m , n ,i , mystr) = if (i =n) then  mystr
              else if (size(mystr) >= size(mult_one (a , str(String.sub(b , n-i-1)) , m , 0, 0) ^ givei(i, ""))) then
                   mult_any ( a, b , m , n , i+1, add(mystr, mult_one (a , str(String.sub(b , n-i-1)) , m , 0, 0) ^ givei(i, "") , 0, size(mystr), size(mult_one (a , str (String.sub(b , n-i-1)) , m , 0, 0) ^ givei(i, ""))  , 0 ) )
              else (
                mult_any ( a, b , m , n , i+1, add(mult_one (a , str(String.sub(b , n-i-1)) , m , 0, 0) ^ givei(i, "") , mystr, 0, size(mult_one (a , str(String.sub(b , n-i-1)) , m , 0, 0) ^ givei(i, "")) , size(mystr) , 0 ) )
              );

   
    fun mult(tup1 : int * (int list), tup2 : int * (int list)) = 
        let 
        val sign  = (#1 tup1) * (#1 tup2)
        val mymultiplication = mult_any( givemestring(#2 tup1, "") , givemestring(#2 tup2, "") , size(givemestring(#2 tup1, "") ) ,  size(givemestring(#2 tup2, "")) , 0, "" )
        val givemyarray = striptolast("0", givebackarray(mymultiplication , []))
        in
          (sign , givemyarray)

    end;

    fun multiply_prev(tup1 : int * (int list), tup2 : int * (int list)) = 
    (* if (#1 tup1 <> 0 andalso length(#2 tup1) = 0) then raise Berror *)
              if (length(#2 tup1) =0 orelse length(#2 tup2) =0) then (0, [0])
             else if (#1 tup1 = 0 andalso (length(#2 tup1) <> 1 orelse hd(#2 tup1) <> 0)) then raise Berror
            (* else if (#1 tup2 <> 0 andalso length(#2 tup2) = 0) then raise Berror *)
            else if (#1 tup2 = 0 andalso (length(#2 tup2) <> 1 orelse hd(#2 tup2) <> 0)) then raise Berror
            else if (#1 tup1 = 0) then (0 , [0])
            else if (#1 tup2 = 0)  then (0 ,[0])
            else mult(tup1, tup2);



fun multis(a, b , m , i, carry) = if (i=m) then if carry =0 then "" else inttomystring(carry)
								(* if index is equal to m then we will return either the carry as string if not 0 otherwise return empty string *)
								(* recursvely call the mult function with index as i+1 and the carry by findmultcarry function and append the 
								new string/char as string obtained by the function findmultchar *)
								else multis(a, b, m , i+1, findmultcarry( chartostring(String.sub(a, m-i-1)) , b , carry)) ^ findmultchar(chartostring(String.sub(a,m-i-1)),b, carry);

	(* this recursion function gives us the value of the unit digit which is to appended to the divisor and also multiplied with the formed string *)
    fun crctvalue(my_divisor, i , tobefound )  = 
			(* if i>9 then we will just return "9" because it is the largest one digit integer *)
			if (i > 9) then inttomystring(9)
			(* if size of the new formed string is greater than what needs to be found (tobefound) just return (i-1) as the string *)
			else if (String.size(multis(my_divisor , inttomystring(i) , String.size(my_divisor) ,  0 , 0 )) > String.size(tobefound)) then inttomystring(i-1)
			else
				(* if the size is less than that of the string to be found we repeat the recursion with the digit as i+1 *)
				if  (String.size(multis(my_divisor , inttomystring(i) , String.size(my_divisor) ,  0 , 0 )) < String.size(tobefound)) then crctvalue(my_divisor, i+1, tobefound)
				else
					(* if the comparison of the string formed is > the string needed then we will just return i-1 else just call the recursion again *)
				 	if (multis(my_divisor , inttomystring(i) , String.size(my_divisor) ,  0 , 0 ) > tobefound) then inttomystring(i-1)
					else
						crctvalue(my_divisor, i+1, tobefound);

    fun giverem(ls1, ls2, m , n , i, rem) = if (i=m) then rem
      else if (compare( (1,rem @ eletoarr(List.nth(ls1 , i)) ) , (1,ls2)) = LESS) then (
        (* print("in less" ^ "\n");
        print (givemestring(rem @ eletoarr(List.nth(ls1 , i)), "") ^ "\n"); *)
        giverem(ls1, ls2, m ,n, i+1 ,  striptolast( "0", rem @ eletoarr(List.nth(ls1, i))) )
      )
      else (
        (* print("in more" ^ "\n");
        print (givemestring(rem @ eletoarr(List.nth(ls1 , i)), "") ^ "\n"); *)
        (* print (finalpossub( (1 , rem @ eletoarr(List.nth(ls1 , i))) ,  multiply_prev(    (1, ls2 ) , (1, givebackarray(crctvalue( givemestring(ls2, "") , 0 , givemestring( rem @ eletoarr(List.nth(ls1 , i)), "")     ) ,[]  )  )       ))); *)
        (* print (  givemestring( rem @ eletoarr(List.nth(ls1 , i)), "") ^ "\n");
        print (   crctvalue( givemestring(ls2, "") , 0 , givemestring( rem @ eletoarr(List.nth(ls1 , i)), "")) ^ "\n"     ); *)
        giverem( ls1 , ls2 , m ,n , i+1 , striptolast("0", finalpossub( (1 , rem @ eletoarr(List.nth(ls1 , i))) ,  multiply_prev(    (1, ls2 ) , (1, givebackarray(crctvalue( givemestring(striptolast ("0",ls2), "") , 0 , givemestring( striptolast ("0", rem @ eletoarr(List.nth(ls1 , i))), "")     ) ,[]  )  )       )) ))
      )
    ;

    fun givequo(ls1, ls2, m , n , i, rem, quo) = if (i=m) then quo
      else if (compare( (1,rem @ eletoarr(List.nth(ls1 , i))) , (1,ls2)) = LESS) then givequo(ls1, ls2, m ,n, i+1 , striptolast("0",rem @ eletoarr(List.nth(ls1, i))) , striptolast("0",quo @ eletoarr(0)))
      else givequo( ls1 , ls2 , m ,n , i+1 , finalpossub( (1 , striptolast("0",rem @ eletoarr(List.nth(ls1 , i))))        ,  multiply_prev( (1, ls2 ) , (1, givebackarray(crctvalue( givemestring(ls2, "") , 0 , givemestring( striptolast("0", rem @ eletoarr(List.nth(ls1 , i))), "")     ) ,[]  )  )         )) ,   striptolast("0" , quo @ eletoarr (stringtoint( crctvalue( givemestring(ls2, "") , 0 , givemestring( striptolast( "0", rem @ eletoarr(List.nth(ls1 , i))), "")   )  ) ))          )    ;

    fun givemeremainder(ls1, ls2) =
    let
      val myrem = giverem(ls1, ls2, length(ls1) , length(ls2) , 0 , [])
      val rem_after = striptolast("0" , myrem)

    in
       rem_after
    end;

    fun givemequotient(ls1, ls2) =
    let
      val myquo = givequo(ls1, ls2, length(ls1) , length(ls2) , 0 , [], [])
      val quo_after = striptolast("0" , myquo)

    in
       quo_after
    end;

    fun modu (tup1 : int * (int list), tup2 : int * (int list)) = 
        if (length(givemeremainder(striptolast( "0", #2 tup1), striptolast("0" ,#2 tup2))) =0 orelse (length(givemeremainder(striptolast( "0", #2 tup1), striptolast("0" ,#2 tup2))) =1 andalso hd(givemeremainder(striptolast( "0", #2 tup1), striptolast("0" ,#2 tup2)))=0)) then
        (0,[0])
        else if (samesign(tup1, tup2)) then 
             (1 , givemeremainder(striptolast( "0", #2 tup1), striptolast("0" ,#2 tup2)))
        else 
          addi ( (~1 , givemeremainder(striptolast( "0", #2 tup1), striptolast("0" ,#2 tup2))), tup2 )
    ;

   fun divi (tup1 : int * (int list) , tup2 : int * (int list)) = 
        if (length(givemequotient(striptolast("0" ,#2 tup1), striptolast("0", #2 tup2))) =0 orelse (length(givemequotient(striptolast("0" ,#2 tup1), striptolast("0", #2 tup2))) =1 andalso hd(givemequotient(striptolast("0" ,#2 tup1), striptolast("0", #2 tup2)))=0)) then
        (0,[0])
        else if (samesign(tup1, tup2)) then 
             (1 , givemequotient(striptolast("0" ,#2 tup1), striptolast("0", #2 tup2)))
        else 
          (~1 , givemequotient(striptolast("0" ,#2 tup1), striptolast("0", #2 tup2)))
    ;
      
end;

open BigInt;




