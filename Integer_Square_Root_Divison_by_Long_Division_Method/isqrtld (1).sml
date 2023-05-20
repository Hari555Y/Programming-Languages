 	(* it is a function to multiply two strings also by adding carry c and then getting the new carry*)
  	(* a and b are given as strings whereas c is an integer which denotes the carry *)
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


 	(* it is a function to multiply two strings also by adding carry c and then getting the new string at the unit place*)
  	(* a and b are given as strings whereas c is an integer which denotes the carry *)
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

	(* this is  a function to convert an integer a to string my_str *)
fun inttomystring(a) =
	let 
	val my_str : string = Int.toString(a)
	in 
		my_str
	end;

	(* this is a function to convert string s into an integer my_int *)
fun stringtoint(s) =
	let 
	val  my_int : int  = valOf(Int.fromString(s))
	in
		my_int
	end;

	(* this is a function to add two strings a and b and return a new string m as answer *)
fun addasstring(a, b) =
	let
		val m : string = Int.toString(a+b)
	in
	  m
	end;

	(* function to convert character c to a string str *)
fun chartostring(c) =
	let 
		val str : string = Char.toString(c)
	in 
		str
	end;

	(* this is a function which takes a string c and return it's ith character into integer form*)
fun chartoint(c , i) =
	let
		(*  my_str is the string which is basically the character at the ith position*)
	  val my_str : string = chartostring(String.sub(c, i))
	  	(* converting my_str into an integer my_int *)
	  val my_int : int = valOf(Int.fromString(my_str))
	in
	  my_int
	end

	(* this is a function which takes an array [] and an string and strip the string from starting index to give back a new array which definitely doesn't contain a in the starting indices *)
fun strip(a,[]) = []
|             strip(a, b::y) = if a=chartostring(b)  then strip(a,y)
							   else b::y;

	(* this is a function which is used to multiply a string a of length m with a string b of length 1 
	here carry denotes the carry which will be carried over in the next recursion call and i is the indexing thing in the string a *)
fun mult(a, b , m , i, carry) = if (i=m) then if carry =0 then "" else inttomystring(carry)
								(* if index is equal to m then we will return either the carry as string if not 0 otherwise return empty string *)
								(* recursvely call the mult function with index as i+1 and the carry by findmultcarry function and append the 
								new string/char as string obtained by the function findmultchar *)
								else mult(a, b, m , i+1, findmultcarry( chartostring(String.sub(a, m-i-1)) , b , carry)) ^ findmultchar(chartostring(String.sub(a,m-i-1)),b, carry);

	(* this is a function which takes as input two strings a and b of length m and n with the carry thing in it and i is the indexing in the string *)
fun sub(a, b , i,  m , n , carry)  =  if (i>=m) then ""
										(* if index is greater than greater of the two lengths then we will just return empty string *)
										(* if index is greater than the smaller string and less than larger then we will just see the case whether the carry thing is negative or not 
										   if it is negative then we will just add "9" and then move forward the carry to the next index. *)
									else if (i>=n) then
											if ((carry + stringtoint(chartostring(String.sub(a, m-i-1)))) = ~1) then sub(a,b,i+1, m,n,~1) ^ "9"
										(* else we will just move 0 as the carry to the next index and add the string part of the carry added to that particular index of the larger index *)
											else sub(a,b,i+1, m,n,0) ^ inttomystring(chartoint(a, m-i-1) +carry)
									else 
										(* else we will just divide it into two cases which include when carry +  larger string index greater than smaller index then we will just move forward with 0 carry
										otherwise move the -1 carry forward to next index and append the remaining thing to the next recursion call *)
										if (carry + chartoint(a, m-i-1) < chartoint(b, n-i-1)) then sub(a,b,i+1,m,n , ~1) ^ inttomystring((chartoint(a, m-i-1) +carry +10 -(chartoint(b, n-i-1))) mod 10)
										else sub(a, b , i+1, m , n , 0) ^ inttomystring(chartoint(a, m-i-1) + carry - (chartoint(b,n-i-1)));
				
	(* this function is used to give the final subtraction of two strings a and b of length m and n	*)
fun finalsub(a, b, m , n) = 
	let 
	(* this gives us the normal subtraction of the two strings *)
	val str1 = sub(a, b , 0 , m , n ,0)
	(* convert the string to array using explode *)
	val  p = explode str1
	(* this will be used to compare in the strip function defined above *)
	val mt  : string = "0"
	(* this will give the list after removing the zeroes using strip function *)
	val new_p  : char list = strip( mt, p )
	in
		(* this will return us the string formed from new_p list *)
		implode new_p
	end;

	(* this recursion function gives us the value of the unit digit which is to appended to the divisor and also multiplied with the formed string *)
fun crctvalue(my_divisor, i , tobefound)  = 
			(* if i>9 then we will just return "9" because it is the largest one digit integer *)
			if (i > 9) then inttomystring(9)
			(* if size of the new formed string is greater than what needs to be found (tobefound) just return (i-1) as the string *)
			else if (String.size(mult(my_divisor  ^ inttomystring(i) , inttomystring(i) , String.size(my_divisor  ^ inttomystring(i)) ,  0 , 0 )) > String.size(tobefound)) then inttomystring(i-1)
			else
				(* if the size is less than that of the string to be found we repeat the recursion with the digit as i+1 *)
				if  (String.size(mult(my_divisor  ^ inttomystring(i) , inttomystring(i) , String.size(my_divisor  ^ inttomystring(i)) ,  0 , 0 )) < String.size(tobefound)) then crctvalue(my_divisor, i+1, tobefound)
				else
					(* if the comparison of the string formed is > the string needed then we will just return i-1 else just call the recursion again *)
				 	if (mult(my_divisor  ^ inttomystring(i) , inttomystring(i) , String.size(my_divisor  ^ inttomystring(i)) ,  0 , 0 ) > tobefound) then inttomystring(i-1)
					else
						crctvalue(my_divisor, i+1, tobefound);

	(* this function gives us the double of the quotient which is used to get the next divisor in next step *)
fun givemedoubleofquotient(quo) = mult(quo , "2" , String.size(quo) ,0 , 0);

	(* this gives us the value of the divisor in any step without the last (unit place digit) *)
fun givememydivisorwithoutlastdigit(quo , rem, tobefound) = mult(mult(quo , "2" , String.size(quo) ,0 , 0)^ crctvalue(rem, 1, tobefound) , crctvalue(rem, 1, tobefound) , String.size(mult(quo , "2" , String.size(quo) ,0 , 0)^ crctvalue(rem, 1, tobefound)) , 0, 0);

	(* this gives us the product i.e. which will be subtracted from the reminder + two/one digits using the crctvalue function and thus this will be used to get the next reminder and hence will be recursed  *)
fun givemeproduct(quo, rem, tobefound) =  mult(mult(quo , "2" , String.size(quo) ,0 , 0)^ crctvalue(givemedoubleofquotient(quo), 1, tobefound) , crctvalue(givemedoubleofquotient(quo), 1, tobefound) , String.size(mult(quo , "2" , String.size(quo) ,0 , 0)^ crctvalue(givemedoubleofquotient(quo), 1, tobefound)) , 0, 0);

	(* this function is the recursion form of square root with a as the given string and ind as the index in the string a, quo as quotient, rem as reminder, and len as the length of a *)
fun rec_sqrt(a, ind,  quo, rem, len) : string * string = if (ind  = len) then
															(* if ind = len if quo is "" and rem = "" then return ("0" , "0")  else just return (quo, "0") *)
															if (rem = "") then
																if (quo = "") then ("0" , "0")
																else
																(quo, "0")
															else (quo , rem) 
														(* case when we have to take the only 1 length substring initially we took that and move index to i+1 for further recursion *)
														else if ((len-ind) mod 2 = 1) then 
															rec_sqrt( a, ind+1 , quo  ^ crctvalue(mult(quo, "2" ,String.size(quo) , 0, 0), 1, rem ^ String.extract(a, ind, Int.fromString("1"))) , 
															finalsub( rem ^ String.extract(a, ind, Int.fromString("1")) , givemeproduct(quo, rem,  rem ^ String.extract(a, ind, Int.fromString("1"))) , String.size(rem ^ String.extract(a, ind, Int.fromString("1"))) , String.size(givemeproduct(quo, rem,  rem ^ String.extract(a, ind, Int.fromString("1")))))   , 
															len)
														(* the normal case of when we need to have 2 length substrings in the string a (just move index to ind+2) *)
														else  rec_sqrt( a, ind+2 , quo  ^ crctvalue(mult(quo, "2" ,String.size(quo) , 0, 0), 1, rem ^ String.extract(a, ind, Int.fromString("2"))) , 
															finalsub( rem ^ String.extract(a, ind, Int.fromString("2")) , givemeproduct(quo, rem,  rem ^ String.extract(a, ind, Int.fromString("2"))) , String.size(rem ^ String.extract(a, ind, Int.fromString("2"))) , String.size(givemeproduct(quo, rem,  rem ^ String.extract(a, ind, Int.fromString("2")))))   , 
															len) ;

	(* this function just returns -1 if the input is correct otherise return 1 if other than numeric some other character is there.*)
fun checkfault(a, len, ind) =
		if (ind = len) then ~1
		else 
			if (String.extract(a, ind, Int.fromString("1")) <"0")
				then 1
			else if (String.extract(a, ind, Int.fromString("1")) >"9") then 1
			else 
				(* just move ahead the recursion with index as ind+1 *)
				checkfault(a, len, ind+1);
		
			

	(* this is the main function of our program which is provided with an input string a and returns the (quotient, reminder) as the pair *)
fun isqrtld(a : string) = 
		(* in case of "" empty string it will return both the reminder and the quotient as "" strings *)
		if a = "" then ("" , "")
		else
			(* in other case firstly we will strip all the starting "0"s from the given string after converting it to list and then call
			the rec_sqrt function with the string mystr and index as 0 and length as len as well as quotient and reminder as "" , "" *)
			let 
				val the_l = explode a
				val mt  : string = "0"
				val given_str : char list = strip(mt, the_l)
				val mystr : string = implode given_str
				val len : int  = String.size(mystr)
			in
				(* if there is some other character in the input string given to us then we will print this else just call rec_sqrt*)
				if (checkfault(mystr ,len, 0) > 0) then ("The input given is wrong"  , "Please correct it!!!")
				else
					rec_sqrt(mystr, 0, "" , "" , len)

		end;