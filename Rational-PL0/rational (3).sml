use "bigint.sml";

functor RATIONAL(BigInt: BIGINT) :
sig
      type rational
      exception rat_error


    val rat_zero  : rational
    val rat_one : rational
    val rat: bigint -> rational option
    val make_rat: (bigint) * (bigint) -> rational option
    val reci: bigint -> rational option
    val neg: rational -> rational
    val inverse : rational -> rational option
    val equal : rational * rational -> bool (* equality *)
    val less : rational * rational -> bool (* less than *)
    val add : rational * rational -> rational (* addition *)
    val subtract : rational * rational -> rational (* subtraction *)
    val multiply : rational * rational -> rational (* multiplyiplication *)
    val divide : rational * rational -> rational option
    val showRat : rational -> string
    val showDecimal : rational -> string
    val toDecimal : rational -> string
    val fromDecimal : string -> rational

   (*
     *)
end =


  struct
  exception rat_error
  type rational = bigint * bigint
  
  val rat_zero = (zero, one)
  val rat_one = (one, one)

      (* this is  a function to convert an integer a to string my_str *)
    fun inttomystring(a) =
      let 
      val my_str : string = Int.toString(a)
      in 
        my_str
    end;

    fun striptolast(a, arr) = if (length(arr)=0) then [0]
    else if (length(arr)=1) then arr
    else if a=inttomystring(hd(arr))  then striptolast(a,tl(arr))
    else arr;

  fun rat (b1 : bigint) : rational option = SOME ((#1 b1 , (striptolast("0" , #2 b1))) , one);

  fun gcd(a : bigint, b : bigint) : bigint = if compare(b, zero) = EQUAL then a else gcd(b, modu(a, b));

  (* will handle this function later *)
  fun make_rat(b1 : bigint, b2 : bigint) : rational option = 
  if (#1 (b2) = 0) then raise rat_error
  else if ((#1 b1) = 0) then rat( (0, [0]) )
  else if (samesign(b1 , b2)) then SOME ( (1 , #2 (divi(b1 , gcd(b1, b2)))) , (1 , #2 (divi(b2 , gcd(b1, b2))) ))
  else (SOME ( (~1 , #2 (divi(b1 , gcd(b1, b2))) ) , (1 , #2 (divi(b2 , gcd(b1, b2))))))
  ;

  fun reci(b1 : bigint) = if (#1 b1 = 0) then raise rat_error  
    else if (#1 b1 = ~1) then SOME (nega(one), abs(( #1 b1 , striptolast("0" , #2 b1)) ))
    else (SOME ((one), ( #1 b1 , striptolast("0" , #2 b1))));

  fun neg (rat : rational) = if (#1 (#2 (rat) )  = 0) then raise rat_error
    else if (samesign(#1 rat, #2 rat) = false) then valOf(make_rat(abs(#1 rat) , abs(#2 rat)))
    else valOf(make_rat(nega(abs(#1 rat)) , abs(#2 rat)))
    ;
  
  fun inverse(rat: rational) = if (#1(#1 rat) = 0 orelse #1(#2 (rat)) =0) then raise rat_error
    else if (samesign(#1 rat, #2 rat) = false) then make_rat(nega(abs(#2 rat)) , abs(#1 rat))
    else make_rat((abs(#2 rat)) , abs(#1 rat))

  fun equal(rat1 : rational, rat2 :rational) = if (#1 (#2 rat1) = 0 orelse #1 (#2 rat2) = 0 ) then raise rat_error
  else if (compare(multiply_prev(#1 rat1, #2 rat2) , multiply_prev(#1 rat2, #2 rat1) ) = EQUAL) then true
                          else false;
  
  fun less(rat1 : rational , rat2 :rational) =  if (#1 (#2 rat1) = 0 orelse #1 (#2 rat2) = 0 ) then raise rat_error
  else if (samesign(#1 rat1, #2 rat1) andalso samesign(#1 rat2, #2 rat2) = false) then false
  else if (samesign(#1 rat2, #2 rat2) andalso samesign(#1 rat1, #2 rat1) = false) then true
  else if (samesign(#1 rat1, #2 rat1) andalso samesign(#1 rat2, #2 rat2)) then (
    if (compare(multiply_prev(#1 rat1, #2 rat2) , multiply_prev(#1 rat2, #2 rat1) ) = LESS) then true
    else false
  )
  else (   
    if (compare(multiply_prev(abs(#1 rat1), abs(#2 rat2)) , multiply_prev(abs(#1 rat2), abs(#2 rat1)) ) = LESS) then false
    else true
  )
  ;

  fun add(rat1 : rational, rat2 : rational) = if (#1 (#2 rat1) = 0 orelse #1 (#2 rat2) = 0 ) then raise rat_error
  else valOf(make_rat(addi(multiply_prev(#1 rat1, #2 rat2) , multiply_prev(#1 rat2, #2 rat1)) , multiply_prev(#2 rat1, #2 rat2)))
  ;

  fun subtract(rat1 : rational, rat2 : rational) = if (#1 (#2 rat1) = 0 orelse #1 (#2 rat2) = 0 ) then raise rat_error
  else valOf(make_rat(subt(multiply_prev(#1 rat1, #2 rat2) , multiply_prev(#1 rat2, #2 rat1)) , multiply_prev(#2 rat1, #2 rat2)))
  ;

  fun multiply(rat1 : rational, rat2: rational) = if (#1 (#2 rat1) = 0 orelse #1 (#2 rat2) = 0 ) then raise rat_error
  else valOf(make_rat(multiply_prev(#1 rat1, #1 rat2) , multiply_prev(#2 rat1, #2 rat2)))
  ;

  fun divide(rat1: rational, rat2: rational) = if (#1 (#2 rat1) = 0 orelse #1 (#2 rat2) = 0 orelse #1 (#1 rat2) = 0) then raise rat_error
  else make_rat(multiply_prev(#1 rat1, #2 rat2) , multiply_prev(#2 rat1, #1 rat2))
  ;

  fun showRat(rat: rational) = 
  let
  val myral =  valOf(make_rat(#1 rat, #2 rat))
  val mystr = tostring(#1 myral) ^ "/" ^ tostring(#2 myral)
  in
    mystr
  end;

    fun eletoarr(i) =
    let
    val imy : int = i
    val myarr : int list = imy::[]

    in
      myarr
    end;

  fun listcontains(ls : int list, i : int , ind : int) = if (ind = length(ls)) then ~1
        else if (List.nth(ls, ind) = i) then ind
        else listcontains(ls, i ,  ind+1)
   ;

   fun returndecimalpart(ls : int list , i : int) = tostring ( (1, List.take(ls, i)) ) ^ "(" ^ tostring ( (1, List.drop(ls,i))) ^")"
   ;

  fun digits_after_dec(a :int list , b : int list , arr : int list , count : int) =  
          if (listcontains(arr,  toint( (1, striptolast("0" ,givemequotient( a@[0] , b))))  , 0)  <> ~1 andalso count > length(b)-1) then 
          (
            (* print(returndecimalpart(arr, listcontains(arr,  toint( (1 ,striptolast("0" ,givemequotient( a@[0] , b))))  , 0)) ^ "\n"); *)
          returndecimalpart(arr, listcontains(arr,  toint( (1 ,striptolast("0" ,givemequotient( a@[0] , b))))  , 0))
          )
          else 
          (
            (* print( givemestring(givemequotient(a@[0] , b) , "")); *)
            (* print( givemestring(arr , "") ^ "\n");  *)
          digits_after_dec(givemeremainder(a @ [0], b) , b, arr @ givemequotient( a@[0] , b) , count+1)
          )

  ;

  fun showd(rat : rational) = if (#1(#2 rat) =0) then raise rat_error
      else if  (samesign(#1 rat, #2 rat) ) then 
      (* ( print( givemestring(#2 (modu(#1 rat, #2 rat)) , "") ^ "\n" ); *)
      (
        givemestring ( #2 (divi(#1 rat, #2 rat)), "") ^ "." ^ digits_after_dec(#2 (modu(#1 rat, #2 rat)) , #2 (#2 rat) , [] , 0)
      
      )
      else  "~" ^ givemestring ( #2 (divi(abs(#1 rat), abs(#2 rat))), "") ^ "." ^ digits_after_dec(#2 (modu(abs(#1 rat), abs(#2 rat))) , #2 (#2 rat) , [] ,0)
  ;


  fun showDecimal (rat : rational)= showd( valOf(make_rat (( #1 (#1 rat), striptolast("0", #2 (#1 rat))) , (#1 (#2 rat), striptolast("0", #2 (#2 rat))))) )
  ;

  fun toDecimal (rat : rational) =
      showDecimal(rat) 
  ;

  fun getstrind(st :string, ind :int, ch : char) = if (String.sub(st, ind) = ch) then ind
      else getstrind(st, ind +1, ch)
  ;

    fun chartoint(c : char) =
      let
        val my_str : string = str (c)
        val my_int : int = valOf(Int.fromString(my_str))
      in
        my_int
      end
    ;

  fun fill_small_ele (st :string , i :int , small : int list) = if (i = size(st)) then small
  else if (String.sub(st, i) <> #"(" andalso String.sub(st, i) <> #")" andalso String.sub(st, i) <> #".") then fill_small_ele(st, i+1, small @ (eletoarr(chartoint(String.sub(st,i))) ) ) 
  else fill_small_ele (st, i+1, small);

  fun fill_array (arr : int list, i  : int, times : int ) = if (times =0) then arr
    else fill_array(arr@eletoarr(i) , i, times-1);


  fun fd (st : string)= 
  let
    val dot = getstrind(st, 0, #".")   
    val start_brac = getstrind(st, 0 , #"(")
    val end_brac = getstrind(st, 0, #")") 
    (* 1, 2, 4 *)
    val larger = fill_small_ele(st ^ String.extract(st, start_brac, NONE), 0, [])
    val smaller = fill_small_ele(st, 0, [])
    val res = subt((1, larger) , (1, smaller))
    val new_arr = fill_array([] , 9 , end_brac-start_brac-1)
    val final_denom = fill_array(new_arr, 0 , size(st)-dot-3)
  in
      make_rat((1 , #2 res) , (1, final_denom))
  end;


  fun fromDecimal(st: string) = if (size(st) =0) then raise rat_error
      else if (String.sub(st , 0)= #"~") then neg(valOf(fd(String.extract(st , 1 , NONE))))
      else if (String.sub(st , 0)= #"+") then (valOf(fd(String.extract(st , 1 , NONE))))
      else valOf(fd(st));

end;

structure Rational = RATIONAL(BigInt);

open Rational;