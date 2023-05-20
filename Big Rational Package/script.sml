(* Author: Harshil Vagadia *)
(* Script for evaluating Rational package *)

use "rational.sml"; (* load Rational package *)

(* Create test cases *)

val list_of_tests = [
    (Rational.showDecimal(Rational.fromDecimal("1.2(0)")), "1.2(0)"),
    (Rational.showDecimal(Rational.add(Rational.fromDecimal("+100.(0)"), Rational.fromDecimal("~200.(0)"))), "~100.(0)"),
    (Rational.showDecimal(Rational.subtract(Rational.fromDecimal("1000.(0)"), Rational.fromDecimal("0.00001(0)"))), "999.99999(0)"),
    (Rational.showDecimal(Rational.multiply(Rational.fromDecimal("1000000.(0)"), Rational.fromDecimal("1000000.(0)"))), "1000000000000.(0)"),
    (Rational.showRat(valOf(Rational.divide(Rational.fromDecimal("0.000001(0)"), Rational.fromDecimal("1000000.(0)")))), "1/1000000000000"),
    (if Rational.equal(valOf(Rational.inverse(Rational.fromDecimal("0.4(3)"))), Rational.fromDecimal("3.(0)")) then "1" else "0", "0"),
    (if Rational.equal(valOf(Rational.inverse(Rational.fromDecimal("0.333333(3)"))), Rational.fromDecimal("3.00(0000)")) then "1" else "0", "1"),
    (if Rational.less(Rational.neg(Rational.fromDecimal("+999999999999999999.(9)")), Rational.fromDecimal("~0.0000000000000000001(0)")) then "1" else "0", "1")
]

fun check_test(test) =
    let
        val (x, y) = test
    in
        if x = y then
            (print("Test passed: " ^ x ^ " = " ^ y ^ "\n"); 1)
        else
            (print("Test failed: " ^ x ^ " != " ^ y ^ "\n"); 0)
    end

(* Run tests *)

val score = foldl (fn (x, y) => x + y) 0 (map check_test list_of_tests)