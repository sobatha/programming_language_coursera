(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_1 = only_capitals ["Abc","abc","Css"] = ["Abc","Css"]
val test2 = longest_string1 ["A","bc","C"] = "bc"
val test2_1 = longest_string1 ["A","bc","Cd"] = "bc"
val test2_2 = longest_string1 [] = ""
val test3 = longest_string2 ["A","bc","C"] = "bc"
val test3_1 = longest_string2 ["A","bc","Cd"] = "Cd"
val test4a = longest_string3 ["A","bc","Cd"] = "bc"
val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"
val test6 = rev_string "abc" = "cba"
val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x > 3 then SOME [x] else NONE) [1,2,3,4,5] = SOME [5, 4]
val test8_2 = all_answers (fn x => if x > 3 then SOME [x] else NONE) [] = SOME []

val test9a = count_wildcards Wildcard = 1
val test9a_1 = count_wildcards (ConstP(4)) = 0
val test9a_2 = count_wildcards (Variable("a")) = 0
val test9a_3 = count_wildcards (TupleP([Wildcard, ConstP(4), Wildcard])) = 2
val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_1 = count_wild_and_variable_lengths (Variable("abc")) = 3
val test9b_2 = count_wild_and_variable_lengths (TupleP([Wildcard, Variable("abc"), Wildcard])) = 5
val test9c = count_some_var ("x", Variable("x")) = 1
val test9c_1 = count_some_var ("x", Wildcard) = 0
val test9c_2 = count_some_var ("abc", TupleP([Wildcard, Variable("abc"), Wildcard, Variable("abc")])) = 2
val test10 = check_pat (Variable("x")) = true
val test10_1 = check_pat (TupleP([Wildcard, Variable("abc"), Wildcard, Variable("abc")])) = false

val test11 = match (Const(1), UnitP) = NONE
val test11_1 = match (Const(1), ConstP(1)) = SOME []
val test11_2 = match (Tuple([Unit, Const(1), Const(1)]), TupleP([Wildcard, Variable("abc"), ConstP(1)])) = SOME [("abc", Const(1))]

val test12 = first_match Unit [UnitP] = SOME []
val test12_1 = first_match Unit [ConstP(1)] = NONE
