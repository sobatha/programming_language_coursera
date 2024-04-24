(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals lst =
    List.filter (fn s => Char.isUpper (String.sub (s, 0))) lst

fun longest_string1 lst = 
    foldl (fn (s, acc) => if String.size s > String.size acc then s else acc) "" lst

fun longest_string2 lst = 
    foldl (fn (s, acc) => if String.size s >= String.size acc then s else acc) "" lst

fun longest_string_helper f lst = 
    foldl (fn (s, acc) => if f(String.size s, String.size acc) then s else acc) "" lst

val longest_string3 = longest_string_helper (fn (s, acc) => s > acc )

val longest_string4 = longest_string_helper (fn (s, acc) => s >= acc )

infix |> 
fun x |> f = f x

val longest_capitalized = longest_string1 o only_capitals

fun rev_string s = 
    (String.implode o List.rev o String.explode) s

fun first_answer f lst = (* (’a -> ’b option) -> ’a list -> ’b *)
    case lst of 
      [] => raise NoAnswer
    | x::xs => case f x of
                  NONE => first_answer f xs
                | SOME y => y

fun all_answers f lst = (* (’a -> ’b list option) -> ’a list -> ’b list option *)
    let fun all_answers_helper acc =
        foldl (fn (x, acc) => case f x of 
                                  NONE => acc
                                | SOME y => y @ acc
            ) acc lst
    val all_answers_list = all_answers_helper []
    in
        case lst of 
          [] => SOME []
        | _ => case all_answers_list of 
                    [] => NONE
                  | _ => SOME all_answers_list 
    end
