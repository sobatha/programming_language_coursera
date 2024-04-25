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

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f lst = (* (’a -> ’b option) -> ’a list -> ’b *)
    case lst of 
      [] => raise NoAnswer
    | x::xs => case f x of
                  NONE => first_answer f xs
                | SOME y => y

fun all_answers f lst = (* (’a -> ’b list option) -> ’a list -> ’b list option *)
    let fun h acc l = 
        case l of
          []    => SOME acc
        | x::xs => case f x of
                      NONE => NONE
                    | SOME y => h (y @ acc) xs
    in h [] lst end

val count_wildcards = g (fn () => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn s => String.size s)

fun count_some_var (target, p) = 
    g (fn () => 0) (fn s => if target = s then 1 else 0) p

fun check_pat p = (* pattern -> bool *) 
    let
        fun get_all_strings p =
            case p of
                Variable x        => [x]
                | TupleP ps       => List.foldl (fn (p,i) => (get_all_strings p) @ i) [] ps
                | ConstructorP(_, p) => get_all_strings p
                | _               => []
        fun is_repeat lst = 
            case lst of 
            [] => false
            | x::xs => List.exists (fn s => x=s) xs orelse is_repeat xs
    in
        not ((is_repeat o get_all_strings) p)
    end

fun match (v, p) = (* (valu * pattern) -> (string * valu) list option *)
    case (valu,pat) of
	    (_,Wildcard)    => SOME []
      | (_,Variable(s)) => SOME [(s,valu)]
      | (Unit,UnitP)    => SOME []
      | (Const i, ConstP j)    => if i=j then SOME [] else NONE
      | (Tuple(vs),TupleP(ps)) => if length vs = length ps
				                          then all_answers match (ListPair.zip(vs,ps))
				                          else NONE
      | (Constructor(s1,v), ConstructorP(s2,p)) => if s1=s2 then match(v,p)
                                                   else NONE
      | _ => NONE
      
fun first_match v ps = (* valu -> pattern list -> (string * valu) list option *)
    (SOME (first_answer match (map (fn p => (v, p)) ps))) handle NoAnswer => NONE
    