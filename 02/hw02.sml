(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (target, lis) = 
    (* acc holds filtered list and found holds whether list contained target string or not *)
    let fun except (acc, lis, found) = 
        case (acc, lis, found) of 
        (acc, [], f) => (acc, [], f)
        | (acc, x::xs', f) => if same_string(target, x) then except(acc, xs', true)
                    else except(acc @ [x], xs', f)
    in 
        let val (acc, ls, found) = except([], lis, false) 
        in
            if found then SOME acc else NONE
        end
    end

fun get_substitutions1 (lis, target) = 
    case lis of 
      [] => []
    | x::xs' => 
        case all_except_option (target, x) of
          NONE => get_substitutions1(xs', target)
        | SOME x => x @ get_substitutions1(xs', target)

fun get_substitutions2 (lis, target) = 
    let fun substitutions_tail_recurr (lis, target, acc) = 
        case lis of 
        [] => acc
        | x::xs' => 
            case all_except_option (target, x) of
            NONE => substitutions_tail_recurr(xs', target, acc)
            | SOME lis => substitutions_tail_recurr(xs', target, acc @ lis)
    in substitutions_tail_recurr(lis, target, []) end

fun similar_names (names, {first, last, middle}) = 
    let fun create_similar_name (lst) =
        case lst of 
          [] => []
        | x::xs' => {first=x, last=last, middle=middle}::create_similar_name(xs')
    in
        {first=first, last=last, middle=middle} :: create_similar_name (get_substitutions2 (names, first))
    end 

(*


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (card) = Black

fun card_value (card) = 2

fun remove_card (cards, card, exp) = []

fun all_same_color (cards) = true

fun sum_cards (cards) = 4

fun score (cards, goal) = 4

fun officiate (cards, moves, goal) = 6
             
*)
