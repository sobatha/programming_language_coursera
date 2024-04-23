(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (target, lis) = SOME []

fun get_substitutions1 (lis, target) = []

fun get_substitutions2 (lis, target) = []

fun similar_names (names, target) = []

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

fun remove_card (cards, card, exception) = []

fun all_same_color (cards) = true

fun sum_cards (cards) = 4

fun score (cards, goal) = 4

fun officiate (cards, moves, goal) = 6
             
             
