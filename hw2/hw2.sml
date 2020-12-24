(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1. This problem involves using first-name substitutions to come up with alternate names. For example,
Fredrick William Smith could also be Fred William Smith or Freddie William Smith. Only part (d) is
specifically about this, but the other problems are helpful. *)

(* (a) Write a function all_except_option, which takes a string and a string list. Return NONE if the
string is not in the list, else return SOME lst where lst is identical to the argument list except the string
is not in it. You may assume the string is in the list at most once. *)
fun all_except_option (s, xs) =
    case xs of
	[] => NONE
      | x::xs' => if same_string(s, x)
		 then SOME xs'
		 else case all_except_option(s, xs') of
			  NONE => NONE
			  | SOME l => SOME (x::l)

(* (b) Write a function get_substitutions1, which takes a string list list (a list of list of strings, the
substitutions) and a string s and returns a string list. The result has all the strings that are in
some list in substitutions that also has s, but s itself should not be in the result. *)
fun get_substitutions1 (xs, s) =
    case xs of 
	[] => []
      | x::xs' => case all_except_option(s, x) of
		      SOME l => l @ get_substitutions1(xs', s)
		    | NONE => get_substitutions1(xs', s)

(* (c) Write a function get_substitutions2, which is like get_substitutions1 except it uses a tail-recursive
local helper function. *)
fun get_substitutions2 (xs, s) =
    let
	fun aux (xs, acc) =
	    case xs of
		[] => acc
	     | x::xs' => case all_except_option(s, x) of
			     SOME l => aux(xs', acc @ l)
			   | NONE => aux(xs', acc)
    in
	aux(xs, [])
    end

(* (d) Write a function similar_names, which takes a string list list of substitutions (as in parts (b) and
(c)) and a full name of type {first:string,middle:string,last:string} and returns a list of full
names (type {first:string,middle:string,last:string} list). The result is all the full names you
can produce by substituting for the first name (and only the first name) using substitutions. 
The answer should begin with the original name *)
fun similar_names (xs, {first=f, middle=m, last=l}) =
    let
	fun aux (xs) =
	    case xs of
		[] => []
	      | x::xs' => {first=x, middle=m, last=l} :: aux (xs')
    in
	{first=f, middle=m, last=l} :: aux (get_substitutions2(xs, f))
    end
 
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* (a) Write a function card_color, which takes a card and returns its color (spades and clubs are black,
diamonds and hearts are red). Note: One case-expression is enough. *)
fun card_color card =
    case card of
	(Spades, _) => Black
      | (Clubs, _)  => Black
      | (_, _) => Red
		      
(* (b) Write a function card_value, which takes a card and returns its value (numbered cards have their
number as the value, aces are 11, everything else is 10). Note: One case-expression is enough. *)
fun card_value card =
    case card of
	(_, Num x) => x
      | (_, Ace) => 11
      | (_, _) => 10

(* (c) Write a function remove_card, which takes a list of cards cs, a card c, and an exception e. It returns a
list that has all the elements of cs except c. If c is in the list more than once, remove only the first one.
If c is not in the list, raise the exception e. You can compare cards with =. *)
fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
      | x::xs' => if x = c
		  then xs'
		  else x :: remove_card(xs', c, e)

(* (d) Write a function all_same_color, which takes a list of cards and returns true if all the cards in the
list are the same color *)
fun all_same_color cards =
    case cards of
	[] => true
      | x::[] => true
      | head::(neck::rest) => card_color(head) = card_color(neck) andalso all_same_color (neck::rest)

(* (e) Write a function sum_cards, which takes a list of cards and returns the sum of their values. Use a locally
defined helper function that is tail recursive. *)
fun sum_cards cards =
    let
	fun aux (xs, acc) =
	    case xs of
		[] => acc
	      | x::xs' => aux(xs', acc + card_value(x))
    in
	aux(cards, 0)
    end

(* (f) Write a function score, which takes a card list (the held-cards) and an int (the goal) and computes
the score as described above. *)
fun score (held_cards, goal) =
    let
	val sum = sum_cards held_cards
	val preliminary_score = if sum > goal
				then 3 * (sum - goal)
				else goal - sum
    in
	if all_same_color held_cards
	then preliminary_score div 2
	else preliminary_score
    end

(* (g) Write a function officiate, which “runs a game.” It takes a card list (the card-list) a move list
(what the player “does” at each point), and an int (the goal) and returns the score at the end of the
game after processing (some or all of) the moves in the move list in order. *)
fun officiate (card_list, move_list, goal) =
    let
	fun aux (card_list, held_cards, move_list) =
	    case move_list of
		[] => score(held_cards, goal)
	      | x::xs' => case x of
			      Discard card => aux(card_list, remove_card(held_cards, card, IllegalMove), xs')
			    | Draw => case card_list of
					  [] => score(held_cards, goal)
					| y::ys' => if sum_cards(y::held_cards) > goal
						    then score(y::held_cards, goal)
						    else aux(ys', y::held_cards, xs')
    in
	aux(card_list, [], move_list)
    end

(* (a) Write score_challenge and officiate_challenge to be like their non-challenge counterparts except
each ace can have a value of 1 or 11 and score_challenge should always return the least (i.e., best)
possible score. (Note the game-ends-if-sum-exceeds-goal rule should apply only if there is no sum that
is less than or equal to the goal.) Hint: This is easier than you might think. *)
fun ace_count held_cards =
    case held_cards of
	[] => 0
      | x::xs' => case x of
		      (_, Ace) => 1 + ace_count xs'
		    | (_, _) => ace_count xs'

fun sum_cards_variable_ace (held_cards, acc, one_ace_count) =
    case held_cards of
        [] => acc
      | (_, Ace)::xs' => if one_ace_count <> 0
                         then sum_cards_variable_ace(xs', acc + 1, one_ace_count - 1)
                         else sum_cards_variable_ace(xs', acc + 11, 0)
      | x::xs' => sum_cards_variable_ace(xs', acc + card_value(x), one_ace_count)
					
fun score_challenge (held_cards, goal) =
    let
	fun score_with_ace_count (held_cards, one_ace_count) =
	    let
		val sum = sum_cards_variable_ace(held_cards, 0, one_ace_count)
		val preliminary_score = if sum > goal
					then 3 * (sum - goal)
					else goal - sum
	    in
		if all_same_color held_cards
		then preliminary_score div 2
		else preliminary_score
	    end

	fun find_smallest_score (held_cards, one_ace_count) =
	    if one_ace_count = 0
	    then score_with_ace_count(held_cards, one_ace_count)
	    else Int.min(score_with_ace_count(held_cards, one_ace_count), find_smallest_score(held_cards, one_ace_count - 1))
    in
	find_smallest_score(held_cards, ace_count(held_cards))
    end

fun officiate_challenge (card_list, move_list, goal) =
	let
            fun aux (card_list, held_cards, move_list) =
		case move_list of
                    [] => score_challenge(held_cards, goal)
		  | x::xs' => case x of
				  Discard card => aux(card_list, remove_card(held_cards, card, IllegalMove), xs')
				| Draw => case card_list of
                                              [] => score_challenge(held_cards, goal)
                                            | y::ys' => if sum_cards_variable_ace(y::held_cards, 0, ace_count(y::held_cards)) > goal
							then score_challenge(y::held_cards, goal)
							else aux(ys', y::held_cards, xs')
	in
            aux(card_list, [], move_list)
	end

(* (b) Write careful_player, which takes a card-list and a goal and returns a move-list such that calling
officiate with the card-list, the goal, and the move-list has this behavior:
• The value of the held cards never exceeds the goal.
• A card is drawn whenever the goal is more than 10 greater than the value of the held cards. As a
detail, you should (attempt to) draw, even if no cards remain in the card-list.
• If a score of 0 is reached, there must be no more moves.
• If it is possible to reach a score of 0 by discarding a card followed by drawing a card, then this
must be done. Note careful_player will have to look ahead to the next card, which in many card
games is considered “cheating.” Also note that the previous requirement takes precedence: There
must be no more moves after a score of 0 is reached even if there is another way to get back to 0. *)
fun careful_player (card_list, goal) =
    let
	fun find_card_to_discard (all_held_cards, iter_held_cards, new_card) =
	    case iter_held_cards of
		[] => NONE
	      | x::xs' => if score(new_card :: remove_card(all_held_cards, x, IllegalMove), goal) = 0
			  then SOME x
			  else find_card_to_discard(all_held_cards, xs', new_card)
		
	fun aux (card_list, held_cards) =
	    if (goal - 10 >= sum_cards(held_cards))
	    then case card_list of 
		     [] => [Draw]
		   | x::xs' => Draw :: aux(xs', x::held_cards)
	    else case score(held_cards, goal) of
		     0 => []
		   | _ => case card_list of
			      [] => []
			    | x::_ => case find_card_to_discard(held_cards, held_cards, x) of
					  SOME c => [Discard c, Draw]
					| NONE => []
    in
	aux(card_list, [])
    end
							   
