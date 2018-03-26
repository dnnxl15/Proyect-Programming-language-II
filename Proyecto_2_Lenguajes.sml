
(*Reference code*)

datatype suit = Clubs | Diamonds | Hearts | Spades

datatype rank = Jack | Queen | King | Ace | Num of int

type card = suit*rank

datatype color = Red | Black

datatype move = Discard of card | Draw

exception IllegalMove

(*function 1*)
(*Description: Return the card color*)
(*Last modification: 26/03/18*)

fun card_color(pCard: card) = 
	case #1 pCard of
		Clubs => Black
	|	Spades => Black
	|	Diamonds => Red
	|	Hearts => Red

val test1 = card_color(Clubs, Num 2) = Black;

(*function 2*)
(*Description: Return the card value*)
(*Last modification: 26/03/18*)

fun card_value(pCard: card) = 
	case #2 pCard of
		Jack => 10
	|	Queen => 15
	|   King => 11
	|   Num number => number

val test2 = card_value(Clubs, Num 2) = 2;

(*function*)
(*Description: Return a boolean if the card and the second card are the same*)
(*Last modification: 26/03/18*)

fun equal(pCard: card, pCardSecond: card) = 
	if #1 pCard = #1 pCardSecond
	then if #2 pCard = #2 pCardSecond
			then true
			else false
	else
	false

val test3 = equal((Clubs, Jack),(Diamonds, Queen)) = false;
val test4 = equal((Diamonds, Queen),(Diamonds, Queen)) = true;


(*Function*)
(*Description: Return a boolean if the element is on the list*)
(*Last modification: 26/03/18*)

fun beList(cs: card list, c: card) =
	case cs of
		[] => false
	| x:: cs' => if equal(x, c) then true
				 else beList(cs', c)

val test5 = beList([(Diamonds, Queen), (Clubs, Jack)], (Clubs, Jack)) = true;

(*Function*)
(*Description: Remove only once the card on the list*)
(*Last modification: 26/03/18*)

fun remove_card(cs: card list, c: card, count: int) =
  case cs of
    [] => []
  
  | hd:: tl => if equal(hd, c) 
          then if count = 0 then remove_card(tl, c, count + 1)
            else hd ::remove_card(tl, c, count)
            
         else hd::remove_card(tl, c, count)

val test6 = remove_card([(Diamonds, Queen), (Clubs, Jack), (Clubs, Jack)], (Clubs, Jack), 0) = [(Diamonds, Queen),(Clubs, Jack)];

(*function 3*)
(*Description: Return the list of card with the remove card setter in the parameter*)
(*Last modification: 26/03/18*)

fun remove(cs: card list, c: card, e: exn) =
	case cs of
	 [] => raise e
	| x:: cs' =>if beList(cs, c) then remove_card(cs, c, 0)
				else raise e

val test7 = remove([(Diamonds, Queen), (Clubs, Jack), (Clubs, Jack)], (Clubs, Jack), IllegalMove) = [(Diamonds, Queen),(Clubs, Jack)];


(*Function*)
(*Description: Return a boolean if all the card on the list i the same color as the given in the parameter*)
(*Last modification: 26/03/18*)

fun all_same_color_aux(pCardList : card list, pColor: color) =
	case pCardList of 
	[] => true
  | x::pCardList' => if card_color(x) = pColor
  						then true andalso all_same_color_aux(pCardList', pColor)
  					 else
  					 	false

val test8 = all_same_color_aux([(Diamonds, Queen), (Clubs, Jack), (Clubs, Jack)], Red) = false;
val test9 = all_same_color_aux([(Clubs, Jack), (Clubs, Jack)], Black) = true;

(*Function 4*)
(*Description: Return a boolean if all the card on the list i the same color*)
(*Last modification: 26/03/18*)

fun all_same_color(pCardList : card list) =
	case pCardList of
    [] => false
  | x::pCardList' => if all_same_color_aux(pCardList, Red)
  					 then true
  					 else if all_same_color_aux(pCardList, Black)
  					 	  then true
  					 	  else false

val test9 = all_same_color([(Diamonds, Queen), (Clubs, Jack), (Clubs, Jack)]) = false;

val test10 = all_same_color([(Clubs, Jack), (Clubs, Jack)]) = true;