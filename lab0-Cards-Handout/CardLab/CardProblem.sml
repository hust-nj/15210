structure CardProblem =
struct
open CardType
exception NYI
fun card_color((suit,rank):card) = case suit of
    	Clubs => Black
    	|Spades=>Black
    	|Diamonds=>Red
    	|Hearts=>Red


fun card_value((suit,rank):card) = case rank of
	Num a =>a
	|Ace =>11
	|_ =>10

fun remove_card(cs:card list,c:card) = 
	case cs of
	[] => raise IllegalMove
	| x::y=> if x=c then y
			 else x::remove_card(y,c)

fun all_same_color (cs:card list) =
	case cs of
		[]=>true
		|_::[]=>true
		|x::y=>(card_color(x)=card_color(hd(y)))andalso(all_same_color(y))

fun sum_cards(cs:card list)=
	let fun sum(cs,help) =
		case cs of
			[]=>help
			|x::y=>sum(y,help+card_value(x))
	in sum(cs,0)
	end

fun score(cs:card list,goal:int) =
	let val preliminary=
		let val sum=sum_cards(cs)
		in if sum > goal then (sum-goal)*3
		else (goal-sum)
		end
	in if all_same_color(cs) then preliminary div 2
		else preliminary
	end

fun officiate(card_list:card list,move_list:move list,goal:int)=
	let
		fun inner_officiate (card_list: card list, held_cards :card list,move_list:move list)=
			case move_list of
			[]=>held_cards
			|(Discard name_of_removed_cards)::tl=>inner_officiate (card_list, remove_card(held_cards,name_of_removed_cards) ,tl)
			|Draw::tl=>
				case card_list of
				[]=>held_cards
				|first_on_the_card_list::left_the_card_list:card list=>if(sum_cards(first_on_the_card_list::held_cards)>goal)
					then first_on_the_card_list::held_cards 
					else inner_officiate (left_the_card_list ,first_on_the_card_list::held_cards, tl)
				
	in
		score(inner_officiate (card_list, [] ,move_list),goal)
	end
end