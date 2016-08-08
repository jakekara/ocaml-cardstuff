open Cards

(* Draw every card from a list of cards and print them *)
let rec take_all_cards (d:card list) : unit =
  match d with
  | [] -> Printf.printf "That's it!\n"
  | _ :: _ -> let a_card, new_deck = take_a_card d 0 in
	      let () = Printf.printf "%s, %d cards left\n"
				     (string_of_card (a_card))
				     (List.length new_deck) in 
	      take_all_cards new_deck

(* Test whether hand is a royal flush *)
let royal_flush (hand : card list) : bool =
  let suit, _ = List.nth hand 0 in
  hands_match hand
	      ([(suit, Ace);(suit, King);
		(suit, Queen);(suit, Jack);(suit, Ten)])

(* Shuffle a deck and draw the first five cards until a *)
let try_for_hand (hand_test : (card list) -> bool) () : int =
  let rec tff_inner  (n: int): int  =
    let h, _ = (take_x_cards (shuffle (deck())) 5) in
    (* let () = if n mod 100000 == 0 then *)
    (* 	       let _ = Printf.printf "trial %d:\t%s\n" *)
    (* 				     n (string_of_hand h) in *)
    (* 	       flush stdout *)
    (* 	     else () in *)
    if hand_test h then
      (* if hands_match poker_hand poker_hand then *)
      let () = Printf.printf "Royal flush! It took %d attempts!\n%s\n"
			     n (string_of_hand h) in
      let () = flush stdout in n
    else tff_inner (n + 1) in
  tff_inner 0 

(* Try a function f n times and return the average ret val *)
(* of that function. Function must return an integer *)
let try_n_times f (n : int) : int = 
  let rec tnt_inner n' c : int =
    (* let () = Printf.printf "Trial: %d, c: %d\n" n' c in *)
    (* let () = flush stdout in  *)
    if n' > n then c / n
    else tnt_inner (n' + 1) (c + f ()) in
  tnt_inner 1 0
	    
(* let () = take_all_cards (shuffle (shuffle (deck()))) *)
(* let () = take_all_cards (shuffle (shuffle (deck()))) *)
(* let () = take_all_cards (shuffle (shuffle (deck()))) *)
(* let () = take_all_cards (sort_cards (shuffle (deck()))) *)
	    
(* let poker_hand, _ = take_x_cards deck 5 *)

	    
(* let () = try_for_flush  *)
let trials = 10
let () = Printf.printf "Completed %d trials; Average deals required: %d\n"
		       trials (try_n_times (try_for_hand royal_flush) trials)		       
