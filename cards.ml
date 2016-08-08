(* Type for face values of cards *)
type face_value =
  | Ace | Two | Three | Four | Five
  | Six | Seven | Eight | Nine | Ten
  | Jack | Queen | King

(* Type for card suits*)
type suit =
  | Diamonds | Clubs | Hearts | Spades

(* Type for cards *)
type card = suit * face_value

(* Exceptions to throw *)
exception InvalidCard
exception NotEnoughCards

(* Convert a face_value to a string*)
let string_of_face(f: face_value): string =
  match f with
  | Ace -> "Ace"
  | Two -> "Two"
  | Three -> "Three"
  | Four -> "Four"
  | Five -> "Five"
  | Six -> "Six"
  | Seven -> "Seven"
  | Eight -> "Eight"
  | Nine -> "Nine"
  | Ten -> "Ten"
  | Jack -> "Jack"
  | Queen -> "Queen"
  | King -> "King"

(* Convert a suit to a string *)
let string_of_suit (s : suit) : string =
  match s with
  | Clubs -> "Clubs"
  | Diamonds -> "Diamonds"
  | Hearts -> "Hearts"
  | Spades -> "Spades"

(* Convert a card to a string *)
let string_of_card ((s, f) : card) : string =
  Printf.sprintf "%s of %s"
		 (string_of_face(f))
		 (string_of_suit(s))		 

(* Convert an integer to a face_value *)
let int_of_face (f : face_value) : int =
  match f with
  | Ace -> 0 
  | Two  -> 1 
  | Three  -> 2 
  | Four  -> 3 
  | Five -> 4
  | Six  -> 5 
  | Seven  -> 6 
  | Eight  -> 7 
  | Nine  -> 8 
  | Ten  -> 9 
  | Jack  -> 10 
  | Queen  -> 11 
  | King  -> 12

(* Convert an integer to a face_value *)
let face_from_int (n : int) : face_value =
  match n with
  | 0 -> Ace
  | 1 -> Two
  | 2 -> Three
  | 3 -> Four
  | 4 -> Five
  | 5 -> Six
  | 6 -> Seven
  | 7 -> Eight
  | 8 -> Nine
  | 9 -> Ten
  | 10 -> Jack
  | 11 -> Queen
  | 12 -> King
  | _ -> raise InvalidCard

(* Convert a suit to an integer *)
let int_of_suit (s : suit ) : int =
  match s with
  | Clubs -> 0
  | Diamonds -> 1
  | Hearts -> 2
  | Spades -> 3

(* Convert an integer to a suit *)
let suit_from_int (n : int) : suit =
  match n with
  | 0 -> Clubs
  | 1 -> Diamonds
  | 2 -> Hearts
  | 3 -> Spades
  | _ -> raise InvalidCard

(* Return the nth card in a card list *)
let pick_a_card (d : card list) : int -> card =
  List.nth d

(* Return a random card index in a card list *)
let any_card_int (d: card list) : int = 
  let () = Random.self_init() in
  Random.int (List.length d)
	     
(* Return a random card in a card list *)
let any_card (d: card list) : card =
  pick_a_card d (any_card_int d)
	      
(* Remove a card from a list, plus the new card list without that card  *)
let take_a_card (d:card list) (x:int): card * card list =
  let rec rc_inner (n:int) (left: card list)
		   (right: card list) : card * card list =
    if x == n then ((List.hd right), (left @ (List.tl right)))
    else rc_inner (n + 1) (left @ [List.hd right]) (List.tl right) in
  if x < List.length d then rc_inner 0 [] d
  else raise InvalidCard

(* Take the first x cards from a deck. *)
(* Return a tuple containing the hand and the new deck *)
(* with the cards removed *)
let take_x_cards (d:card list) (x:int) : card list * card list =
  let rec txc_inner (c: card list) (d': card list) (y:int) =
    (* let  a_card, new_deck = take_a_card d' (any_card_int d') in *)
    let  a_card, new_deck = take_a_card d' 0 in
    (* let () = Printf.printf "Drew the %s.\n" (string_of_card (a_card)) in  *)
    let hand = a_card :: c in
    if y - 1 == 0 then hand, new_deck
    else txc_inner hand new_deck (y - 1) in
  txc_inner [] d x

(* Generate a deck of cards in order *)
let deck () : card list =
  let rec deck_inner (deck_so_far : card list)
		     (card_n : int) : card list =
    let suit_n = card_n / 13 in 
    let face_n = card_n mod 13 in 
    if card_n == 52 || suit_n > 3 then deck_so_far
    else
      deck_inner ((suit_from_int (suit_n), face_from_int (face_n))
		  :: deck_so_far)
		 (card_n +1) in
  deck_inner [] 0 

(* Compare two faces *)
let compare_faces ((_, face_a) : card) ((_, face_b) : card) : int =
  int_of_face face_a - int_of_face face_b

(* Compare two cards *)
let compare_cards ((suit_a, face_a) : card) ((suit_b, face_b) : card) : int =
  if int_of_suit suit_a == int_of_suit suit_b
  then int_of_face face_a - int_of_face face_b
  else int_of_suit suit_a - int_of_suit suit_b

(* Return cards in sorted order *)
let sort_cards  = List.sort compare_cards

(* Shuffle a given deck *)
let shuffle (d: card list) : card list =
  let rec s_inner (so_far : card list) (remaining : card list) =
    match remaining with
    | [] -> so_far
    | _ -> let next, new_deck = take_a_card remaining (any_card_int remaining) in
	   s_inner (next :: so_far) new_deck in
  s_inner [] d

(* Test whether two hands match *)
let hands_match (hand_a: card list) (hand_b: card list) : bool =
  let rec hm_inner (n:int) : bool =
    if n == List.length hand_a then true
    else
      if List.mem (List.nth hand_a n) hand_b
      then hm_inner (n + 1)
      else false in
  if
    List.length hand_a == List.length hand_b
  then hm_inner 0
  else false

(* Convert a list of cards into a string *)
let string_of_hand (hand : card list) : string =
  let rec ph_inner (hand_str : string)
	  (left_in_hand : card list) : string = 
    match left_in_hand with
    | [] -> Printf.sprintf "%s" hand_str
    | h :: t -> ph_inner (Printf.sprintf "%s; %s" (string_of_card h) hand_str)
			     t in
  ph_inner "" hand
