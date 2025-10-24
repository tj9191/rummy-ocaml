(* types.ml *)

(* ======== Cards, Players, Melds ======== *)

type suit = Hearts | Spades | Diamonds | Clubs

type rank =
  | Two | Three | Four | Five | Six | Seven
  | Eight | Nine | Ten | Jack | Queen | King | Ace

type card = { suit : suit; rank : rank }

type meld_kind = Set | Run
type meld = { kind : meld_kind; cards : card list }

type player = {
  id : int;
  name : string;
  hand : card list;
}

(* ======== Config / Policy ======== *)

type ace_policy = Low | High
type scoring = {
  deadwood_threshold : int option;
}

(* ======== Turn Phases & Actions ======== *)

type phase = Draw | Play | Discard | EndCheck

type play_action =
  | Make_set of card list
  | Make_run of card list
  | Lay_off of card * int   (* card, meld index *)
  | Skip_to_discard

type discard_action = Discard_card of card

(* ======== Whole Game State ======== *)

type draw_source =
  | FromDeck
  | FromDiscard
  | FromDiscardN of int

type state = {
  deck : card list;
  discard : card list;
  melds : meld list;
  players : player array;
  current : int;
  phase : phase;
  ace_policy : ace_policy;
  scoring : scoring;
  required_to_use : card option;
  scores : int array;
}

(* ======== Pretty helpers ======== *)

let string_of_suit = function
  | Hearts -> "♥" | Spades -> "♠" | Diamonds -> "♦" | Clubs -> "♣"

let string_of_rank = function
  | Two->"2"|Three->"3"|Four->"4"|Five->"5"|Six->"6"|Seven->"7"
  | Eight->"8"|Nine->"9"|Ten->"10"|Jack->"J"|Queen->"Q"|King->"K"|Ace->"A"

let string_of_card c = Printf.sprintf "%s%s" (string_of_rank c.rank) (string_of_suit c.suit)