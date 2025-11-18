(* types.ml *)

(* ======== Cards, Players, Melds ======== *)
open! Ppx_yojson_conv_lib.Yojson_conv

type suit = Hearts | Spades | Diamonds | Clubs
[@@deriving yojson]

type rank =
  | Two | Three | Four | Five | Six | Seven
  | Eight | Nine | Ten | Jack | Queen | King | Ace
[@@deriving yojson]

type card = { suit : suit; rank : rank }
[@@deriving yojson]

type meld_kind = Set | Run
[@@deriving yojson]

type meld = { kind : meld_kind; cards : card list }
[@@deriving yojson]

type player = {
  id : int;
  name : string;
  hand : card list;
}
[@@deriving yojson]

(* ======== Config / Policy ======== *)

type ace_policy = Low | High
[@@deriving yojson]

type scoring = {
  deadwood_threshold : int option;
}
[@@deriving yojson]

(* ======== Turn Phases & Actions ======== *)

type phase = Draw | Play | Discard | EndCheck
[@@deriving yojson]

type play_action =
  | Make_set of card list
  | Make_run of card list
  | Lay_off of card * int
  | Skip_to_discard
[@@deriving yojson]

type discard_action = Discard_card of card
[@@deriving yojson]

(* ======== Whole Game State ======== *)

type draw_source =
  | FromDeck
  | FromDiscard
  | FromDiscardN of int
[@@deriving yojson]

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
[@@deriving yojson]

(* ======== JSON helpers for Firebase ======== *)

let serialize_state (st : state) : string =
  yojson_of_state st |> Yojson.Safe.to_string

let deserialize_state (s : string) : state option =
  try
    let json = Yojson.Safe.from_string s in
    Some (state_of_yojson json)
  with _ ->
    None

(* ======== Pretty helpers ======== *)

let string_of_suit = function
  | Hearts -> "♥"
  | Spades -> "♠"
  | Diamonds -> "♦"
  | Clubs -> "♣"

let string_of_rank = function
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Seven -> "7"
  | Eight -> "8"
  | Nine -> "9"
  | Ten -> "10"
  | Jack -> "J"
  | Queen -> "Q"
  | King -> "K"
  | Ace -> "A"

let string_of_card c =
  Printf.sprintf "%s%s" (string_of_rank c.rank) (string_of_suit c.suit)

type t = {
  foo : int;
  bar : string;
} [@@deriving yojson]