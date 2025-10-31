(* rules.ml *)
open Types

(* ---------- Helpers ---------- *)

let all_same_rank (cs : card list) =
  match cs with
  | [] | [_] -> false
  | c :: rest -> List.for_all (fun x -> x.rank = c.rank) rest

let distinct_suits (cs : card list) =
  let module S = Set.Make(struct type t = suit let compare = compare end) in
  let s = List.fold_left (fun acc c -> S.add c.suit acc) S.empty cs in
  S.cardinal s = List.length cs

let rank_value ~ace_policy = function
  | Two->2 | Three->3 | Four->4 | Five->5 | Six->6 | Seven->7
  | Eight->8 | Nine->9 | Ten->10 | Jack->11 | Queen->12 | King->13
  | Ace -> (match ace_policy with Low -> 1 | High -> 14)

let rec is_consecutive_ints = function
  | [] | [_] -> true
  | a :: b :: rest -> (b = a + 1) && is_consecutive_ints (b :: rest)

(* ---------- Meld predicates ---------- *)

let is_set (cs : card list) : bool =
  List.length cs >= 3 && all_same_rank cs && distinct_suits cs

let is_run ~ace_policy (cs : card list) : bool =
  match cs with
  | [] | [_] | [_; _] -> false
  | c :: rest ->
      let same_suit = List.for_all (fun x -> x.suit = c.suit) rest in
      if not same_suit then false
      else
        cs
        |> List.map (fun x -> rank_value ~ace_policy x.rank)
        |> List.sort compare
        |> is_consecutive_ints

let valid_meld ~ace_policy cards =
  is_set cards || is_run ~ace_policy cards

let can_layoff ~ace_policy (c : card) (m : meld) : bool =
  valid_meld ~ace_policy (c :: m.cards)

let deadwood_points (_hand : card list) : int = 0