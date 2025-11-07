open! Core
module S = Rummy_engine.Setup
module T = Rummy_engine.Types
module E = Rummy_engine.Engine

(* deterministic init: don't shuffle, just take from S.all_cards *)
let init_state_deterministic (n : int) : T.state =
  let hand_size = 7 in
  let rec deal_k_hands k deck acc =
    if k = 0 then (List.rev acc, deck)
    else
      let hand, deck' = List.split_n deck hand_size in
      deal_k_hands (k - 1) deck' (hand :: acc)
  in
  let hands, deck_after = deal_k_hands n S.all_cards [] in
  let players =
    hands
    |> List.mapi ~f:(fun i h ->
         { T.id = i
         ; name = sprintf "Player %d" (i + 1)
         ; hand = h })
    |> Array.of_list
  in
  { T.deck = deck_after
  ; discard = []
  ; melds = []
  ; players
  ; current = 0
  ; phase = T.Draw
  ; ace_policy = T.Low
  ; scoring = { T.deadwood_threshold = None }
  ; required_to_use = None
  ; scores = Array.create ~len:n 0
  }

let string_of_phase = function
  | T.Draw -> "Draw"
  | T.Play -> "Play"
  | T.Discard -> "Discard"
  | T.EndCheck -> "EndCheck"

let%expect_test "initial deterministic state" =
  let st = init_state_deterministic 2 in
  (* 52 total - 2 players * 7 cards = 38 left in deck *)
  printf "deck=%d discard=%d players=%d phase=%s\n"
    (List.length st.deck)
    (List.length st.discard)
    (Array.length st.players)
    (string_of_phase st.phase);
  [%expect {| deck=38 discard=0 players=2 phase=Draw |}]

let%expect_test "draw from deck moves to Play and shrinks deck" =
  let st = init_state_deterministic 2 in
  let before = List.length st.deck in
  let st' =
    match E.draw ~source:T.FromDeck st with
    | Ok s | E.End_round s -> s
    | Error _ -> failwith "draw failed in expect test"
  in
  printf "deck(before)=%d deck(after)=%d phase=%s\n"
    before
    (List.length st'.deck)
    (string_of_phase st'.phase);
  [%expect {| deck(before)=38 deck(after)=37 phase=Play |}]

let%expect_test "skip to discard moves to Discard" =
  let st = init_state_deterministic 2 in
  (* must draw first to get to Play *)
  let st1 =
    match E.draw ~source:T.FromDeck st with
    | Ok s | E.End_round s -> s
    | Error _ -> failwith "draw failed"
  in
  let st2 =
    match E.play ~action:T.Skip_to_discard st1 with
    | Ok s | E.End_round s -> s
    | Error _ -> failwith "skip to discard failed"
  in
  printf "phase=%s discard=%d\n"
    (string_of_phase st2.phase)
    (List.length st2.discard);
  [%expect {| phase=Discard discard=0 |}]