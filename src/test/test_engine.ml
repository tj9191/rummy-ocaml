open! Core
module T = Rummy_engine.Types
module E = Rummy_engine.Engine
module S = Rummy_engine.Setup

let init_state_local (n : int) ~(vs_computer : bool) : T.state =
  let deck0 = S.shuffle S.all_cards in
  let hand_size = 7 in
  let rec deal_k_hands k deck acc =
    if k = 0 then (List.rev acc, deck)
    else
      let rec take m acc d =
        if m = 0 then (List.rev acc, d)
        else match d with
          | [] -> (List.rev acc, [])
          | x::xs -> take (m-1) (x::acc) xs
      in
      let (h, deck') = take hand_size [] deck in
      deal_k_hands (k - 1) deck' (h :: acc)
  in
  let (hands, deck_after) = deal_k_hands n deck0 [] in
  let players =
    hands
    |> List.mapi ~f:(fun i h ->
         { T.id = i
         ; name = if vs_computer && i = 1 then "Computer" else sprintf "Player %d" (i+1)
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

let total_hand_cards (st : T.state) : int =
  st.players |> Array.to_list |> List.sum (module Int) ~f:(fun p -> List.length p.hand)

(* Move from Draw -> Play (by drawing) -> Discard (by skipping). *)
let go_to_discard (st0 : T.state) : T.state =
  match E.draw ~source:T.FromDeck st0 with
  | Ok st1 | E.End_round st1 ->
    (match E.play ~action:T.Skip_to_discard st1 with
     | Ok st2 | E.End_round st2 -> st2
     | Error _ -> st1)
  | Error _ -> st0

let%test "draw from deck reduces deck by 1" =
  let st = init_state_local 2 ~vs_computer:false in
  let before = List.length st.deck in
  match E.draw ~source:T.FromDeck st with
  | Ok st' | E.End_round st' -> List.length st'.deck = before - 1
  | Error _ -> false

let%test "draw from discard (top) reduces discard when non-empty" =
  let st0 = init_state_local 2 ~vs_computer:false in
  let st1 = go_to_discard st0 in
  (* Put a known card on discard *)
  let p1 = st1.players.(st1.current) in
  let st2 =
    match p1.hand with
    | [] -> st1
    | c :: _ ->
      (match E.discard ~action:(T.Discard_card c) st1 with
       | Ok s | E.End_round s -> s
       | Error _ -> st1)
  in
  let before = List.length st2.discard in
  if before = 0 then true
  else
    match E.draw ~source:T.FromDiscard st2 with
    | Ok st3 | E.End_round st3 -> List.length st3.discard = before - 1
    | Error _ -> false

let%test "skip to discard enters Discard phase" =
  let st = init_state_local 2 ~vs_computer:false in
  match E.play ~action:T.Skip_to_discard st with
  | Ok st' | E.End_round st' ->
    (match st'.phase with T.Discard -> true | _ -> false)
  | Error _ -> false

let%test "discard removes a card from hand and grows discard" =
  let st0 = init_state_local 2 ~vs_computer:false in
  let st1 = go_to_discard st0 in
  let cur_before : int = st1.current in
  let hands_before = total_hand_cards st1 in
  let discard_before = List.length st1.discard in
  match st1.players.(cur_before).hand with
  | [] -> true
  | c :: _ ->
    (match E.discard ~action:(T.Discard_card c) st1 with
     | Ok st2 | E.End_round st2 ->
       total_hand_cards st2 = hands_before - 1
       && List.length st2.discard = discard_before + 1
       && (match st2.discard with d::_ -> String.(Rummy_engine.Types.string_of_card d
                                                  = Rummy_engine.Types.string_of_card c)
                                 | [] -> false)
     | Error _ -> false)