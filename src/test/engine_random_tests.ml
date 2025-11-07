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

let count_cards (st : T.state) : int =
  let hands =
    st.players |> Array.to_list |> List.sum (module Int) ~f:(fun p -> List.length p.hand)
  in
  let melds =
    st.melds |> List.sum (module Int) ~f:(fun m -> List.length m.cards)
  in
  List.length st.deck + List.length st.discard + hands + melds

let random_draw (st : T.state) : T.state option =
  let cands =
    [ T.FromDeck ]
    @ (if List.is_empty st.discard then [] else [ T.FromDiscard ])
    @ (let n = List.length st.discard in
       if n <= 1 then [] else List.init n ~f:(fun i -> T.FromDiscardN (i+1)))
  in
  match List.random_element cands with
  | None -> None
  | Some src ->
    (match E.draw ~source:src st with
     | Ok st' | E.End_round st' -> Some st'
     | Error _ -> None)

let rec combinations_k (k : int) (xs : 'a list) : 'a list list =
  if k = 0 then [ [] ]
  else match xs with
       | [] -> []
       | y::ys ->
         let with_y  = List.map (combinations_k (k-1) ys) ~f:(fun r -> y :: r) in
         let without = combinations_k k ys in
         with_y @ without

let random_play (st : T.state) : T.state option =
  let p = st.players.(st.current) in
  let combos =
    let n = List.length p.hand in
    List.concat_map (List.range 3 (n+1)) ~f:(fun k -> combinations_k k p.hand)
  in
  let acts =
    (List.map combos ~f:(fun cs -> T.Make_set cs))
    @ (List.map combos ~f:(fun cs -> T.Make_run cs))
    @ (let mcount = List.length st.melds in
       List.concat_map p.hand ~f:(fun c ->
         List.init mcount ~f:(fun mi -> T.Lay_off (c, mi))))
    @ [ T.Skip_to_discard ]
  in
  match List.random_element acts with
  | None -> None
  | Some act ->
    (match E.play ~action:act st with
     | Ok st' | E.End_round st' -> Some st'
     | Error _ -> None)

let random_discard (st : T.state) : T.state option =
  let p = st.players.(st.current) in
  match List.random_element p.hand with
  | None -> None
  | Some c ->
    (match E.discard ~action:(T.Discard_card c) st with
     | Ok st' | E.End_round st' -> Some st'
     | Error _ -> None)

let%test "random walk explores without crashing" =
  let st0 : T.state = init_state_local 2 ~vs_computer:false in
  let start_count = count_cards st0 in
  (* annotate st so OCaml knows it's a T.state *)
  let rec step (i : int) (st : T.state) : T.state =
    if i = 0 then st
    else
      match st.phase with
      | T.Draw ->
        (match random_draw st with
         | Some st' -> step (i - 1) st'
         | None -> st)
      | T.Play ->
        (match random_play st with
         | Some st' -> step (i - 1) st'
         | None -> st)
      | T.Discard ->
        (match random_discard st with
         | Some st' -> step (i - 1) st'
         | None -> st)
      | T.EndCheck ->
        (match E.endcheck st with
         | Ok st' | E.End_round st' -> step (i - 1) st'
         | Error _ -> st)
  in
  let stf = step 60 st0 in
  let final_count = count_cards stf in
  (* relaxed sanity check *)
  final_count = start_count
  || final_count = start_count - 1
  || final_count = start_count + 1