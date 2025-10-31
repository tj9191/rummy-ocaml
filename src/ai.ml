(* src/ai.ml *)

open! Core
open Types
module T = Types
module E = Engine
module R = Rules
[@@@ocaml.warning "-32"]  (* disable unused-value-declaration warnings *)

let choose_random (rng : Random.State.t) (xs : 'a list) : 'a option =
  match xs with
  | [] -> None
  | _  -> Some (List.nth_exn xs (Random.State.int rng (List.length xs)))

let rec combinations_k (k : int) (xs : 'a list) : 'a list list =
  if k = 0 then [ [] ]
  else
    match xs with
    | [] -> []
    | y :: ys ->
        let with_y  = List.map (combinations_k (k - 1) ys) ~f:(fun rest -> y :: rest) in
        let without = combinations_k k ys in
        with_y @ without

let all_3plus_combos (hand : 'a list) : 'a list list =
  let n = List.length hand in
  List.concat_map (List.range 3 (n + 1)) ~f:(fun k -> combinations_k k hand)

(* apply helpers that "swallow" End_round *)
let apply_draw ~source (st : T.state) : T.state option =
  match E.draw ~source st with
  | Ok st' | E.End_round st' -> Some st'
  | Error _ -> None

let apply_play ~action (st : T.state) : T.state option =
  match E.play ~action st with
  | Ok st' | E.End_round st' -> Some st'
  | Error _ -> None

let apply_discard ~card (st : T.state) : T.state option =
  match E.discard ~action:(T.Discard_card card) st with
  | Ok st' | E.End_round st' -> Some st'
  | Error _ -> None

let apply_endcheck (st : T.state) : T.state option =
  match E.endcheck st with
  | Ok st' | E.End_round st' -> Some st'
  | Error _ -> None

let candidate_draws (st : T.state) : T.draw_source list =
  let from_deck     = [ T.FromDeck ] in
  let from_discard1 = if List.is_empty st.discard then [] else [ T.FromDiscard ] in
  let from_discardN =
    let len = List.length st.discard in
    if len <= 1 then []
    else List.init len ~f:(fun i -> T.FromDiscardN (i + 1))
  in
  from_deck @ from_discard1 @ from_discardN

let candidate_plays (st : T.state) : T.play_action list =
  let p = st.players.(st.current) in
  let set_actions =
    all_3plus_combos p.hand |> List.map ~f:(fun cs -> T.Make_set cs)
  in
  let run_actions =
    all_3plus_combos p.hand |> List.map ~f:(fun cs -> T.Make_run cs)
  in
  let lay_actions =
    let mcount = List.length st.melds in
    List.concat_map p.hand ~f:(fun c ->
      List.init mcount ~f:(fun mi -> T.Lay_off (c, mi)))
  in
  let skip = [ T.Skip_to_discard ] in
  set_actions @ run_actions @ lay_actions @ skip

let candidate_discards (st : T.state) : T.card list =
  let p = st.players.(st.current) in
  p.hand

(* keep only legal ones *)
let legalize_draws (st : T.state) : T.draw_source list =
  candidate_draws st
  |> List.filter_map ~f:(fun src ->
       Option.map (apply_draw ~source:src st) ~f:(fun _ -> src))

let legalize_plays (st : T.state) : T.play_action list =
  candidate_plays st
  |> List.filter_map ~f:(fun act ->
       Option.map (apply_play ~action:act st) ~f:(fun _ -> act))

let legalize_discards (st : T.state) : T.card list =
  candidate_discards st
  |> List.filter_map ~f:(fun c ->
       Option.map (apply_discard ~card:c st) ~f:(fun _ -> c))

type policy = Random.State.t -> T.state -> T.state option

let force_skip_then_discard (st : T.state) : T.state option =
  match E.play ~action:T.Skip_to_discard st with
  | Ok st_d | E.End_round st_d -> (
      let p = st_d.players.(st_d.current) in
      match p.hand with
      | [] -> apply_endcheck st_d
      | c :: _ -> apply_discard ~card:c st_d
    )
  | Error _ -> None

let force_discard_from_discard_phase (st : T.state) : T.state option =
  let p = st.players.(st.current) in
  match p.hand with
  | [] -> apply_endcheck st
  | c :: _ -> apply_discard ~card:c st

let random_ai : policy =
 fun rng st ->
  match st.T.phase with
  | T.Draw ->
      let draws = legalize_draws st in
      (match choose_random rng draws with
       | Some src -> apply_draw ~source:src st
       | None -> apply_endcheck st)
  | T.Play ->
      let plays = legalize_plays st in
      (match choose_random rng plays with
       | Some act -> apply_play ~action:act st
       | None -> force_skip_then_discard st)
  | T.Discard ->
      let discards = legalize_discards st in
      (match choose_random rng discards with
       | Some c -> apply_discard ~card:c st
       | None -> force_discard_from_discard_phase st)
  | T.EndCheck ->
      apply_endcheck st

let card_points (c : T.card) : int =
  match c.rank with
  | T.Ace -> 15
  | T.King | T.Queen | T.Jack | T.Ten -> 10
  | _ -> 5

let hand_penalty (hand : T.card list) : int =
  List.fold hand ~init:0 ~f:(fun acc c -> acc + card_points c)

(* view: positive is good for player 0, negative good for player 1 *)
let eval_state (st : T.state) : int =
  let s0 = st.scores.(0) in
  let s1 = st.scores.(1) in
  let h0 = st.players.(0).hand |> hand_penalty in
  let h1 = st.players.(1).hand |> hand_penalty in
  (* "score lead" minus "my deadwood" plus "their deadwood" *)
  (s0 - s1) - h0 + h1

let successors (st : T.state) : T.state list =
  match st.phase with
  | T.Draw ->
      legalize_draws st
      |> List.filter_map ~f:(fun src -> apply_draw ~source:src st)

  | T.Play ->
      let plays = legalize_plays st in
      if List.is_empty plays then
        (* no legal play → force the usual “skip → discard” path *)
        (match force_skip_then_discard st with
         | None -> []
         | Some st' -> [ st' ])
      else
        List.filter_map plays ~f:(fun act -> apply_play ~action:act st)

  | T.Discard ->
      let discards = legalize_discards st in
      if List.is_empty discards then
        (match force_discard_from_discard_phase st with
         | None -> []
         | Some st' -> [ st' ])
      else
        List.filter_map discards ~f:(fun c -> apply_discard ~card:c st)

  | T.EndCheck ->
      (match apply_endcheck st with
       | None -> []
       | Some st' -> [ st' ])

let eval_state (st : T.state) : int =
  st.scores.(0) - st.scores.(1)

let rec minimax_eval ~(depth:int) ~(maximizing:bool) (st : T.state) : int =
  if depth = 0 then
    eval_state st
  else
    let succs = successors st in
    if List.is_empty succs then
      eval_state st
    else if maximizing then
      (* our turn: pick max *)
      List.fold succs ~init:Int.min_value ~f:(fun best st' ->
        Int.max best (minimax_eval ~depth:(depth - 1) ~maximizing:false st'))
    else
      (* opponent turn: pick min *)
      List.fold succs ~init:Int.max_value ~f:(fun best st' ->
        Int.min best (minimax_eval ~depth:(depth - 1) ~maximizing:true st'))

let minimax_ai ?(depth=2) : policy =
 fun _rng st ->
  let my_turn = st.current in
  let succs = successors st in
  match succs with
  | [] -> apply_endcheck st
  | _ ->
      let best_state, _ =
        List.fold succs
          ~init:(List.hd_exn succs, (if my_turn = 0 then Int.min_value else Int.max_value))
          ~f:(fun (best_st, best_val) cand ->
            let v =
              minimax_eval
                ~depth:(depth-1)
                ~maximizing:(cand.current = 0)
                cand
            in
            if my_turn = 0 then
              (* we want bigger *)
              if v > best_val then (cand, v) else (best_st, best_val)
            else
              (* we want smaller *)
              if v < best_val then (cand, v) else (best_st, best_val))
      in
      Some best_state

let simulate_to_end
    (rng : Random.State.t)
    (pol0 : policy)
    (pol1 : policy)
    (st0  : T.state)
  : int
  =
  let max_steps = 20_000 in
  let rec loop steps st =
    if steps <= 0 then
      st.current
    else
      let current = st.T.current in
      let pol = if current = 0 then pol0 else pol1 in
      match pol rng st with
      | None ->
          (match apply_endcheck st with
           | None -> current
           | Some st' ->
               let empty0 = List.is_empty st'.players.(0).hand in
               let empty1 = List.is_empty st'.players.(1).hand in
               if empty0 && not empty1 then 0
               else if empty1 && not empty0 then 1
               else loop (steps - 1) st')
      | Some st' ->
          let empty0 = List.is_empty st'.players.(0).hand in
          let empty1 = List.is_empty st'.players.(1).hand in
          if empty0 && not empty1 then 0
          else if empty1 && not empty0 then 1
          else loop (steps - 1) st'
  in
  loop max_steps st0

let timed_pick_draw
    (rng : Random.State.t)
    (st : T.state)
    (choices : T.draw_source list)
    ~(opponent : policy)
    (time_ms : int)
  : T.state option =
  match choices with
  | [] -> None
  | _ ->
      let deadline =
        Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_int_ms time_ms)
      in
      let scores = Array.of_list (List.map choices ~f:(fun _ -> 0, 0)) in
      let i_ref = ref 0 in
      while Time_ns.(now () < deadline) do
        let i = !i_ref in
        i_ref := (i + 1) % (Array.length scores);
        let src = List.nth_exn choices i in
        match apply_draw ~source:src st with
        | None -> ()
        | Some st_after ->
            let us = st.T.current in
            let winner =
              if us = 0
              then simulate_to_end rng random_ai opponent st_after
              else simulate_to_end rng opponent random_ai st_after
            in
            let w, n = scores.(i) in
            let w' = if winner = us then w + 1 else w in
            scores.(i) <- (w', n + 1)
      done;
      let best_i, _best =
        Array.foldi scores ~init:(0, Float.neg_infinity)
          ~f:(fun i (best_i, best_score) (w, n) ->
            let score =
              if n = 0 then Float.neg_infinity
              else Float.(of_int w / of_int n)
            in
            if Float.(score > best_score) then (i, score) else (best_i, best_score))
      in
      apply_draw ~source:(List.nth_exn choices best_i) st

let timed_pick_play
    (rng : Random.State.t)
    (st : T.state)
    (choices : T.play_action list)
    ~(opponent : policy)
    (time_ms : int)
  : T.state option =
  match choices with
  | [] -> None
  | _ ->
      let deadline =
        Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_int_ms time_ms)
      in
      let scores = Array.of_list (List.map choices ~f:(fun _ -> 0, 0)) in
      let i_ref = ref 0 in
      while Time_ns.(now () < deadline) do
        let i = !i_ref in
        i_ref := (i + 1) % (Array.length scores);
        let act = List.nth_exn choices i in
        match apply_play ~action:act st with
        | None -> ()
        | Some st_after ->
            let us = st.T.current in
            let winner =
              if us = 0
              then simulate_to_end rng random_ai opponent st_after
              else simulate_to_end rng opponent random_ai st_after
            in
            let w, n = scores.(i) in
            let w' = if winner = us then w + 1 else w in
            scores.(i) <- (w', n + 1)
      done;
      let best_i, _best =
        Array.foldi scores ~init:(0, Float.neg_infinity)
          ~f:(fun i (best_i, best_score) (w, n) ->
            let score =
              if n = 0 then Float.neg_infinity
              else Float.(of_int w / of_int n)
            in
            if Float.(score > best_score) then (i, score) else (best_i, best_score))
      in
      apply_play ~action:(List.nth_exn choices best_i) st

let timed_pick_discard
    (rng : Random.State.t)
    (st : T.state)
    (choices : T.card list)
    ~(opponent : policy)
    (time_ms : int)
  : T.state option =
  match choices with
  | [] -> None
  | _ ->
      let deadline =
        Time_ns.add (Time_ns.now ()) (Time_ns.Span.of_int_ms time_ms)
      in
      let scores = Array.of_list (List.map choices ~f:(fun _ -> 0, 0)) in
      let i_ref = ref 0 in
      while Time_ns.(now () < deadline) do
        let i = !i_ref in
        i_ref := (i + 1) % (Array.length scores);
        let c = List.nth_exn choices i in
        match apply_discard ~card:c st with
        | None -> ()
        | Some st_after ->
            let us = st.T.current in
            let winner =
              if us = 0
              then simulate_to_end rng random_ai opponent st_after
              else simulate_to_end rng opponent random_ai st_after
            in
            let w, n = scores.(i) in
            let w' = if winner = us then w + 1 else w in
            scores.(i) <- (w', n + 1)
      done;
      let best_i, _best =
        Array.foldi scores ~init:(0, Float.neg_infinity)
          ~f:(fun i (best_i, best_score) (w, n) ->
            let score =
              if n = 0 then Float.neg_infinity
              else Float.(of_int w / of_int n)
            in
            if Float.(score > best_score) then (i, score) else (best_i, best_score))
      in
      apply_discard ~card:(List.nth_exn choices best_i) st

let timed_ai ?(time_ms = 2000) ~(opponent : policy) : policy =
  fun rng st ->
    match st.T.phase with
    | T.Draw ->
        let cands = legalize_draws st in
        timed_pick_draw rng st cands ~opponent time_ms
    | T.Play ->
        let cands = legalize_plays st in
        (match timed_pick_play rng st cands ~opponent time_ms with
         | Some st' -> Some st'
         | None -> force_skip_then_discard st)
    | T.Discard ->
        let cands = legalize_discards st in
        (match timed_pick_discard rng st cands ~opponent time_ms with
         | Some st' -> Some st'
         | None -> force_discard_from_discard_phase st)
    | T.EndCheck ->
        apply_endcheck st

type game_result =
  { winner      : int
  ; steps_taken : int
  }

let simulate_game
    ~(rng : Random.State.t)
    ~(p0 : policy)
    ~(p1 : policy)
    (st0 : T.state)
  : game_result
  =
  let max_steps = 50_000 in
  let rec loop steps st =
    if steps <= 0 then
      { winner = st.current; steps_taken = max_steps }
    else
      let cur = st.T.current in
      let pol = if cur = 0 then p0 else p1 in
      match pol rng st with
      | Some st' ->
          let empty0 = List.is_empty st'.players.(0).hand in
          let empty1 = List.is_empty st'.players.(1).hand in
          if empty0 && not empty1 then
            { winner = 0; steps_taken = max_steps - steps + 1 }
          else if empty1 && not empty0 then
            { winner = 1; steps_taken = max_steps - steps + 1 }
          else
            loop (steps - 1) st'
      | None ->
          (match apply_endcheck st with
           | Some st' -> loop (steps - 1) st'
           | None     -> { winner = cur; steps_taken = max_steps - steps })
  in
  loop max_steps st0

let simulate_games
    ~(rng : Random.State.t)
    ~(games : int)
    ~(mk_initial : unit -> T.state)
    ~(p0 : policy)
    ~(p1 : policy)
  : (int * int)
  =
  let wins0 = ref 0 in
  let wins1 = ref 0 in
  for _ = 1 to games do
    let st0 = mk_initial () in
    let r = simulate_game ~rng ~p0 ~p1 st0 in
    if r.winner = 0 then incr wins0 else incr wins1
  done;
  (!wins0, !wins1)