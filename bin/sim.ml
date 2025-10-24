open! Core
module T = Rummy_engine.Types
module E = Rummy_engine.Engine
module S = Rummy_engine.Setup
module A = Rummy_engine.Ai

let init_state () =
  let deck = S.shuffle S.all_cards in
  let hand_size = 7 in
  let rec take k acc d =
    if k = 0 then (List.rev acc, d)
    else match d with
      | [] -> (List.rev acc, [])
      | x :: xs -> take (k - 1) (x :: acc) xs
  in
  let (h1, d1) = take hand_size [] deck in
  let (h2, d2) = take hand_size [] d1 in
  { T.deck = d2
  ; discard = []
  ; melds = []
  ; players = [| { T.id = 0; name = "AI1"; hand = h1 }
               ; { T.id = 1; name = "AI2"; hand = h2 } |]
  ; current = 0
  ; phase = T.Draw
  ; ace_policy = T.Low
  ; scoring = { T.deadwood_threshold = None }
  ; required_to_use = None
  ; scores = Array.create ~len:2 0
  }

let simulate rng ~ai1 ~ai2 n_games =
  let wins = Array.create ~len:2 0 in
  for _i = 1 to n_games do
    let st0 = init_state () in
    let winner = A.simulate_to_end rng ai1 ai2 st0 in
    wins.(winner) <- wins.(winner) + 1
  done;
  Printf.printf "AI1 wins: %d | AI2 wins: %d\n%!" wins.(0) wins.(1)

let () =
  Random.self_init ();
  let rng = Random.State.make_self_init () in
  let ai1 = A.random_ai in
  let ai2 = A.timed_ai ~time_ms:500 ~opponent:A.random_ai in
  simulate rng ~ai1 ~ai2 100