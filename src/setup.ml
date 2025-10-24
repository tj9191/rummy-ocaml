(* src/setup.ml *)
open Types

let all_cards : card list =
  let suits = [Hearts; Spades; Diamonds; Clubs] in
  let ranks =
    [ Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace ]
  in
  List.concat_map (fun s -> List.map (fun r -> { suit = s; rank = r }) ranks) suits

let shuffle (xs : 'a list) : 'a list =
  Random.self_init ();
  List.map (fun x -> (Random.bits (), x)) xs
  |> List.sort (fun (a, _) (b, _) -> compare a b)
  |> List.map snd

let deal_n n deck =
  let rec take k acc d =
    if k = 0 then (List.rev acc, d)
    else
      match d with
      | [] -> (List.rev acc, [])
      | x :: xs -> take (k - 1) (x :: acc) xs
  in
  take n [] deck

let init_state (num_players : int) ~(vs_computer : bool) : state =
  let hand_size = 7 in
  let rec deal_k_hands k deck acc =
    if k = 0 then (List.rev acc, deck)
    else
      let (h, deck') = deal_n hand_size deck in
      deal_k_hands (k - 1) deck' (h :: acc)
  in
  let deck0 = shuffle all_cards in
  let (hands, deck_after) = deal_k_hands num_players deck0 [] in
  let players =
    hands
    |> List.mapi (fun i h ->
           if vs_computer && i = 1 then
             { id = i; name = "Computer"; hand = h }
           else
             { id = i; name = Printf.sprintf "Player %d" (i + 1); hand = h })
    |> Array.of_list
  in
  {
    deck = deck_after;
    discard = [];
    melds = [];
    players;
    current = 0;
    phase = Draw;
    ace_policy = Low;
    scoring = { deadwood_threshold = None };
    required_to_use = None;
    scores = Array.make num_players 0;
  }