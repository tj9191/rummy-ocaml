open Rummy_engine
open Types
open Engine

(* ===== Helpers: cards, shuffle, deal ===== *)

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

(* Show table melds with indices *)
let print_melds (melds : meld list) =
  let show_meld i m =
    let kind = match m.kind with Set -> "Set" | Run -> "Run" in
    let cards = m.cards |> List.map string_of_card |> String.concat " " in
    Printf.sprintf "%d) %s: [%s]" (i + 1) kind cards
  in
  if melds = [] then
    print_endline "Melds on table: (none)"
  else (
    print_endline "Melds on table:";
    melds |> List.mapi show_meld |> List.iter print_endline
  )

(* Parse space-separated ints (UI is 1-based; convert to 0-based) *)
let parse_indices (s : string) : int list =
  s
  |> String.split_on_char ' '
  |> List.filter (fun t -> t <> "")
  |> List.filter_map int_of_string_opt
  |> List.map (fun k -> k - 1)

(* Build a card list from 0-based indices; fail if any index is bad *)
let cards_by_indices (hand : card list) (idxs : int list) : card list option =
  let rec nth i = function
    | [] -> None
    | x :: xs -> if i = 0 then Some x else nth (i - 1) xs
  in
  let rec gather acc = function
    | [] -> Some (List.rev acc)
    | i :: is ->
        if i < 0 then None
        else
          match nth i hand with
          | None -> None
          | Some c -> gather (c :: acc) is
  in
  gather [] idxs

(* ===== Construct initial state ===== *)

let init_state (num_players : int) ~(vs_computer : bool) =
  let hand_size = 7 in
  (* deal k hands of size hand_size, consuming the deck left-to-right *)
  let rec deal_k_hands k deck acc =
    if k = 0 then (List.rev acc, deck)
    else
      let (h, deck') = deal_n hand_size deck in
      deal_k_hands (k - 1) deck' (h :: acc)
  in
  let deck0 = shuffle all_cards in
  let (hands, deck_after) = deal_k_hands num_players deck0 [] in
  (* Build players 1..n with hands *)
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

(* ===== Display helpers ===== *)

let pp_phase = function
  | Draw -> "Draw"
  | Play -> "Play"
  | Discard -> "Discard"
  | EndCheck -> "EndCheck"

let clear_screen () =
  print_string "\027[2J\027[H";  (* ANSI clear + home *)
  flush stdout

let print_hand (label : string) (cards : card list) =
  let s = cards |> List.map string_of_card |> String.concat " " in
  Printf.printf "%s: [%s]\n" label s

let print_hidden_hand (label : string) (cards : card list) =
  Printf.printf "%s: [%d cards]\n" label (List.length cards)

let print_discard_pile (discard : card list) =
  match discard with
  | [] -> print_endline "Discard pile: (empty)"
  | _ ->
      print_endline "Discard pile (top → bottom):";
      discard
      |> List.mapi (fun i c -> Printf.sprintf "%d) %s" (i + 1) (string_of_card c))
      |> String.concat "  "
      |> print_endline

let show_turn_header (st : state) =
  let p = st.players.(st.current) in
  let discard_top =
    match st.discard with
    | [] -> "∅"
    | c :: _ -> string_of_card c
  in
  Printf.printf "\n-- %s's turn (phase=%s) --  Deck:%d  Discard:%s\n"
    p.name (pp_phase st.phase) (List.length st.deck) discard_top;
  if p.name = "Computer"
  then print_hidden_hand (p.name ^ " (hidden)") p.hand
  else print_hand (p.name ^ "'s hand") p.hand;
  Array.iteri
    (fun i q ->
      if i <> st.current then print_hidden_hand (q.name ^ " (hidden)") q.hand)
    st.players;
  print_discard_pile st.discard;
  flush stdout

let print_numbered_hand (cards : card list) =
  cards
  |> List.mapi (fun i c -> Printf.sprintf "%d) %s" (i + 1) (string_of_card c))
  |> String.concat "  "
  |> print_endline

let nth_card i cards =
  let rec go k = function
    | [] -> None
    | x :: xs -> if k = 0 then Some x else go (k - 1) xs
  in
  if i < 0 then None else go i cards

(* ===== Round scoring ===== *)

let score_round (st : state) : state =
  let num_players = Array.length st.players in
  let scores = Array.copy st.scores in
  let current = st.current in
  let winner = st.players.(current) in

  print_endline "\n=== ROUND OVER ===";
  Printf.printf "%s went out!\n" winner.name;

  (* simple deadwood value *)
  let hand_value hand =
    List.fold_left
      (fun acc c ->
        acc
        +
        match c.rank with
        | Ace -> 15
        | King | Queen | Jack -> 10
        | Two -> 2 | Three -> 3 | Four -> 4 | Five -> 5
        | Six -> 6 | Seven -> 7 | Eight -> 8 | Nine -> 9 | Ten -> 10)
      0 hand
  in

  let total_gain = ref 0 in
  for i = 0 to num_players - 1 do
    let p = st.players.(i) in
    let v = hand_value p.hand in
    if i = current then
      Printf.printf "%s: %d points (winner)\n" p.name v
    else (
      Printf.printf "%s: %d deadwood points subtracted\n" p.name v;
      total_gain := !total_gain + v;
      scores.(i) <- max 0 (scores.(i) - v);
    )
  done;

  scores.(current) <- scores.(current) + !total_gain;

  print_endline "\n--- TOTAL SCORES ---";
  Array.iteri (fun i s -> Printf.printf "%s: %d\n" st.players.(i).name s) scores;

  { st with scores }

(* ===== AI (trivial) ===== *)

let rec ai_turn (st : state) : state =
  let p = st.players.(st.current) in
  match st.phase with
  | Draw ->
      (match Engine.draw ~source:FromDeck st with
       | Ok st' -> ai_turn st'
       | End_round st' -> st'
       | Error _ -> st)
  | Play ->
      let hand = p.hand in
      let try_meld maker =
        let rec find_combo = function
          | [] | [_] | [_; _] -> None
          | a :: b :: c :: rest ->
              let combo = [ a; b; c ] in
              if maker combo then Some combo else find_combo (b :: c :: rest)
        in
        find_combo hand
      in
      (match try_meld (Rules.is_set) with
       | Some cs ->
           (match Engine.play ~action:(Make_set cs) st with
            | Ok st' -> ai_turn st'
            | _ -> ai_turn { st with phase = Discard })
       | None ->
           (match try_meld (Rules.is_run ~ace_policy:st.ace_policy) with
            | Some cs ->
                (match Engine.play ~action:(Make_run cs) st with
                 | Ok st' -> ai_turn st'
                 | _ -> ai_turn { st with phase = Discard })
            | None -> ai_turn { st with phase = Discard }))
  | Discard ->
      (match p.hand with
       | [] -> st
       | c :: _ ->
           (match Engine.discard ~action:(Discard_card c) st with
            | Ok st' | End_round st' -> st'
            | Error _ -> st))
  | EndCheck ->
      (match Engine.endcheck st with
       | Ok st' | End_round st' -> st'
       | Error _ -> st)
       
let handle_round_end (st' : state) : state =
  let st_scored = score_round st' in
  let maybe_winner =
    Array.mapi (fun i score -> (i, score)) st_scored.scores
    |> Array.to_list
    |> List.find_opt (fun (_, s) -> s >= 500)
  in
  match maybe_winner with
  | Some (i, _) ->
      Printf.printf "\n🏆 %s wins the game! 🏆\n" st_scored.players.(i).name;
      flush stdout;
      exit 0
  | None ->
      print_endline "\nNext round will begin.";
      (* Re-deal a fresh deck; keep scores and players *)
      let deck0 = shuffle all_cards in
      let hand_size = 7 in
      let rec deal_k_hands k deck acc =
        if k = 0 then (List.rev acc, deck)
        else
          let (h, deck') = deal_n hand_size deck in
          deal_k_hands (k - 1) deck' (h :: acc)
      in
      let n = Array.length st_scored.players in
      let (hands, deck_after) = deal_k_hands n deck0 [] in
      let players' =
        Array.mapi (fun i p -> { p with hand = List.nth hands i }) st_scored.players
      in
      {
        st_scored with
        players = players';
        deck = deck_after;
        discard = [];
        melds = [];
        required_to_use = None;
        phase = Draw;
      }

(* ===== Game loop ===== *)

let rec loop st =
  clear_screen ();
  show_turn_header st;
  let p = st.players.(st.current) in

  (* If it's the AI's turn, let it play the whole turn. *)
  if p.name = "Computer" then (
    print_endline "\nComputer is thinking...";
    let st' = ai_turn st in
    (* After AI move, if the engine ended the round, handle it here *)
    (match st'.phase with
     | EndCheck ->
         (* loop continues; endcheck will advance *)
         loop st'
     | _ ->
         (* It might have signaled End_round through the step results processed in ai_turn;
            we still need to re-run the main loop on the resulting state. *)
         loop st')
  ) else

  match st.phase with
  | Draw ->
      let discard_top = match st.discard with [] -> None | c :: _ -> Some c in
      Printf.printf "draw> (d) deck, (x or xN) discard%s, (q) quit\n"
        (match discard_top with
         | None -> " [empty]"
         | Some c -> " [" ^ string_of_card c ^ "]");
      flush stdout;

      begin
        match String.lowercase_ascii (read_line ()) with
        | "q" -> exit 0
        | "d" ->
            (match Engine.draw ~source:FromDeck st with
             | Ok st' ->
                 let p' = st'.players.(st'.current) in
                 (match p'.hand with
                  | c :: _ -> Printf.printf "You drew %s from deck\n" (string_of_card c)
                  | [] -> ());
                 loop st'
             | End_round st' -> loop (handle_round_end st')
             | Error _ -> st)
        | s when String.length s >= 1 && s.[0] = 'x' ->
            (* x or xN (e.g., x3) *)
            let n_opt =
              if String.length s = 1 then Some 1
              else int_of_string_opt (String.sub s 1 (String.length s - 1))
            in
            (match n_opt with
             | None ->
                 print_endline "Usage: x or xN (e.g., x3)"; flush stdout; loop st
             | Some n ->
                 (match Engine.draw ~source:(FromDiscardN n) st with
                  | Ok st' ->
                      Printf.printf "You picked up %d card(s) from discard\n" n;
                      loop st'
                  | End_round st' -> loop (handle_round_end st')
                  | Error e ->
                      (match e with
                       | Empty_discard -> print_endline "Discard is empty."
                       | Illegal_play msg -> Printf.printf "Cannot pick up: %s\n" msg
                       | _ -> ());
                      flush stdout;
                      loop st))
        | _ ->
            (* default to deck for any other key *)
            (match Engine.draw ~source:FromDeck st with
             | Ok st' ->
                 let p' = st'.players.(st'.current) in
                 (match p'.hand with
                  | c :: _ -> Printf.printf "You drew %s from deck\n" (string_of_card c)
                  | [] -> ());
                 loop st'
             | End_round st' -> loop (handle_round_end st')
             | Error _ -> st)
      end

  | Play ->
      print_melds st.melds;
      print_endline "play> set i j k..., run i j k..., lay c on m, s(skip), q(quit)";
      flush stdout;
      let line = read_line () |> String.trim in
      if String.lowercase_ascii line = "q" then exit 0;
      let toks =
        line
        |> String.split_on_char ' '
        |> List.filter (fun t -> t <> "")
        |> List.map String.lowercase_ascii
      in
      let do_set ints =
        match cards_by_indices p.hand (parse_indices (String.concat " " ints)) with
        | None ->
            print_endline "Invalid indices."; loop st
        | Some cs ->
            (match Engine.play ~action:(Make_set cs) st with
             | Ok st' -> loop st'
             | End_round st' -> loop (handle_round_end st')
             | Error (Illegal_play msg) ->
                 print_endline ("Illegal set: " ^ msg); loop st
             | Error _ -> print_endline "Error."; loop st)
      and do_run ints =
        match cards_by_indices p.hand (parse_indices (String.concat " " ints)) with
        | None ->
            print_endline "Invalid indices."; loop st
        | Some cs ->
            (match Engine.play ~action:(Make_run cs) st with
             | Ok st' -> loop st'
             | End_round st' -> loop (handle_round_end st')
             | Error (Illegal_play msg) ->
                 print_endline ("Illegal run: " ^ msg); loop st
             | Error _ -> print_endline "Error."; loop st)
      and do_lay parts =
        (* expect: lay <cardIndex> on <meldIndex>  (both 1-based in UI) *)
        match parts with
        | ci :: "on" :: mi :: [] ->
            (match int_of_string_opt ci, int_of_string_opt mi with
             | Some c1, Some m1 ->
                 let ci0 = c1 - 1 and mi0 = m1 - 1 in
                 (match nth_card ci0 p.hand with
                  | None -> print_endline "Bad card index."; loop st
                  | Some c ->
                      (match Engine.play ~action:(Lay_off (c, mi0)) st with
                       | Ok st' -> loop st'
                       | End_round st' -> loop (handle_round_end st')
                       | Error (Illegal_play msg) ->
                           print_endline ("Illegal layoff: " ^ msg); loop st
                       | Error _ -> print_endline "Error."; loop st))
             | _ ->
                 print_endline "Usage: lay <cardIndex> on <meldIndex>"; loop st)
        | _ ->
            print_endline "Usage: lay <cardIndex> on <meldIndex>"; loop st
      in
      begin
        match toks with
        | [] -> loop st
        | "s" :: _ ->
            (match Engine.play ~action:Skip_to_discard st with
             | Ok st' -> loop st'
             | End_round st' -> loop (handle_round_end st')
             | Error _ -> loop st)
        | "set" :: rest ->
            if rest = [] then (print_endline "Give indices, e.g., set 1 3 5"; loop st)
            else do_set rest
        | "run" :: rest ->
            if rest = [] then (print_endline "Give indices, e.g., run 2 3 4"; loop st)
            else do_run rest
        | "lay" :: rest -> do_lay rest
        | _ ->
            print_endline "Commands: set, run, lay, s, q"; loop st
      end

  | Discard ->
      print_endline "Your hand:";
      print_numbered_hand p.hand;
      Printf.printf "discard> choose 1..%d (Enter=1, q=quit): " (List.length p.hand);
      flush stdout;
      let line = String.lowercase_ascii (read_line ()) in
      if line = "q" then exit 0;
      let idx =
        match String.trim line with
        | "" -> 0
        | s ->
            (match int_of_string_opt s with
             | Some k when 1 <= k && k <= List.length p.hand -> k - 1
             | _ -> 0)
      in
      (match nth_card idx p.hand with
       | None -> loop st
       | Some c ->
           (match Engine.discard ~action:(Discard_card c) st with
            | Ok st' -> loop st'
            | End_round st' -> loop (handle_round_end st')
            | Error _ -> loop st))

  | EndCheck ->
      (match Engine.endcheck st with
       | Ok st' -> loop st'
       | End_round st' -> loop (handle_round_end st')
       | Error _ -> st)

(* ===== Small prompt helper ===== *)

let rec read_num_players () =
  print_string "How many players? (2-4): ";
  flush stdout;
  match int_of_string_opt (String.trim (read_line ())) with
  | Some n when n >= 2 && n <= 4 -> n
  | _ ->
      print_endline "Please enter 2, 3, or 4.";
      read_num_players ()

(* ===== Entry point ===== *)

let () =
  print_endline "Welcome to Rummy!";
  print_endline "For pass and play, press 'p'. For playing against the computer, press 'c'.";
  print_string "Choice (p/c): ";
  flush stdout;

  let choice = String.lowercase_ascii (String.trim (read_line ())) in
  if choice = "p" then (
    let num_players = read_num_players () in
    print_endline "=== Rummy: Pass-and-Play Mode ===";
    ignore (loop (init_state num_players ~vs_computer:false));
    print_endline "\n(done)"
  ) else if choice = "c" then (
    print_endline "=== Rummy: Player vs Computer ===";
    let st0 = init_state 2 ~vs_computer:true in
    ignore (loop st0);
    print_endline "\n(done)"
  ) else
    print_endline "Invalid choice. Please restart and choose 'p' or 'c'."